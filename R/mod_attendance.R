# mod_attendance.R ─ Noon Conference Attendance (after-the-fact entry)
#
# Promoted from imslu.ind.dash (2026-09) so both ind.dash and
# imslu.resident.digest can compose it. Lives in amiontools (not gmed)
# because it depends on amiontools::mod_conference_calendar_*/
# use_expected_calendar_cached — amiontools already Imports gmed, so gmed
# can't import amiontools back (circular). Only real change from the
# ind.dash original: the one `.rc_save()` call now goes through
# `gmed::rc_save_instance()` with the already-passed rdm_token/redcap_url
# (was ind.dash's local `.rc_save()`, implicitly reading app_config).
#
# Real-time check-in still happens in the separate imslu.at.noon app. This
# module only covers the case where a resident wasn't able to check in live
# (e.g. arrived late) and wants to log it afterward from their own
# dashboard. Writes to the same REDCap `questions` repeating instrument
# at.noon uses. There's no separate "real-time vs after-the-fact" flag —
# q_entry_timestamp (recorded automatically at save time) combined with the
# resident-picked q_date tells that story: same-day submission looks like
# real-time, a timestamp days after q_date means it was added later.
#
# Conference types and rotation lists mirror imslu.at.noon/global.R
# (conference_type_choices / rotation_choices_for_conference) — duplicated
# here intentionally rather than shared via gmed, to keep that app's own
# change scope minimal (it isn't modularized/packaged).
#
# rdm_data() is only refreshed on login (Phase 2/3 load) — it does not
# automatically pick up a row just written via rc_save_instance().
# display_data() below patches in rows saved earlier in the current session
# so the overview stats update immediately without waiting for a full
# reload.
#
# Calendar (Fred, 2026-09-04): weeks-as-rows, 4-color status (green=on-time,
# yellow=late, red=missing, black=not expected), hover tooltip showing the
# actual scheduled activity — amiontools::mod_conference_calendar. That
# module pulls Amion data itself, so mod_attendance_server() also takes
# resident_id-driven Amion access via rdm_token/redcap_url.

#' @importFrom shiny NS tagList tags uiOutput h6 moduleServer reactiveValues reactiveVal reactive req observeEvent renderUI div actionButton dateInput selectInput updateSelectInput updateDateInput
NULL

.att_sluh_rotation_choices <- c(
  "Red" = "1",
  "Green" = "2",
  "White" = "3",
  "Yellow" = "4",
  "Diamond" = "5",
  "Gold" = "6",
  "MICU" = "7",
  "Bronze" = "8",
  "Cardiology" = "9",
  "Bridge / Acute Care" = "10",
  "Consults - SLUH" = "11",
  "Elective / Clinics CSM" = "12"
)

.att_va_rotation_choices <- c(
  "VA A" = "13",
  "VA B" = "14",
  "VA C" = "15",
  "VA D" = "16",
  "VA Clinics or Consults" = "17"
)

.att_conference_type_choices <- c(
  "SLUH Noon Conference"       = "1",
  "VA Noon Conference"         = "2",
  "Afternoon School"           = "3",
  "SLUH Grand Rounds"          = "4",
  "Other/Specialty Conference" = "5"
)

# Conference types that count toward the July-1 attendance percentage.
# Afternoon School is tracked separately as a simple counter, not a rate.
# "5" (Other/Specialty Conference, added 2026-09-04) counts too -- the
# whole point of that choice is a resident attending an alternate
# conference (e.g. Hem/Onc clinic's own conference) still counts as real
# attendance, per Fred's original ask.
.att_percentage_conference_types <- c("1", "2", "4", "5")

# Team/rotation question is hidden entirely for Afternoon School ("3").
.att_rotation_choices_for_conference <- function(conf_code) {
  if (identical(conf_code, "1") || identical(conf_code, "4")) .att_sluh_rotation_choices
  else if (identical(conf_code, "2")) .att_va_rotation_choices
  else c()
}

# Most recent July 1 on or before `today` — same academic-year convention
# used elsewhere in this ecosystem (gmed's level-at-time calculations).
.att_academic_year_start <- function(today) {
  yr <- as.numeric(format(today, "%Y"))
  mo <- as.numeric(format(today, "%m"))
  if (mo >= 7) as.Date(paste0(yr, "-07-01")) else as.Date(paste0(yr - 1, "-07-01"))
}

#' @export
mod_attendance_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$p(
      class = "text-muted mb-3", style = "font-size:0.9rem;",
      "Log attendance you weren't able to submit in real time — e.g. you arrived late. ",
      "Real-time check-in still happens in the ", tags$strong("Noon Conference"),
      " app (see Program Resources)."
    ),
    uiOutput(ns("attendance_overview")),
    uiOutput(ns("add_button_panel")),
    uiOutput(ns("add_form_panel")),
    h6(class = "mt-4 mb-2", style = "color:var(--gmed-primary); font-weight:700; font-size:0.95rem;",
       "Attendance Calendar"),
    tags$p(class = "text-muted mb-2", style = "font-size:0.85rem;",
           "Hover any day for the scheduled activity."),
    mod_conference_calendar_ui(ns("calendar"))
  )
}

#' Noon-conference attendance module server
#'
#' @param id Module id.
#' @param rdm_data reactive() -> list with $all_forms$questions (all
#'   residents' or this resident's rows, depending on load phase).
#' @param resident_id reactive() -> record_id to save under / filter
#'   history by.
#' @param rdm_token,redcap_url REDCap credentials — also threaded to
#'   \code{mod_conference_calendar} (its own Amion dependency).
#' @export
mod_attendance_server <- function(id, rdm_data, resident_id, rdm_token, redcap_url) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ss             <- reactiveValues(save = NULL)
    form_open      <- reactiveVal(FALSE)
    local_new_rows <- reactiveVal(NULL)  # rows saved this session, ahead of the next full reload
    clicked_context <- reactiveVal(NULL)  # detail_text of the calendar day that opened the form, if any

    base_data <- reactive({
      req(resident_id())
      df <- tryCatch(rdm_data()$all_forms$questions, error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df <- df[as.character(df$record_id) == as.character(resident_id()), , drop = FALSE]
      if (nrow(df) == 0) NULL else df
    })

    # base_data() (from rdm_data(), refreshed only on login) patched with
    # anything saved earlier in this session but not yet reflected there.
    display_data <- reactive({
      base  <- base_data()
      extra <- local_new_rows()
      if (is.null(base) && is.null(extra)) return(NULL)
      if (is.null(base))  return(extra)
      if (is.null(extra)) return(base)
      cols <- union(names(base), names(extra))
      for (cn in setdiff(cols, names(base)))  base[[cn]]  <- NA
      for (cn in setdiff(cols, names(extra))) extra[[cn]] <- NA
      rbind(base[cols], extra[cols])
    })

    observeEvent(input$btn_add,    { form_open(TRUE);  ss$save <- NULL; clicked_context(NULL) })
    observeEvent(input$btn_cancel, { form_open(FALSE); ss$save <- NULL; clicked_context(NULL) })

    output$add_button_panel <- renderUI({
      if (form_open()) return(NULL)
      div(class = "mb-3",
        actionButton(ns("btn_add"), "+ Log Attendance", class = "btn btn-sm",
          style = "background:var(--gmed-primary); color:#fff; border:none; padding:4px 14px; font-size:0.85rem;"))
    })

    output$add_form_panel <- renderUI({
      if (!form_open()) return(NULL)

      lbl <- function(text)
        tags$label(text, class = "form-label fw-semibold", style = "font-size:0.92rem; color:var(--gmed-text-primary);")

      div(class = "card border-0 shadow-sm mb-3", style = "border-radius:8px;",
        div(class = "card-header border-0 d-flex align-items-center gap-2",
            style = "background:#f8fafc; border-radius:8px 8px 0 0; padding:12px 18px;",
          tags$i(class = "bi bi-calendar2-plus-fill", style = "color:var(--gmed-primary); font-size:1rem;"),
          tags$span(style = "font-weight:700; color:var(--gmed-primary); font-size:0.95rem;",
                     "Log Attendance After the Fact")),
        div(class = "card-body",
          if (!is.null(clicked_context()))
            div(class = "alert alert-secondary py-2 px-3 mb-3", style = "font-size:0.85rem;",
              tags$i(class = "bi bi-calendar-event me-1"), "Scheduled that day: ", clicked_context()),
          div(class = "mb-3", lbl("Conference date"),
            dateInput(ns("att_date"), label = NULL, value = Sys.Date() - 1, max = Sys.Date(),
                      daysofweekdisabled = c(0, 6), width = "200px")),
          div(class = "mb-3", lbl("Conference"),
            selectInput(ns("att_conference_type"), label = NULL,
              choices = c("-- select --" = "", .att_conference_type_choices),
              selectize = FALSE, width = "100%")),
          div(class = "mb-3", lbl("Team / rotation"),
            selectInput(ns("att_rotation"), label = NULL,
              choices = c("-- select conference first --" = ""), selectize = FALSE, width = "100%")),
          uiOutput(ns("dup_warning")),
          div(class = "d-flex align-items-center gap-2 mt-3",
            actionButton(ns("btn_save"), "Save", class = "btn btn-sm",
              style = "background:var(--gmed-primary); color:#fff; border:none; padding:6px 18px;"),
            actionButton(ns("btn_cancel"), "Cancel",
              class = "btn btn-sm btn-outline-secondary", style = "padding:6px 14px;"),
            uiOutput(ns("save_status")))
        )
      )
    })

    # Rotation choices follow the picked conference type; hidden entirely
    # (empty choices) for Afternoon School.
    observeEvent(input$att_conference_type, {
      req(nzchar(input$att_conference_type))
      choices <- .att_rotation_choices_for_conference(input$att_conference_type)
      updateSelectInput(session, "att_rotation",
        choices = if (length(choices) == 0) c("-- not needed for this conference --" = "") else choices,
        selected = character(0))
    }, ignoreInit = TRUE)

    output$dup_warning <- renderUI({
      req(input$att_date, nzchar(input$att_conference_type))
      df <- display_data()
      if (is.null(df)) return(NULL)
      dup <- df[as.character(df$q_date) == format(input$att_date, "%Y-%m-%d") &
                  as.character(df$q_conference_type) == input$att_conference_type, ]
      if (nrow(dup) > 0) {
        div(class = "alert alert-warning py-2 px-3 mb-0", style = "font-size:0.85rem;",
          tags$i(class = "bi bi-exclamation-triangle-fill me-1"),
          "You already have an attendance record for this date and conference. Saving will add another entry.")
      } else NULL
    })

    output$save_status <- renderUI({
      r <- ss$save
      if (is.null(r)) return(NULL)
      if (isTRUE(r$success))
        tags$span(class = "text-success", style = "font-size:0.82rem;",
          tags$i(class = "bi bi-check-circle-fill me-1"), paste("Saved", r$ts))
      else
        tags$span(class = "text-danger", style = "font-size:0.82rem;",
          tags$i(class = "bi bi-exclamation-triangle-fill me-1"), r$message)
    })

    observeEvent(input$btn_save, {
      req(resident_id())
      d    <- input$att_date
      conf <- input$att_conference_type
      rot  <- input$att_rotation

      if (is.null(d) || length(d) == 0 || is.na(d)) {
        ss$save <- list(success = FALSE, message = "Please select a date."); return()
      }
      if (d > Sys.Date()) {
        ss$save <- list(success = FALSE, message = "Date can't be in the future."); return()
      }
      if (is.null(conf) || !nzchar(conf)) {
        ss$save <- list(success = FALSE, message = "Please select which conference."); return()
      }
      needs_rotation <- length(.att_rotation_choices_for_conference(conf)) > 0
      if (needs_rotation && (is.null(rot) || !nzchar(rot))) {
        ss$save <- list(success = FALSE, message = "Please select your team/rotation."); return()
      }

      fields <- list(
        q_date              = format(d, "%Y-%m-%d"),
        q_conference_type   = conf,
        q_rotation          = if (needs_rotation) rot else "",
        q_answer            = "",
        q_entry_timestamp   = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )

      df <- display_data()
      inst <- if (!is.null(df) && nrow(df) > 0)
        max(suppressWarnings(as.integer(df$redcap_repeat_instance)), na.rm = TRUE) + 1L else 1L

      result <- gmed::rc_save_instance(resident_id(), "questions", inst, fields,
                                       redcap_url = redcap_url, rdm_token = rdm_token)
      ss$save <- result

      if (isTRUE(result$success)) {
        new_row <- as.data.frame(
          c(list(record_id = as.character(resident_id()),
                 redcap_repeat_instrument = "questions",
                 redcap_repeat_instance   = as.character(inst)),
            fields),
          stringsAsFactors = FALSE, check.names = FALSE)
        local_new_rows(rbind(local_new_rows(), new_row))
        form_open(FALSE)
      }
    })

    # ── Overview: July-1 percentage, Afternoon School counter, heatmap ─────
    attendance_stats <- reactive({
      today  <- Sys.Date()
      july1  <- .att_academic_year_start(today)
      all_days <- seq(july1, today, by = "day")
      weekday_seq <- all_days[!weekdays(all_days) %in% c("Saturday", "Sunday")]

      df <- display_data()
      attended_dates    <- as.Date(character(0))
      afternoon_count   <- 0L
      if (!is.null(df) && nrow(df) > 0) {
        d <- df
        d$.date <- suppressWarnings(as.Date(as.character(d$q_date)))
        d <- d[!is.na(d$.date) & d$.date >= july1 & d$.date <= today, , drop = FALSE]
        qual <- d[as.character(d$q_conference_type) %in% .att_percentage_conference_types, , drop = FALSE]
        attended_dates  <- unique(qual$.date)
        afternoon_count <- sum(as.character(d$q_conference_type) == "3", na.rm = TRUE)
      }

      list(
        july1           = july1,
        today           = today,
        n_weekdays      = length(weekday_seq),
        n_attended      = length(attended_dates),
        pct             = if (length(weekday_seq) > 0) round(length(attended_dates) / length(weekday_seq) * 100) else 0,
        attended_dates  = attended_dates,
        afternoon_count = afternoon_count
      )
    })

    output$attendance_overview <- renderUI({
      st <- attendance_stats()
      div(class = "card border-0 shadow-sm mb-3", style = "border-radius:8px;",
        div(class = "card-body",
          div(class = "d-flex flex-wrap gap-4 align-items-end mb-3",
            div(
              div(style = "font-size:0.78rem; color:#6c757d;",
                  paste0("Noon Conference + Grand Rounds — since ", format(st$july1, "%b %d"))),
              div(style = "font-size:1.8rem; font-weight:700; color:var(--gmed-primary); line-height:1.2;",
                paste0(st$pct, "%"),
                tags$span(style = "font-size:0.85rem; font-weight:400; color:#6c757d; margin-left:6px;",
                          paste0("(", st$n_attended, " of ", st$n_weekdays, " weekdays)")))
            ),
            div(
              div(style = "font-size:0.78rem; color:#6c757d;",
                  paste0("Afternoon School — since ", format(st$july1, "%b %d"))),
              div(style = "font-size:1.8rem; font-weight:700; color:var(--gmed-primary); line-height:1.2;",
                  st$afternoon_count)
            )
          )
        )
      )
    })

    # Cache-first for the Amion/RDM side (the "questions" attendance log
    # stays live regardless -- see amiontools' attendance_reconciliation.R
    # header for why). NULL on a cache miss -> the module falls back to
    # its own live fetch unchanged.
    cached_calendar <- use_expected_calendar_cached(
      rdm_token  = rdm_token,
      redcap_url = redcap_url
    )

    calendar <- mod_conference_calendar_server(
      "calendar",
      resident_id = resident_id,
      rdm_token   = rdm_token,
      redcap_url  = redcap_url,
      expected_calendar_r = cached_calendar
    )

    # Click-to-log (Fred, 2026-09-04): clicking a calendar day opens this
    # same form, pre-filled with that date and (if the day was expected at
    # a specific site) the matching conference type. Not-expected days
    # still open the form with no conference preselected -- a resident may
    # genuinely have attended something unusual on an off day.
    observeEvent(calendar$clicked(), {
      info <- calendar$clicked()
      req(info)
      form_open(TRUE)
      ss$save <- NULL
      clicked_context(info$detail_text)
      updateDateInput(session, "att_date", value = info$date)
      conf_code <- switch(info$expected, "SLUH" = "1", "VA" = "2", "")
      updateSelectInput(session, "att_conference_type", selected = conf_code)
    })
  })
}

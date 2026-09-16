# mod_duty_hour_confirm.R ─ Duty Hours: confirm/edit form (Phase 2)
#
# Promoted from imslu.ind.dash (2026-09) so both ind.dash and
# imslu.resident.digest can compose it. Only real change from the ind.dash
# original: `redcap_url`/`rdm_token` are now explicit server params (was
# implicit via ind.dash's `app_config` global) and the save call goes
# through `gmed::rc_save_instance()` instead of ind.dash's local
# `.rc_save()` (mod_self_eval.R keeps its own copy, untouched by this
# promotion — see redcap_write.R in gmed for why).
#
# Shows ONE day's entry form — either the oldest unconfirmed day in the
# queue (default), or whatever date `selected_date` currently holds (set
# by clicking a cell in mod_duty_hour_calendar). Confirm-as-shown or edit +
# add moonlighting/at-home hours + notes, Save & Next. Writes to RDM's
# duty_hour_log repeating instrument.
#
# Shared state (entries_r, amion_blocks_r, refresh, selected_date) is owned
# by the parent orchestrator and passed in directly — both this module and
# mod_duty_hour_calendar need the same underlying data and need to stay in
# sync (a save here should update the calendar's coloring; a calendar click
# should change what this form shows).
#
# Re-editing an already-confirmed day (via calendar click) pre-fills from
# the SAVED entry, not the Amion default — day_for_date() checks entries_r()
# first.

#' @importFrom shiny NS uiOutput moduleServer reactive req renderUI h4 h5 p tagList fluidRow column selectInput tags div numericInput textAreaInput actionButton HTML observeEvent showNotification
NULL

.dh_hhmm_to_colon <- function(x) {
  ifelse(is.na(x) | x == "", "", paste0(substr(x, 1, 2), ":", substr(x, 3, 4)))
}

#' @export
mod_duty_hour_confirm_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("body"))
}

#' Duty-hour confirm/edit form module server
#'
#' @param id Module id.
#' @param resident_id reactive() -> record_id.
#' @param entries_r,amion_blocks_r,refresh,selected_date Shared reactives
#'   owned by the parent orchestrator — see file header.
#' @param redcap_url REDCap API base URL.
#' @param rdm_token REDCap API token (test or prod — caller's choice).
#' @export
mod_duty_hour_confirm_server <- function(id, resident_id, entries_r, amion_blocks_r,
                                         refresh, selected_date, redcap_url, rdm_token) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    queue_dates <- reactive({
      db <- amion_blocks_r()
      if (nrow(db) == 0) return(as.Date(character()))
      covered <- entries_r()$dh_date
      dates <- unique(db$Date[db$Date <= Sys.Date()])
      sort(setdiff(dates, covered))
    })

    day_for_date <- function(target_date) {
      ex <- entries_r()
      match_row <- ex[ex$dh_date == target_date, ]
      if (nrow(match_row) > 0) {
        m <- match_row[1, ]
        return(data.frame(
          Date = target_date, category = m$dh_category, Hours = m$dh_hours,
          start_hhmm = m$dh_start_time, end_hhmm = m$dh_end_time,
          moon = m$dh_moonlighting_hours, home = m$dh_home_hours,
          notes = m$dh_notes, is_saved = TRUE, stringsAsFactors = FALSE
        ))
      }
      db <- amion_blocks_r()
      rows <- db[db$Date == target_date, ]
      if (nrow(rows) == 0) return(NULL)
      data.frame(
        Date = target_date, category = rows$category[1],
        Hours = { h <- rows$Hours[!is.na(rows$Hours)]; if (length(h) == 0) NA_real_ else sum(h) },
        start_hhmm = { s <- rows$block_start[!is.na(rows$block_start)]; if (length(s) == 0) NA_character_ else s[which.min(to_int_time(s))] },
        end_hhmm   = { e <- rows$block_end[!is.na(rows$block_end)];   if (length(e) == 0) NA_character_ else e[which.max(to_int_time(e))] },
        moon = NA_real_, home = NA_real_, notes = NA_character_, is_saved = FALSE,
        stringsAsFactors = FALSE
      )
    }

    current_day <- reactive({
      target <- selected_date()
      if (is.null(target)) {
        qd <- queue_dates()
        if (length(qd) == 0) return(NULL)
        target <- qd[1]
      }
      day_for_date(target)
    })

    output$body <- renderUI({
      day <- current_day()
      n_remaining <- length(queue_dates())
      manual <- !is.null(selected_date())

      if (is.null(day)) {
        return(tagList(
          h5("Duty Hours — Confirm"),
          p(class = "text-success", "✓ All caught up! No pending days to confirm."),
          p(class = "text-muted small", "New days appear here as they pass — check back as your schedule continues.")
        ))
      }

      tagList(
        h4(format(day$Date, "%A, %B %d, %Y"), style = "margin-bottom: 2px;"),
        p(class = "text-muted small",
          if (manual) {
            if (isTRUE(day$is_saved)) "Editing a previously saved entry — changing anything and saving again updates it."
            else "This day isn't saved yet — fill in what actually happened and save it."
          } else {
            sprintf("%d day(s) need confirmation — showing the oldest first. Work through these regularly so your record stays accurate.", n_remaining)
          }),
        if (is.na(day$Hours))
          p(class = "small", style = "color:#b8860b;",
            "⚠ Amion doesn't have a default for this day — there's nothing to confirm, you'll need to enter what actually happened."),
        fluidRow(
          column(6, selectInput(ns("category"), "Category",
                                choices = names(DUTY_HOUR_CATEGORY_UI_CHOICES),
                                selected = if (day$category %in% names(DUTY_HOUR_CATEGORY_UI_CHOICES))
                                             day$category else "Elective")),
          # Native HTML5 time input, not shiny::textInput — Shiny's built-in
          # input bindings only bind input[type="text"/"number"/etc.], NOT
          # type="time" (confirmed against the installed shiny.js before
          # using this), so this wires it manually via onchange ->
          # Shiny.setInputValue(). Its .value is spec-guaranteed "HH:MM"
          # (24h, zero-padded) regardless of the browser's locale/display
          # format, matching .dh_hhmm_to_colon()'s output exactly.
          column(3, tags$div(class = "form-group",
                             tags$label("Start time"),
                             tags$input(type = "time", class = "form-control", id = ns("start_time"),
                                       value = .dh_hhmm_to_colon(day$start_hhmm),
                                       onchange = sprintf("Shiny.setInputValue('%s', this.value)", ns("start_time"))))),
          column(3, tags$div(class = "form-group",
                             tags$label("End time"),
                             tags$input(type = "time", class = "form-control", id = ns("end_time"),
                                       value = .dh_hhmm_to_colon(day$end_hhmm),
                                       onchange = sprintf("Shiny.setInputValue('%s', this.value)", ns("end_time")))))
        ),
        fluidRow(
          column(4, numericInput(ns("hours"), "Total hours (this rotation/category)",
                                 value = if (is.na(day$Hours)) NA else day$Hours, min = 0, max = 24, step = 0.5)),
          column(4, numericInput(ns("moonlighting"), "+ Moonlighting hours",
                                 value = if (is.na(day$moon)) NA else day$moon, min = 0, max = 24, step = 0.5)),
          column(4, numericInput(ns("home_hours"), "+ At-home chart-review hours",
                                 value = if (is.na(day$home)) NA else day$home, min = 0, max = 24, step = 0.5))
        ),
        p(class = "text-muted small", style = "margin-top: -8px;",
          "Leave moonlighting/at-home blank if none — both are optional, and both add on top of your regular hours above, they don't replace them."),
        # The onchange handlers above keep input$start_time/end_time live
        # once edited, but a pre-filled value the resident never touches
        # (the common "confirm as shown" case) would otherwise never reach
        # the R side at all — no change event ever fires for it. This
        # initializes both on every render (re-runs each time current_day()
        # changes, since the whole form is rebuilt).
        tags$script(HTML(sprintf(
          "Shiny.setInputValue('%s', '%s'); Shiny.setInputValue('%s', '%s');",
          ns("start_time"), .dh_hhmm_to_colon(day$start_hhmm),
          ns("end_time"), .dh_hhmm_to_colon(day$end_hhmm)
        ))),
        textAreaInput(ns("notes"), "Notes (optional)", value = if (is.na(day$notes)) "" else day$notes, rows = 2),
        div(class = "d-flex gap-2 mt-2 align-items-center flex-wrap",
          actionButton(ns("save_next"), if (manual) "Save" else "Save & Next", class = "btn btn-primary"),
          if (!is.na(day$Hours))
            actionButton(ns("confirm_as_shown"), "Confirm as shown", class = "btn btn-outline-secondary"),
          if (manual)
            actionButton(ns("back_to_queue"), "Back to queue", class = "btn btn-outline-secondary"),
          tags$span(class = "text-muted small",
            if (!is.na(day$Hours))
              "\"Confirm as shown\" accepts the schedule above exactly as-is — use it when your day matched Amion's plan. Otherwise edit the fields first, then Save."
            else
              "Fill in what actually happened, then Save — there's no default to confirm here.")
        )
      )
    })

    .build_and_save <- function(day, category, start_colon, end_colon, hours_val, moon_val, home_val, notes) {
      entries <- entries_r()
      existing <- entries$redcap_repeat_instance[entries$dh_date == day$Date]
      instance <- if (length(existing) > 0) existing[1]
                  else max(c(0, entries$redcap_repeat_instance), na.rm = TRUE) + 1

      unchanged <- !is.na(day$Hours) &&
        identical(category, day$category) &&
        identical(start_colon, .dh_hhmm_to_colon(day$start_hhmm)) &&
        identical(end_colon, .dh_hhmm_to_colon(day$end_hhmm)) &&
        isTRUE(all.equal(hours_val, day$Hours)) &&
        isTRUE(all.equal(ifelse(is.na(moon_val), 0, moon_val), ifelse(is.na(day$moon), 0, day$moon))) &&
        isTRUE(all.equal(ifelse(is.na(home_val), 0, home_val), ifelse(is.na(day$home), 0, day$home)))
      source_label <- if (unchanged) "Resident confirmed" else "Resident entered"

      fields <- list(
        dh_date = as.character(day$Date),
        dh_category = unname(DUTY_HOUR_CATEGORY_CODES[category]),
        dh_start_time = start_colon,
        dh_end_time = end_colon,
        dh_hours = hours_val,
        dh_moonlighting_hours = moon_val,
        dh_home_hours = home_val,
        dh_source = unname(DUTY_HOUR_SOURCE_CODES[source_label]),
        dh_notes = notes,
        dh_confirmed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      result <- gmed::rc_save_instance(resident_id(), "duty_hour_log", instance, fields,
                                       redcap_url = redcap_url, rdm_token = rdm_token)
      if (isTRUE(result$success)) {
        refresh(refresh() + 1)
        selected_date(NULL)  # always return to queue-oldest view after a save
        showNotification("Saved.", type = "message", duration = 2)
      } else {
        showNotification(paste("Save failed:", result$message), type = "error", duration = NULL)
      }
    }

    observeEvent(input$save_next, {
      day <- current_day()
      req(day)
      .build_and_save(
        day, input$category, input$start_time, input$end_time,
        suppressWarnings(as.numeric(input$hours)),
        suppressWarnings(as.numeric(input$moonlighting)),
        suppressWarnings(as.numeric(input$home_hours)),
        input$notes
      )
    })

    observeEvent(input$confirm_as_shown, {
      day <- current_day()
      req(day)
      req(!is.na(day$Hours))
      .build_and_save(
        day, day$category, .dh_hhmm_to_colon(day$start_hhmm), .dh_hhmm_to_colon(day$end_hhmm),
        day$Hours, day$moon, day$home, day$notes
      )
    })

    observeEvent(input$back_to_queue, {
      selected_date(NULL)
    })
  })
}

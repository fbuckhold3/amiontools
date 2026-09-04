# =============================================================================
# Shiny module: redesigned attendance calendar (Fred, 2026-09-04) — weeks
# as rows, 4-color status (green=attended on time, yellow=attended late,
# red=missing, black=not expected/excused). Replaces the old
# attended/no-entry/upcoming heatmap AND the raw "Your Attendance History"
# table in mod_attendance.R — this calendar is the only day-level view now.
#
# Click-to-log (Fred, 2026-09-04): native `title=` hover tooltips proved
# unreliable in practice (slow/easy to miss) -- every cell is clickable
# instead. Click emits a namespaced Shiny input event (date + expected +
# detail_text, pipe-delimited); the server function returns
# list(clicked = reactive(...)) so the CALLER (mod_attendance.R, which
# owns the log-attendance form) can open and pre-fill it. This module
# deliberately does not own the form itself -- keeps the form's REDCap
# write logic in one place. `title=` is kept alongside as a harmless
# fallback, not the primary interaction anymore.
# =============================================================================

#' @importFrom shiny NS moduleServer reactive req renderUI uiOutput tagList div tags
NULL

.CONF_CAL_COLORS <- c(
  attended_ontime = "#1D9E75",
  attended_late   = "#EF9F27",
  missing         = "#E24B4A",
  not_expected    = "#2C2C2A",
  pending_today   = "#FFFFFF"
)

#' @rdname mod_conference_calendar
#' @export
mod_conference_calendar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("calendar"))
}

#' Attendance-calendar module — UI + server.
#'
#' @param id Module namespace id.
#' @param resident_id Reactive returning the currently-selected RDM
#'   record_id.
#' @param rdm_token RDM REDCap API token (test or prod — caller's choice).
#' @param redcap_url REDCap API URL.
#' @param amion_lo Amion Lo= program token; defaults to AMION_LO_DEFAULT.
#' @param crosswalk_r,amion_r Optional reactives (e.g. from
#'   use_amion_data()) returning pre-fetched crosswalk/Amion data. Ignored
#'   when `expected_calendar_r` is also supplied. NULL (default): fetches
#'   its own data.
#' @param expected_calendar_r Optional reactive (e.g. from
#'   \code{use_expected_calendar_cached()}) returning the cached, expanded
#'   expected-conference calendar — when supplied, skips the live Amion
#'   fetch entirely (questions_log stays live regardless). NULL (default):
#'   computed live from crosswalk_r/amion_r.
#' @return \code{list(clicked = reactive(...))} — the reactive returns
#'   \code{NULL} until a day cell is clicked, then
#'   \code{list(date=, expected=, detail_text=)} for whichever day was
#'   last clicked. The caller owns what happens next (e.g. opening/
#'   pre-filling a log-attendance form) — this module only reports clicks.
#' @name mod_conference_calendar
#' @export
mod_conference_calendar_server <- function(id, resident_id, rdm_token, redcap_url,
                                           amion_lo = AMION_LO_DEFAULT,
                                           crosswalk_r = NULL,
                                           amion_r = NULL,
                                           expected_calendar_r = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    cal_data <- shiny::reactive({
      shiny::req(resident_id())
      build_conference_calendar_data(
        record_id = resident_id(), rdm_token = rdm_token, redcap_url = redcap_url,
        amion_lo = amion_lo,
        crosswalk = if (!is.null(crosswalk_r)) crosswalk_r() else NULL,
        amion     = if (!is.null(amion_r)) amion_r() else NULL,
        expected_calendar = if (!is.null(expected_calendar_r)) expected_calendar_r() else NULL
      )
    })

    output$calendar <- shiny::renderUI({
      cal <- cal_data()
      if (is.null(cal) || nrow(cal) == 0) {
        return(shiny::tags$p(class = "text-muted small", "No Amion schedule data available for this resident."))
      }

      week_starts <- sort(unique(cal$week_start))
      day_labels  <- c("Mon", "Tue", "Wed", "Thu", "Fri")
      click_input_id <- session$ns("day_clicked")

      week_rows <- lapply(week_starts, function(ws) {
        week_cells <- lapply(day_labels, function(d) {
          row <- cal[cal$week_start == ws & cal$weekday_label == d, ]
          if (nrow(row) == 0) {
            cell <- shiny::div(style = "width:20px;height:20px;")
          } else {
            bg <- .CONF_CAL_COLORS[[row$status[1]]]
            border <- if (row$status[1] == "pending_today") "1px dashed #999" else "none"
            tip <- paste0(format(row$Date[1], "%a, %b %d"), " — ", row$detail_text[1])
            # Pipe-delimited payload (date|expected|detail_text) -- parsed
            # back apart in the `clicked` reactive below. detail_text can't
            # itself contain "|" (category names/rotation labels never do).
            payload <- paste(format(row$Date[1], "%Y-%m-%d"), row$expected[1], row$detail_text[1], sep = "|")
            onclick_js <- sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                                  click_input_id, gsub("'", "", payload))
            cell <- shiny::div(
              title = tip, onclick = onclick_js,
              style = paste0("width:20px;height:20px;border-radius:4px;background:", bg,
                             ";border:", border, ";cursor:pointer;")
            )
          }
          shiny::tags$td(style = "padding:2px;", cell)
        })
        shiny::tags$tr(
          shiny::tags$td(style = "font-size:0.72rem;color:#6c757d;padding-right:10px;text-align:right;white-space:nowrap;",
                          paste("Week of", format(ws, "%b %d"))),
          week_cells
        )
      })

      legend <- shiny::div(
        style = "display:flex;flex-wrap:wrap;gap:14px;margin-top:12px;font-size:0.78rem;color:#6c757d;",
        .conf_cal_legend_item(.CONF_CAL_COLORS[["attended_ontime"]], "Attended, logged live"),
        .conf_cal_legend_item(.CONF_CAL_COLORS[["attended_late"]], "Attended, logged later"),
        .conf_cal_legend_item(.CONF_CAL_COLORS[["missing"]], "Expected, missing"),
        .conf_cal_legend_item(.CONF_CAL_COLORS[["not_expected"]], "Not expected (off, elective, etc.)")
      )

      shiny::tagList(
        shiny::div(style = "overflow-x:auto;",
          shiny::tags$table(style = "border-collapse:collapse;", shiny::tags$tbody(week_rows))
        ),
        legend,
        shiny::tags$p(class = "text-muted", style = "font-size:0.78rem;margin-top:6px;",
                       "Click a day to log or review attendance for it.")
      )
    })

    clicked <- shiny::reactive({
      shiny::req(input$day_clicked)
      parts <- strsplit(input$day_clicked, "\\|")[[1]]
      list(date = as.Date(parts[1]), expected = parts[2],
           detail_text = if (length(parts) >= 3) parts[3] else "")
    })

    list(clicked = clicked)
  })
}

.conf_cal_legend_item <- function(color, label) {
  shiny::div(style = "display:flex;align-items:center;gap:5px;",
    shiny::div(style = paste0("width:12px;height:12px;border-radius:3px;background:", color, ";")),
    label
  )
}

# =============================================================================
# Shiny module: redesigned attendance calendar (Fred, 2026-09-04) — weeks
# as rows, color by status (green=attended on time, yellow=attended late,
# red=missing, black=not expected/excused, blue=logged on a not-expected
# day; purple dot = Afternoon School logged, independent of tile color). Replaces the old
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
  attended_unexpected = "#378ADD",
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
#' @param new_entries_r Optional reactive returning attendance rows the
#'   caller saved this session (q_date, q_conference_type,
#'   q_entry_timestamp). They're applied on top of the already-built
#'   calendar, so a save recolors its tile instantly without re-pulling
#'   anything. NULL (default): calendar only reflects what was in REDCap
#'   when it was built.
#' @return \code{list(clicked = reactive(...), data = reactive(...))} —
#'   \code{data} is the calendar table from
#'   \code{build_conference_calendar_data()}. The \code{clicked} reactive returns
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
                                           expected_calendar_r = NULL,
                                           new_entries_r = NULL) {
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

    # Built once per resident (the slow part: everyone's log + Amion);
    # this session's saves are layered on top instantly.
    shown_cal <- shiny::reactive({
      cal <- cal_data()
      if (is.null(new_entries_r)) return(cal)
      new_rows <- new_entries_r()
      if (!is.null(new_rows) && nrow(new_rows) > 0)
        new_rows <- new_rows[as.character(new_rows$record_id) == as.character(resident_id()), , drop = FALSE]
      .conf_cal_apply_entries(cal, new_rows)
    })

    output$calendar <- shiny::renderUI({
      cal <- shown_cal()
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
              style = paste0("position:relative;width:20px;height:20px;border-radius:4px;background:", bg,
                             ";border:", border, ";cursor:pointer;"),
              if (isTRUE(row$afternoon_school[1])) .conf_cal_as_dot(7)
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

      # Each color gets a one-line "what this means" (Fred, 2026-10-20) —
      # residents were reading black as missed. Window times come from
      # .CONF_CHECKIN_WINDOWS so the text can't drift from the rule.
      w <- .CONF_CHECKIN_WINDOWS
      legend <- shiny::div(
        style = "display:grid;grid-template-columns:auto 1fr;gap:6px 10px;align-items:start;margin-top:12px;font-size:0.78rem;color:#6c757d;",
        .conf_cal_legend_item(.CONF_CAL_COLORS[["attended_ontime"]], "Checked in during conference",
          paste0("Logged on the day, during conference time (SLUH ", .conf_cal_window_text(w[["1"]]),
                 ", VA ", .conf_cal_window_text(w[["2"]]), ").")),
        .conf_cal_legend_item(.CONF_CAL_COLORS[["attended_late"]], "Logged after the fact",
          "Attendance logged, but later that day or on a later day."),
        .conf_cal_legend_item(.CONF_CAL_COLORS[["missing"]], "Missed",
          "You were expected at conference and nothing was logged."),
        .conf_cal_legend_item(.CONF_CAL_COLORS[["not_expected"]], "Not expected",
          paste("Your schedule excused you that day: elective, night float, emergency medicine,",
                "time off/holiday, jeopardy, admin, or a VA afternoon clinic.",
                "Also shown when there's no schedule data. Doesn't count against you.")),
        .conf_cal_legend_item(.CONF_CAL_COLORS[["attended_unexpected"]], "Attended on a not-expected day",
          "Your schedule didn't require conference that day, but attendance was logged."),
        shiny::tagList(
          shiny::div(style = "display:flex;align-items:center;gap:5px;white-space:nowrap;",
            shiny::div(style = "position:relative;width:12px;height:12px;border-radius:3px;flex:none;background:#888780;",
                       .conf_cal_as_dot(5)),
            shiny::tags$strong(style = "font-weight:600;", "Afternoon School logged")),
          shiny::div("Tracked separately; doesn't change the day's noon conference color.")
        ),
        .conf_cal_legend_item(.CONF_CAL_COLORS[["pending_today"]], "Today",
          "Expected today, not logged yet.", border = "1px dashed #999")
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

    list(clicked = clicked, data = shown_cal)
  })
}

# Purple dot marking a day with Afternoon School logged (Fred, 2026-10-20) —
# sits on top of the noon-conference tile color rather than replacing it.
.conf_cal_as_dot <- function(size) {
  shiny::div(style = paste0("position:absolute;right:1px;bottom:1px;width:", size, "px;height:", size,
                            "px;border-radius:50%;background:#7F77DD;border:1.5px solid #fff;box-sizing:content-box;"))
}

# One legend row = two grid cells (swatch + label, then description), so
# the descriptions line up in a column.
.conf_cal_legend_item <- function(color, label, description, border = "none") {
  shiny::tagList(
    shiny::div(style = "display:flex;align-items:center;gap:5px;white-space:nowrap;",
      shiny::div(style = paste0("width:12px;height:12px;border-radius:3px;flex:none;background:",
                                color, ";border:", border, ";")),
      shiny::tags$strong(style = "font-weight:600;", label)
    ),
    shiny::div(description)
  )
}

# c(690, 780) -> "11:30-1:00" (12-hour clock, no am/pm: always midday).
.conf_cal_window_text <- function(w) {
  fmt <- function(m) {
    h <- m %/% 60; h12 <- if (h > 12) h - 12 else h
    sprintf("%d:%02d", h12, m %% 60)
  }
  paste0(fmt(w[1]), "\u2013", fmt(w[2]))
}

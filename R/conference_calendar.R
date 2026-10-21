# =============================================================================
# Per-resident day-by-day conference status, for the redesigned attendance
# calendar (Fred, 2026-09-04): weeks-as-rows grid, 4-color status, hover
# tooltip showing the day's actual scheduled activity. Replaces the old
# attended/no-entry/upcoming heatmap in mod_attendance.R.
#
# Status categories (color mapping is the UI's job, not this data layer's):
#   "attended_ontime" — expected, logged on the conference day INSIDE that
#                        conference's check-in window (.CONF_CHECKIN_WINDOWS,
#                        Central time) -> green
#   "attended_late"    — expected, logged but not inside the window: later
#                        that day, or on a later day (after-the-fact) -> yellow
#   "missing"          — expected, nothing logged, and the day is already
#                        over -> red
#   "not_expected"     — not an expected day at all (off, elective, night
#                        float, excused CC-VA-pm, etc.) -> black
#   "attended_unexpected" — not an expected day, but attendance was logged
#                        anyway (Fred, 2026-10-20) -> blue
#
# Afternoon School (q_conference_type "3") is NOT noon conference (Fred,
# 2026-10-20): it never sets a day's status. It's reported separately as
# the `afternoon_school` flag (a dot on the tile in the UI).
#   "pending_today"    — expected, nothing logged YET, but it's still
#                        today -> distinct from "missing" (the day isn't
#                        over) and from a colored past-day count
#
# Unlike build_attendance_reconciliation() (which only returns EXPECTED
# days, for the summary stats), this returns EVERY weekday in the window —
# the calendar needs to render "not expected" days too, not just omit them.
# =============================================================================

#' @importFrom dplyr filter left_join group_by summarise
NULL

# Check-in windows for "logged during conference" (Fred, 2026-10-20), in
# minutes after midnight, America/Chicago, keyed by q_conference_type code.
# Picked from prod check-in times: SLUH clusters 11:45-12:45, VA runs later.
# Codes not listed here (Afternoon School "3") keep the old same-day rule.
.CONF_CHECKIN_WINDOWS <- list(
  "1" = c(11 * 60 + 30, 13 * 60),       # SLUH Noon Conference  11:30-1:00
  "4" = c(11 * 60 + 30, 13 * 60),       # SLUH Grand Rounds     11:30-1:00
  "2" = c(12 * 60,      13 * 60 + 30),  # VA Noon Conference    12:00-1:30
  "5" = c(11 * 60 + 30, 13 * 60 + 30)   # Other/Specialty       11:30-1:30
)

# TRUE when an entry counts as logged live: saved on the conference date
# and, for conference types with a window, inside it (inclusive).
.conf_logged_live <- function(Date, entry_date, entry_time, q_conference_type) {
  same_day <- !is.na(entry_date) & entry_date == Date
  mins <- as.integer(format(entry_time, "%H", tz = "America/Chicago")) * 60L +
          as.integer(format(entry_time, "%M", tz = "America/Chicago"))
  in_window <- mapply(function(code, m) {
    w <- .CONF_CHECKIN_WINDOWS[[as.character(code)]]
    if (is.null(w)) return(TRUE)
    !is.na(m) && m >= w[1] && m <= w[2]
  }, q_conference_type, mins, USE.NAMES = FALSE)
  same_day & as.logical(in_window)
}

#' Build one resident's day-by-day conference status for the attendance
#' calendar.
#'
#' @inheritParams build_rotation_summary
#' @param record_id The RDM record_id to build the calendar for (this
#'   function is per-resident, unlike build_attendance_reconciliation()'s
#'   program-wide detail).
#' @param questions_log Optional pre-fetched log (from
#'   \code{pull_questions_log()}). NULL (default): fetches it.
#' @param expected_calendar Optional pre-built per-day expected-conference
#'   table (record_id, Date, category, expected) — e.g. from
#'   \code{expand_expected_calendar_rle(gmed::load_cached_expected_calendar())}.
#'   When supplied, skips \code{build_daily_detail()}'s live Amion fetch
#'   entirely. NULL (default): computed live from \code{amion}/
#'   \code{crosswalk} as before.
#' @return Tibble, one row per weekday in `[ay_start, min(ay_end, today)]`:
#'   Date, week_start (the Monday of that Date's week — for grouping into
#'   calendar rows), weekday_label ("Mon".."Fri"), expected ("SLUH"/"VA"/
#'   "None"), category (rolled-up rotation category; NA when there's no
#'   schedule data), status (see file header; noon conference only),
#'   afternoon_school (TRUE if Afternoon School was logged that day),
#'   detail_text (human-readable
#'   scheduled-activity string for the hover tooltip, e.g. "Bronze (SLUH
#'   Inpatient)" or "Time Off/Holiday — not expected").
#' @export
build_conference_calendar_data <- function(record_id, rdm_token, redcap_url,
                                            amion_lo = AMION_LO_DEFAULT,
                                            ay_start = current_ay_start(),
                                            ay_end = ay_start,
                                            staff_types = c("R1", "R2", "R3"),
                                            verified_only = TRUE,
                                            crosswalk = NULL,
                                            amion = NULL,
                                            questions_log = NULL,
                                            expected_calendar = NULL) {

  if (is.null(questions_log)) {
    questions_log <- pull_questions_log(rdm_token, redcap_url)
  }
  log_rid <- questions_log[questions_log$record_id == as.character(record_id), ]

  today <- Sys.Date()
  ay_start_date <- as.Date(sprintf("%d-07-01", ay_start))
  window_end <- min(as.Date(sprintf("%d-06-30", ay_end + 1)), today)
  all_days <- seq(ay_start_date, window_end, by = "day")
  all_days <- all_days[!format(all_days, "%u") %in% c("6", "7")]  # weekdays only

  cal <- data.frame(Date = all_days)
  cal$week_start <- cal$Date - (as.integer(format(cal$Date, "%u")) - 1)
  cal$weekday_label <- format(cal$Date, "%a")

  if (!is.null(expected_calendar)) {
    # Cache-fed path: no live Amion fetch.
    ec <- expected_calendar[expected_calendar$record_id == as.character(record_id), ]
    cal <- cal |> dplyr::left_join(ec[, c("Date", "category", "expected")], by = "Date")
    cal$expected[is.na(cal$expected)] <- "None"
  } else {
    dd <- build_daily_detail(
      rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
      ay_start = ay_start, ay_end = ay_end, staff_types = staff_types,
      verified_only = verified_only, crosswalk = crosswalk, amion = amion
    )
    dd <- dd[dd$record_id == as.character(record_id), ]

    cal <- cal |> dplyr::left_join(dd[, c("Date", "Rotation", "Clinic_Sessions")], by = "Date")
    cal$category <- classify_rotation(cal$Rotation)
    cal$expected <- classify_expected_conference(cal$category, cal$Clinic_Sessions)
  }

  as_dates <- unique(log_rid$Date[log_rid$q_conference_type == "3"])
  log_rid <- log_rid[log_rid$q_conference_type != "3", , drop = FALSE]

  # Per date: any noon-conference entry at all, and whether any was logged
  # live (on the day, inside its conference's check-in window).
  if (nrow(log_rid) > 0) {
    log_rid$live <- .conf_logged_live(log_rid$Date, log_rid$entry_date,
                                      log_rid$entry_time, log_rid$q_conference_type)
  } else {
    log_rid$live <- logical(0)
  }
  log_by_day <- log_rid |>
    dplyr::group_by(Date) |>
    dplyr::summarise(logged = TRUE, logged_live = any(live), .groups = "drop")
  cal <- cal |> dplyr::left_join(log_by_day, by = "Date")

  cal$status <- ifelse(
    cal$expected == "None",
    ifelse(!is.na(cal$logged), "attended_unexpected", "not_expected"),
    ifelse(!is.na(cal$logged),
           ifelse(cal$logged_live, "attended_ontime", "attended_late"),
           ifelse(cal$Date == today, "pending_today", "missing"))
  )

  cal$detail_text <- ifelse(
    cal$expected == "None",
    ifelse(is.na(cal$category) | cal$category == "UNMAPPED", "No schedule data", paste0(cal$category, " — not expected")),
    ifelse(is.na(cal$category), "No schedule data", cal$category)
  )
  cal$detail_text <- ifelse(cal$status == "attended_unexpected",
                            paste0(cal$detail_text, " \u00b7 attendance logged"),
                            cal$detail_text)
  cal$afternoon_school <- cal$Date %in% as_dates
  cal$detail_text <- ifelse(cal$afternoon_school,
                            paste0(cal$detail_text, " \u00b7 Afternoon School logged"),
                            cal$detail_text)

  cal[, c("Date", "week_start", "weekday_label", "expected", "category", "status",
          "afternoon_school", "detail_text")]
}

# Patch an already-built calendar (from build_conference_calendar_data())
# with entries saved this session, so a save recolors its tile instantly
# instead of re-pulling every resident's log + the Amion schedule (Fred,
# 2026-10-20). Same rules as the full build: Afternoon School ("3") only
# sets the dot; anything else makes the day attended -- on time if logged
# live, otherwise late, or "attended_unexpected" on a not-expected day.
# `entries`: q_date, q_conference_type, q_entry_timestamp (Central
# wall-clock "YYYY-mm-dd HH:MM:SS"), as mod_attendance saves them.
.conf_cal_apply_entries <- function(cal, entries) {
  if (is.null(cal) || nrow(cal) == 0 || is.null(entries) || nrow(entries) == 0) return(cal)
  for (i in seq_len(nrow(entries))) {
    d <- as.Date(as.character(entries$q_date[i]))
    idx <- which(cal$Date == d)
    if (length(idx) == 0 || is.na(d)) next
    code <- as.character(entries$q_conference_type[i])

    if (code == "3") {
      if (!isTRUE(cal$afternoon_school[idx])) {
        cal$afternoon_school[idx] <- TRUE
        cal$detail_text[idx] <- paste0(cal$detail_text[idx], " \u00b7 Afternoon School logged")
      }
      next
    }

    ts   <- as.POSIXct(as.character(entries$q_entry_timestamp[i]), tz = "America/Chicago",
                       format = "%Y-%m-%d %H:%M:%S")
    live <- .conf_logged_live(d, as.Date(format(ts, "%Y-%m-%d")), ts, code)
    old  <- cal$status[idx]

    if (cal$expected[idx] == "None") {
      if (old != "attended_unexpected") {
        cal$status[idx] <- "attended_unexpected"
        # Insert before any " · Afternoon School logged" suffix, matching the full build's order.
        cal$detail_text[idx] <- sub("( \u00b7 Afternoon School logged)?$", " \u00b7 attendance logged\\1",
                                    cal$detail_text[idx])
      }
    } else if (old != "attended_ontime") {
      cal$status[idx] <- if (isTRUE(live)) "attended_ontime" else "attended_late"
    }
  }
  cal
}

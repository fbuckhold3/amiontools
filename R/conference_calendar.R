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
#'   "None"), status (see file header), detail_text (human-readable
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

  # Per date: any entry at all, and whether any entry was logged live
  # (on the day, inside its conference's check-in window).
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
    cal$expected == "None", "not_expected",
    ifelse(!is.na(cal$logged),
           ifelse(cal$logged_live, "attended_ontime", "attended_late"),
           ifelse(cal$Date == today, "pending_today", "missing"))
  )

  cal$detail_text <- ifelse(
    cal$expected == "None",
    ifelse(is.na(cal$category) | cal$category == "UNMAPPED", "No schedule data", paste0(cal$category, " — not expected")),
    ifelse(is.na(cal$category), "No schedule data", cal$category)
  )

  cal[, c("Date", "week_start", "weekday_label", "expected", "status", "detail_text")]
}

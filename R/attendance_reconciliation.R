# =============================================================================
# Attendance reconciliation: join Amion's per-day rotation/team data against
# the RDM "questions" conference-attendance log, to answer "how many days
# was this resident EXPECTED at a noon conference, and how many did they
# actually log attendance for."
#
# Rule table + general attendance rule: see expected_attendance.R's header.
# Bounded to Date <= today throughout (same reasoning as the off-day fix in
# team_summary.R, 2026-09-04 -- Amion pre-builds the whole year, but a
# future date obviously can't have a real attendance log yet) and to
# weekdays only (noon conference doesn't happen on weekends -- a reasonable
# unstated assumption, not yet explicitly confirmed with Fred).
# =============================================================================

#' @importFrom dplyr filter mutate group_by summarise left_join n
NULL

#' Build the per-resident/per-day attendance reconciliation, plus rolled-up
#' summaries.
#'
#' @inheritParams build_rotation_summary
#' @param questions_log Optional pre-fetched log (from
#'   \code{pull_questions_log()}). NULL (default): fetches it. Deliberately
#'   NOT cached anywhere in this ecosystem (see the caching-architecture
#'   decision, 2026-09-03) — resident self-report, always live.
#' @param expected_calendar Optional pre-built per-day expected-conference
#'   table (record_id, Date, expected) — e.g. from
#'   \code{expand_expected_calendar_rle(gmed::load_cached_expected_calendar())}.
#'   When supplied, skips \code{build_daily_detail()}'s live Amion fetch
#'   entirely for the expected-conference side (still needs \code{crosswalk}
#'   for name/Level, which is cheap — an RDM-only pull, not Amion). NULL
#'   (default): computed live from \code{amion}/\code{crosswalk} as before.
#' @return A list:
#'   \describe{
#'     \item{detail}{One row per (resident, date) where a conference was
#'       expected: record_id, name, Level, Date, expected ("SLUH"/"VA"),
#'       attended (logical), logged_type (raw q_conference_type code(s) if
#'       any, else NA).}
#'     \item{summary_wide}{One row per resident: record_id, name, Level,
#'       Expected_Days, Attended_Days, Attendance_Rate (0-1).}
#'     \item{class_avg_wide}{One row per Level: Level, Avg_Expected_Days,
#'       Avg_Attended_Days, Avg_Attendance_Rate.}
#'   }
#' @export
build_attendance_reconciliation <- function(rdm_token, redcap_url,
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

  today <- Sys.Date()

  if (!is.null(expected_calendar)) {
    # Cache-fed path: no live Amion fetch. Still need crosswalk for
    # name/Level (cheap — RDM-only pull, not the slow part).
    if (is.null(crosswalk)) {
      crosswalk <- get_amion_crosswalk(rdm_token, redcap_url, verified_only = verified_only)
    }
    ec <- expected_calendar
    ec$record_id <- as.character(ec$record_id)
    detail <- ec[ec$expected != "None" & ec$Date <= today, ]
    detail <- merge(detail, crosswalk[, c("record_id", "name", "Level")],
                    by = "record_id", all.x = FALSE)
    detail <- detail[, c("record_id", "name", "Level", "Date", "expected")]
  } else {
    dd <- build_daily_detail(
      rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
      ay_start = ay_start, ay_end = ay_end, staff_types = staff_types,
      verified_only = verified_only, crosswalk = crosswalk, amion = amion
    )

    dd$category <- classify_rotation(dd$Rotation)
    dd$expected <- classify_expected_conference(dd$category, dd$Clinic_Sessions)

    is_weekday <- !format(dd$Date, "%u") %in% c("6", "7")  # ISO: 6=Sat, 7=Sun

    detail <- dd[dd$expected != "None" & dd$Date <= today & is_weekday,
                 c("record_id", "name", "Level", "Date", "expected")]
  }

  # Collapse questions_log to one row per (resident, date) -- concatenate if
  # more than one type was logged the same day (unusual but possible).
  log_by_day <- questions_log |>
    dplyr::group_by(record_id, Date) |>
    dplyr::summarise(logged_type = paste(unique(q_conference_type), collapse = "; "), .groups = "drop")

  detail <- detail |>
    dplyr::left_join(log_by_day, by = c("record_id", "Date"))
  detail$attended <- !is.na(detail$logged_type)

  summary_wide <- detail |>
    dplyr::group_by(record_id, name, Level) |>
    dplyr::summarise(
      Expected_Days    = dplyr::n(),
      Attended_Days    = sum(attended),
      Attendance_Rate  = Attended_Days / Expected_Days,
      .groups = "drop"
    )

  class_avg_wide <- summary_wide |>
    dplyr::group_by(Level) |>
    dplyr::summarise(
      Avg_Expected_Days   = mean(Expected_Days),
      Avg_Attended_Days   = mean(Attended_Days),
      Avg_Attendance_Rate = mean(Attendance_Rate),
      .groups = "drop"
    )

  list(detail = detail, summary_wide = summary_wide, class_avg_wide = class_avg_wide)
}

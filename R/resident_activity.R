# =============================================================================
# Resident activity: one table that every reporting surface (weekly digest
# email, ind.dash, the program report) reads from, so they all report the
# same numbers by construction.
#
# build_resident_activity() is a JOIN of what amiontools already computes —
# build_daily_detail() (schedule), classify_expected_conference() (the
# expected-attendance rule table), the questions log (attendance), and
# build_duty_hour_summary() (duty hours, with resident-confirmed/entered
# rows already overlaid). No new scheduling or duty-hour rules live here;
# if a number looks wrong, the fix belongs in the function it came from.
#
# summarise_activity() rolls that table up by week / month / academic year,
# per resident, per Level, or program-wide. Days after `through` (default
# today) are dropped before anything is summed — Amion pre-builds the whole
# year, so an unfiltered "latest week" is late June (the digest bug fixed
# 2026-10-24).
#
# Conventions (Fred, 2026-10-24):
#   - Weeks are Sunday-Saturday everywhere, matching the duty-hour chart.
#   - Duty days are reported two ways: "needs entry" (no Amion default —
#     Elective/Emergency/Other — the resident must log it) and "unconfirmed"
#     (still on the Amion default; the resident hasn't confirmed it).
# =============================================================================

#' @importFrom dplyr filter mutate group_by summarise left_join n
NULL

# Per-day duty status from duty_blocks (after the resident-entry overlay).
# A resident's saved row replaces every block for its date, so "entered"/
# "confirmed" days are homogeneous; otherwise any block still waiting on the
# resident makes the whole day "needs_entry".
.activity_duty_status <- function(duty_blocks) {
  duty_blocks |>
    dplyr::group_by(record_id, Date) |>
    dplyr::summarise(
      duty_status = dplyr::case_when(
        any(source == "resident_entered")      ~ "entered",
        any(source == "resident_confirmed")    ~ "confirmed",
        any(source == "resident_entry_needed") ~ "needs_entry",
        all(source %in% c("vacation", "day_off", "jeopardy")) ~ "off",
        TRUE                                   ~ "amion_default"
      ),
      .groups = "drop"
    )
}

#' Build the per-resident, per-day activity table: schedule, expected and
#' logged conference attendance, and duty hours.
#'
#' The shared data layer behind the weekly digest, ind.dash, and the program
#' report — roll it up with \code{summarise_activity()}. Every column comes
#' from an existing amiontools function (see the file header); totals match
#' \code{build_attendance_reconciliation()} and
#' \code{build_duty_hour_summary()} exactly.
#'
#' Fetches nothing it's handed: pass \code{crosswalk}, \code{amion},
#' \code{questions_log}, and \code{duty_entries} to reuse data a batch job
#' already pulled. A full live build is ~10-20 s (dominated by the Amion
#' fetch) and ~30k rows for one academic year.
#'
#' @inheritParams build_rotation_summary
#' @param questions_log Optional pre-fetched \code{pull_questions_log()}.
#' @param duty_entries Optional pre-fetched \code{pull_duty_hour_log()};
#'   \code{data.frame()} skips the resident-entry overlay (Amion defaults
#'   only).
#' @param record_ids Optional character vector — keep only these residents
#'   (everything is still computed program-wide; this just filters output).
#' @return A tibble, one row per (resident, date) in the academic year(s):
#'   \describe{
#'     \item{record_id, name, Level, Date, week_start, is_future}{Keys.
#'       \code{week_start} is the Sunday starting the week.}
#'     \item{Rotation, category, super_category, Team_Assignment,
#'       Off_Status, Call_Status, Clinic_Sessions}{Schedule, from
#'       \code{build_daily_detail()} / \code{classify_rotation()}.}
#'     \item{conf_expected}{"SLUH", "VA", or "None" (weekends are always
#'       "None").}
#'     \item{conf_logged}{Any noon-conference entry that day (Afternoon
#'       School excluded).}
#'     \item{conf_attended}{Expected AND logged.}
#'     \item{conf_logged_types}{Raw q_conference_type code(s) logged, or NA.}
#'     \item{afternoon_school}{Afternoon School ("3") logged that day.}
#'     \item{duty_status}{"confirmed", "entered", "amion_default",
#'       "needs_entry", "off", or "no_schedule".}
#'     \item{duty_hours, home_hours, is_vacation}{From
#'       \code{build_duty_hour_summary()$daily}; \code{duty_hours} is NA on
#'       a day with nothing countable yet (e.g. needs entry).}
#'   }
#' @export
build_resident_activity <- function(rdm_token,
                                    redcap_url,
                                    amion_lo = AMION_LO_DEFAULT,
                                    ay_start = current_ay_start(),
                                    ay_end = ay_start,
                                    staff_types = c("R1", "R2", "R3"),
                                    verified_only = TRUE,
                                    crosswalk = NULL,
                                    amion = NULL,
                                    questions_log = NULL,
                                    duty_entries = NULL,
                                    record_ids = NULL) {

  if (is.null(crosswalk)) {
    crosswalk <- get_amion_crosswalk(rdm_token, redcap_url, verified_only = verified_only)
  }
  if (is.null(amion)) {
    amion <- fetch_amion_data(urls = build_amion_urls(amion_lo, start_ay = ay_start, end_ay = ay_end))
  }
  if (is.null(questions_log)) {
    questions_log <- pull_questions_log(rdm_token, redcap_url)
  }
  if (is.null(duty_entries)) {
    duty_entries <- pull_duty_hour_log(rdm_token = rdm_token, redcap_url = redcap_url)
  }

  ay_first <- as.Date(sprintf("%d-07-01", ay_start))
  ay_last  <- as.Date(sprintf("%d-06-30", ay_end + 1L))
  today    <- Sys.Date()

  # ── Schedule + expected conference ─────────────────────────────────────
  dd <- build_daily_detail(
    rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
    ay_start = ay_start, ay_end = ay_end, staff_types = staff_types,
    verified_only = verified_only, crosswalk = crosswalk, amion = amion
  )
  dd$category       <- classify_rotation(dd$Rotation)
  dd$super_category <- classify_super_category(dd$category)
  dd$conf_expected  <- classify_expected_conference(dd$category, dd$Clinic_Sessions)
  # Same weekday rule as build_attendance_reconciliation().
  dd$conf_expected[format(dd$Date, "%u") %in% c("6", "7")] <- "None"
  dd$name <- NULL
  dd$Level <- NULL

  # ── Duty hours ─────────────────────────────────────────────────────────
  duty <- build_duty_hour_summary(
    rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
    ay_start = ay_start, ay_end = ay_end, staff_types = staff_types,
    verified_only = verified_only, crosswalk = crosswalk, amion = amion,
    entries = duty_entries
  )
  duty_status <- .activity_duty_status(duty$duty_blocks)
  duty_daily  <- duty$daily[, c("record_id", "Date", "Total_Hours", "Home_Hours", "is_vacation")]
  names(duty_daily)[3:4] <- c("duty_hours", "home_hours")

  residents <- unique(c(dd$record_id, duty_status$record_id))

  # ── Attendance log ─────────────────────────────────────────────────────
  ql <- questions_log
  ql$record_id <- as.character(ql$record_id)
  ql <- ql[ql$record_id %in% residents & ql$Date >= ay_first & ql$Date <= ay_last, , drop = FALSE]
  noon_by_day <- ql[ql$q_conference_type != "3", , drop = FALSE] |>
    dplyr::group_by(record_id, Date) |>
    dplyr::summarise(conf_logged_types = paste(unique(q_conference_type), collapse = "; "),
                     .groups = "drop")
  as_by_day <- ql[ql$q_conference_type == "3", , drop = FALSE] |>
    dplyr::distinct(record_id, Date) |>
    dplyr::mutate(afternoon_school = TRUE)

  # ── Assemble: every (resident, date) any source knows about ───────────
  keys <- dplyr::bind_rows(
    dd[, c("record_id", "Date")],
    duty_status[, c("record_id", "Date")],
    noon_by_day[, c("record_id", "Date")],
    as_by_day[, c("record_id", "Date")]
  ) |>
    dplyr::distinct() |>
    dplyr::filter(Date >= ay_first, Date <= ay_last)

  people <- crosswalk[, c("record_id", "name", "Level")]
  people$record_id <- as.character(people$record_id)
  people <- dplyr::distinct(people, record_id, .keep_all = TRUE)

  out <- keys |>
    dplyr::inner_join(people, by = "record_id") |>
    dplyr::left_join(dd, by = c("record_id", "Date")) |>
    dplyr::left_join(noon_by_day, by = c("record_id", "Date")) |>
    dplyr::left_join(as_by_day, by = c("record_id", "Date")) |>
    dplyr::left_join(duty_status, by = c("record_id", "Date")) |>
    dplyr::left_join(duty_daily, by = c("record_id", "Date")) |>
    dplyr::mutate(
      week_start       = as.Date(lubridate::floor_date(Date, "week", week_start = 7)),
      is_future        = Date > today,
      conf_expected    = dplyr::coalesce(conf_expected, "None"),
      conf_logged      = !is.na(conf_logged_types),
      conf_attended    = conf_logged & conf_expected != "None",
      afternoon_school = dplyr::coalesce(afternoon_school, FALSE),
      duty_status      = dplyr::coalesce(duty_status, "no_schedule"),
      is_vacation      = dplyr::coalesce(is_vacation, FALSE)
    ) |>
    dplyr::select(
      record_id, name, Level, Date, week_start, is_future,
      Rotation, category, super_category, Team_Assignment, Off_Status,
      Call_Status, Clinic_Sessions,
      conf_expected, conf_logged, conf_attended, conf_logged_types, afternoon_school,
      duty_status, duty_hours, home_hours, is_vacation
    ) |>
    dplyr::arrange(record_id, Date)

  if (!is.null(record_ids)) {
    out <- out[out$record_id %in% as.character(record_ids), , drop = FALSE]
  }
  out
}

#' Roll up \code{build_resident_activity()} by period, per resident, per
#' Level, or program-wide.
#'
#' @param activity A \code{build_resident_activity()} result.
#' @param period "week" (Sunday-Saturday), "month", or "ay" (July-June).
#' @param by "resident", "level", or "program".
#' @param through Last date to include (default today). Future days are
#'   always pre-scheduled in Amion, so leaving this at today is what keeps
#'   "this week" from meaning late June. Pass \code{NULL} to keep every day
#'   (e.g. anticipated hours).
#' @return A tibble with one row per group and period (\code{period_start}).
#'   Per resident: \code{days}, \code{conf_expected_days},
#'   \code{conf_attended_days}, \code{conf_rate} (NA when nothing was
#'   expected), \code{conf_logged_unexpected_days} (logged on a day
#'   conference wasn't expected), \code{afternoon_school_days},
#'   \code{duty_hours}, \code{home_hours}, \code{days_worked},
#'   \code{weeks}, \code{avg_weekly_hours}, \code{days_confirmed},
#'   \code{days_unconfirmed} (still on the Amion default),
#'   \code{days_needs_entry}; for \code{period = "week"} also
#'   \code{rolling_4wk_avg_hours} and \code{flag_80h} (same 4-week window as
#'   \code{build_duty_hour_summary()}). For \code{by = "level"} /
#'   \code{"program"}: \code{n_residents}, the summed counts,
#'   \code{conf_rate} pooled across residents, \code{avg_resident_conf_rate}
#'   (mean of per-resident rates, as in
#'   \code{build_attendance_reconciliation()$class_avg_wide}),
#'   \code{avg_weekly_hours} (mean across residents), and for weeks
#'   \code{n_flag_80h}.
#' @export
summarise_activity <- function(activity,
                               period = c("week", "month", "ay"),
                               by = c("resident", "level", "program"),
                               through = Sys.Date()) {
  period <- match.arg(period)
  by     <- match.arg(by)

  a <- activity
  if (!is.null(through)) a <- a[a$Date <= as.Date(through), , drop = FALSE]

  a$period_start <- switch(period,
    week  = a$week_start,
    month = as.Date(lubridate::floor_date(a$Date, "month")),
    ay    = as.Date(sprintf("%d-07-01",
                            ifelse(as.integer(format(a$Date, "%m")) >= 7,
                                   as.integer(format(a$Date, "%Y")),
                                   as.integer(format(a$Date, "%Y")) - 1L)))
  )

  res <- a |>
    dplyr::group_by(record_id, name, Level, period_start) |>
    dplyr::summarise(
      days                        = dplyr::n(),
      conf_expected_days          = sum(conf_expected != "None"),
      conf_attended_days          = sum(conf_attended),
      conf_logged_unexpected_days = sum(conf_logged & conf_expected == "None"),
      afternoon_school_days       = sum(afternoon_school),
      # days_worked and has_duty_data read the per-day duty_hours, so they
      # must come before duty_hours is summed (summarise() sees the new
      # scalar from that line on).
      days_worked                 = sum(!is.na(duty_hours) & duty_hours > 0 & !is_vacation),
      has_duty_data               = any(!is.na(duty_hours)),
      duty_hours                  = sum(duty_hours, na.rm = TRUE),
      home_hours                  = sum(home_hours, na.rm = TRUE),
      weeks                       = dplyr::n_distinct(week_start),
      days_confirmed              = sum(duty_status %in% c("confirmed", "entered")),
      days_unconfirmed            = sum(duty_status == "amion_default"),
      days_needs_entry            = sum(duty_status == "needs_entry"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      conf_rate        = ifelse(conf_expected_days > 0, conf_attended_days / conf_expected_days, NA_real_),
      avg_weekly_hours = duty_hours / weeks
    )

  if (period == "week") {
    # Mirrors build_duty_hour_summary(): the 4-week window runs over weeks
    # that have duty data, so a week with nothing countable yet doesn't
    # count as a 0-hour week.
    res <- res |>
      dplyr::arrange(record_id, period_start) |>
      dplyr::group_by(record_id) |>
      dplyr::mutate(rolling_4wk_avg_hours = {
        h <- ifelse(has_duty_data, duty_hours, NA_real_)
        idx <- which(has_duty_data)
        r <- rep(NA_real_, length(h))
        for (k in seq_along(idx)) r[idx[k]] <- mean(h[idx[max(1, k - 3):k]])
        r
      }) |>
      dplyr::ungroup() |>
      dplyr::mutate(flag_80h = !is.na(rolling_4wk_avg_hours) & rolling_4wk_avg_hours > 80)
  }
  res$has_duty_data <- NULL

  if (by == "resident") return(res)

  grp <- if (by == "level") c("Level", "period_start") else "period_start"
  agg <- res |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
    dplyr::summarise(
      n_residents                 = dplyr::n_distinct(record_id),
      conf_expected_days          = sum(conf_expected_days),
      conf_attended_days          = sum(conf_attended_days),
      conf_logged_unexpected_days = sum(conf_logged_unexpected_days),
      afternoon_school_days       = sum(afternoon_school_days),
      avg_resident_conf_rate      = mean(conf_rate, na.rm = TRUE),
      duty_hours                  = sum(duty_hours),
      avg_weekly_hours            = mean(avg_weekly_hours),
      days_confirmed              = sum(days_confirmed),
      days_unconfirmed            = sum(days_unconfirmed),
      days_needs_entry            = sum(days_needs_entry),
      n_flag_80h                  = if ("flag_80h" %in% names(res)) sum(flag_80h) else NA_integer_,
      .groups = "drop"
    ) |>
    dplyr::mutate(conf_rate = ifelse(conf_expected_days > 0, conf_attended_days / conf_expected_days, NA_real_))
  agg$avg_resident_conf_rate[is.nan(agg$avg_resident_conf_rate)] <- NA_real_
  if (period != "week") agg$n_flag_80h <- NULL
  agg
}

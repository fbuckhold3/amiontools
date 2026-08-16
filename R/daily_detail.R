# =============================================================================
# Day-by-day detail log: reconstructs, in table form, what a resident's
# Amion calendar view actually shows per day — rotation, team assignment,
# off/on-call status, and clinic sessions — so the aggregate summaries
# elsewhere in this package can be checked against the real record.
#
# Requested by Fred 2026-08-17 alongside the off-day tracking fix — wanted
# to see rotation detail, not just totals.
# =============================================================================

#' @importFrom dplyr inner_join filter group_by summarise left_join distinct arrange bind_rows select
NULL

#' Build a per-resident, per-date detail log.
#'
#' One row per (resident, date) present in ANY Amion row that date (r/o/c
#' Assignment Types all contribute) — columns are concatenated (";"
#' -separated) when more than one entry of that kind exists the same day,
#' e.g. a resident can have both a team assignment AND an on-call tag the
#' same date.
#'
#' @inheritParams build_rotation_summary
#' @return A tibble with columns: record_id, name, Level, Date, Rotation
#'   (r-type Grouping), Team_Assignment (o-type kind team/nf_coverage),
#'   Off_Status (o-type kind status_off), Call_Status (o-type kind
#'   status_call/status_jeopardy), Clinic_Sessions (c-type Assignment
#'   Name), Other_Assignment (any o-type kind=="UNMAPPED" — should
#'   normally be empty; non-empty means TEAM_ASSIGNMENT_MAP needs a new
#'   entry, surfaced here rather than silently dropped).
#' @export
build_daily_detail <- function(rdm_token,
                               redcap_url,
                               amion_lo = AMION_LO_DEFAULT,
                               ay_start = current_ay_start(),
                               ay_end = ay_start,
                               staff_types = c("R1", "R2", "R3"),
                               verified_only = TRUE,
                               crosswalk = NULL,
                               amion = NULL) {

  if (is.null(crosswalk)) {
    crosswalk <- get_amion_crosswalk(rdm_token, redcap_url, verified_only = verified_only)
  }

  if (is.null(amion)) {
    amion <- fetch_amion_data(urls = build_amion_urls(amion_lo, start_ay = ay_start, end_ay = ay_end))
  }

  matched <- amion |>
    dplyr::filter(`Staff Type` %in% staff_types) |>
    dplyr::inner_join(crosswalk, by = c("Staff ID" = "amion_staff_id"))
  # record_id can come back numeric from REDCapR depending on the caller's
  # crosswalk source; force character so every branch below (including the
  # empty-case fallback in .concat_by_day) agrees for bind_rows().
  matched$record_id <- as.character(matched$record_id)

  .concat_by_day <- function(rows, out_col) {
    if (nrow(rows) == 0) {
      empty <- data.frame(record_id = character(), name = character(),
                          Level = character(), Date = as.Date(character()),
                          value = character(), stringsAsFactors = FALSE)
      names(empty)[5] <- out_col
      return(empty)
    }
    rows |>
      dplyr::group_by(record_id, name, Level, Date) |>
      dplyr::summarise(!!out_col := paste(unique(`Assignment Name`), collapse = "; "), .groups = "drop")
  }

  rotation <- matched |>
    dplyr::filter(`Assignment Type` == "r") |>
    dplyr::group_by(record_id, name, Level, Date) |>
    dplyr::summarise(Rotation = paste(unique(Grouping), collapse = "; "), .groups = "drop")

  o_rows <- matched |> dplyr::filter(`Assignment Type` == "o")
  o_classified <- classify_team_assignment(o_rows$`Assignment Name`)
  o_rows$kind <- o_classified$kind

  team_assignment <- .concat_by_day(o_rows |> dplyr::filter(kind %in% c("team", "nf_coverage")), "Team_Assignment")
  off_status      <- .concat_by_day(o_rows |> dplyr::filter(kind == "status_off"), "Off_Status")
  call_status     <- .concat_by_day(o_rows |> dplyr::filter(kind %in% c("status_call", "status_jeopardy")), "Call_Status")
  other_o         <- .concat_by_day(o_rows |> dplyr::filter(kind == "UNMAPPED"), "Other_Assignment")

  sessions <- matched |>
    dplyr::filter(`Assignment Type` == "c") |>
    dplyr::group_by(record_id, name, Level, Date) |>
    dplyr::summarise(Clinic_Sessions = paste(unique(`Assignment Name`), collapse = "; "), .groups = "drop")

  all_days <- dplyr::bind_rows(
    rotation[, c("record_id", "name", "Level", "Date")],
    team_assignment[, c("record_id", "name", "Level", "Date")],
    off_status[, c("record_id", "name", "Level", "Date")],
    call_status[, c("record_id", "name", "Level", "Date")],
    other_o[, c("record_id", "name", "Level", "Date")],
    sessions[, c("record_id", "name", "Level", "Date")]
  ) |> dplyr::distinct()

  all_days |>
    dplyr::left_join(rotation, by = c("record_id", "name", "Level", "Date")) |>
    dplyr::left_join(team_assignment, by = c("record_id", "name", "Level", "Date")) |>
    dplyr::left_join(off_status, by = c("record_id", "name", "Level", "Date")) |>
    dplyr::left_join(call_status, by = c("record_id", "name", "Level", "Date")) |>
    dplyr::left_join(sessions, by = c("record_id", "name", "Level", "Date")) |>
    dplyr::left_join(other_o, by = c("record_id", "name", "Level", "Date")) |>
    dplyr::arrange(record_id, Date)
}

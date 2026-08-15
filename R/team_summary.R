# =============================================================================
# Team-level assignment summary: fetch Amion -> join RDM crosswalk ->
# restrict to Assignment Type == "o", kind == "team" -> count distinct days
# per resident per team/role/slot, plus program-wide and per-class averages.
#
# Counts DISTINCT CALENDAR DAYS, not hours. o-type row duration is unreliable
# for this population — the same Assignment Name shows up both as a
# zero-duration membership marker and, on other days, as a real timed block
# (confirmed 2026-08-15) — so duration isn't a trustworthy signal for "was
# this resident on this team that day," only presence is. Duration-based
# hour tracking is a separate, later problem (duty-hour work).
#
# Unlike build_rotation_summary()'s single Assignment Type=='r' row per
# resident-date, a resident can hold MULTIPLE concurrent 'o' rows the same
# day (e.g. a team assignment + a status_call tag) - confirmed 2026-08-15,
# ~14% of resident-dates with any 'o' row. That's expected, not deduped:
# each (resident, team, role, slot) combo is counted independently.
# =============================================================================

#' @importFrom dplyr inner_join filter mutate group_by summarise count distinct n_distinct
#' @importFrom tidyr pivot_wider
NULL

#' Build per-resident team-assignment day counts, plus program-wide and
#' per-class averages.
#'
#' @inheritParams build_rotation_summary
#' @return A list with:
#'   - detail: every Assignment Type=='o', kind=='team' row used, with
#'     team/role/slot attached
#'   - team_summary_long: record_id/name/Level/team/role/slot/Days (distinct
#'     calendar days) — full granularity
#'   - team_summary_wide: one row per resident (+ Level), one column per
#'     TEAM (role/slot collapsed — summed) — the "how many days on
#'     Green/Yellow/etc" view
#'   - program_avg_long, program_avg_wide: same shape, averaged across ALL
#'     matched residents (Fred's "program as a whole" ask) rather than
#'     grouped by class
#'   - class_avg_wide: per-Level average per team (role/slot collapsed) —
#'     included alongside the program-wide average since Level is already
#'     on hand from the crosswalk; same current-Level-only caveat as
#'     build_rotation_summary()'s class_avg for multi-year pulls
#'   - unmapped: any Assignment Name values classify_team_assignment()
#'     didn't recognize, across ALL kinds (not just "team") — should be
#'     empty; non-empty means TEAM_ASSIGNMENT_MAP needs a new entry
#' @export
build_team_summary <- function(rdm_token,
                               redcap_url,
                               amion_lo = AMION_LO_DEFAULT,
                               ay_start = current_ay_start(),
                               ay_end = ay_start,
                               staff_types = c("R1", "R2", "R3"),
                               verified_only = TRUE) {

  crosswalk <- get_amion_crosswalk(rdm_token, redcap_url, verified_only = verified_only)

  amion <- fetch_amion_data(urls = build_amion_urls(amion_lo, start_ay = ay_start, end_ay = ay_end))

  o_only <- amion |>
    dplyr::filter(`Staff Type` %in% staff_types, `Assignment Type` == "o") |>
    dplyr::inner_join(crosswalk, by = c("Staff ID" = "amion_staff_id"))

  classified <- classify_team_assignment(o_only$`Assignment Name`)
  o_only$kind <- classified$kind
  o_only$team <- classified$team
  o_only$role <- classified$role
  o_only$slot <- classified$slot

  unmapped <- o_only |>
    dplyr::filter(kind == "UNMAPPED") |>
    dplyr::count(`Assignment Name`, sort = TRUE)

  detail <- o_only |> dplyr::filter(kind == "team")

  team_summary_long <- detail |>
    dplyr::distinct(record_id, name, Level, team, role, slot, Date) |>
    dplyr::group_by(record_id, name, Level, team, role, slot) |>
    dplyr::summarise(Days = dplyr::n_distinct(Date), .groups = "drop")

  # Collapsed-to-team-only view (role/slot summed) for the wide table
  team_days_by_resident <- team_summary_long |>
    dplyr::group_by(record_id, name, Level, team) |>
    dplyr::summarise(Days = sum(Days), .groups = "drop")

  team_summary_wide <- team_days_by_resident |>
    tidyr::pivot_wider(names_from = team, values_from = Days, values_fill = 0)

  program_avg_long <- team_summary_long |>
    dplyr::group_by(team, role, slot) |>
    dplyr::summarise(Avg_Days = mean(Days), .groups = "drop")

  program_avg_wide <- team_days_by_resident |>
    dplyr::group_by(team) |>
    dplyr::summarise(Avg_Days = mean(Days), .groups = "drop") |>
    tidyr::pivot_wider(names_from = team, values_from = Avg_Days, values_fill = 0)

  class_avg_wide <- team_days_by_resident |>
    dplyr::group_by(Level, team) |>
    dplyr::summarise(Avg_Days = mean(Days), .groups = "drop") |>
    tidyr::pivot_wider(names_from = team, values_from = Avg_Days, values_fill = 0)

  list(
    detail             = detail,
    team_summary_long  = team_summary_long,
    team_summary_wide  = team_summary_wide,
    program_avg_long   = program_avg_long,
    program_avg_wide   = program_avg_wide,
    class_avg_wide     = class_avg_wide,
    unmapped           = unmapped
  )
}

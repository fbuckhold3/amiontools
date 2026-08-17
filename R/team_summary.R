# =============================================================================
# Team-level assignment summary: fetch Amion -> join RDM crosswalk ->
# restrict to Assignment Type == "o" -> count distinct days per resident per
# team/role/slot, plus program-wide and per-class averages.
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
#
# On-duty (2026-08-17) covers kind %in% c("team", "nf_coverage") - Night
# Float used to be excluded entirely (tracked as "coverage", not "team"),
# which meant NF off-days had nothing to reconcile against. Now Night
# Float is its own on-duty bucket like any ward team.
#
# Off-duty (2026-08-17) mirrors the on-duty structure for kind ==
# "status_off" rows - Fred wanted off-days during a rotation visible, not
# silently dropped. Collapsed into a single generic "Off" bucket (Fred's
# call, same day, after initially asking for per-rotation attribution) -
# also sidesteps the fact that some off labels don't say which rotation
# they're from ("Intern off", "SLU Res off"), so per-rotation attribution
# would have been incomplete anyway. TEAM_ASSIGNMENT_MAP still carries
# team attribution on the off-status entries where the label specifies one
# (kept for the daily detail log / potential future use) - just not used
# for this aggregation anymore.
#
# Off-duty ALSO includes inferred days (2026-08-17, same day, found while
# investigating why "Off" looked low): on a genuine ward-roster rotation
# (SLUH Inpatient/ICU/VA Inpatient/Night Float - the categories that
# actually have o-type team entries in TEAM_ASSIGNMENT_MAP), a day can have
# an r-type row (rotation continues) but ZERO o-type rows of any kind - not
# even an explicit off label. Confirmed real and substantial: 4,926
# resident-days program-wide, 86 for one spot-checked resident alone vs.
# only 14 documented off-label days for that same person. These are folded
# into the same "Off" total (source="inferred" in detail_off, vs.
# source="documented" for an explicit Amion label) - Fred confirmed the
# magnitude ("good") before this was built. Deliberately NOT extended to
# non-ward categories (Continuity Clinic, Elective, Ambulatory, VACA) -
# those categories don't use Amion's team-roster concept at all, so "no
# o-row" there is normal, not a sign of an uncounted day off (VACA in
# particular is already tracked as its own category elsewhere).
# =============================================================================

.WARD_ROSTER_CATEGORIES <- c("SLUH Inpatient", "ICU", "VA Inpatient", "Night Float")

#' @importFrom dplyr inner_join filter mutate group_by summarise count distinct n_distinct bind_rows anti_join select
#' @importFrom tidyr pivot_wider
NULL

#' Build per-resident team-assignment day counts (on-duty AND off-duty),
#' plus program-wide and per-class averages for each.
#'
#' @inheritParams build_rotation_summary
#' @return A list with:
#'   - detail: every Assignment Type=='o' row used for on-duty
#'     (kind %in% c("team","nf_coverage")), with team/role/slot attached
#'   - detail_off: one row per off day, `source` = "documented" (an
#'     explicit Amion status_off label) or "inferred" (ward-roster
#'     rotation, r-type row present, zero o-type rows that date — see file
#'     header)
#'   - team_summary_long, off_summary_long: record_id/name/Level/team/
#'     role/slot/Days (distinct calendar days) — full granularity
#'   - team_summary_wide: one row per resident (+ Level), one column per
#'     TEAM (role/slot collapsed — summed). off_summary_wide: same shape
#'     but a single "Off" column (all status_off entries collapsed
#'     together, not broken out per rotation — Fred's call 2026-08-17)
#'   - program_avg_long, program_avg_wide: on-duty only, averaged across ALL
#'     matched residents (Fred's "program as a whole" ask) rather than
#'     grouped by class
#'   - class_avg_wide, off_class_avg_wide: per-Level average per team
#'     (role/slot collapsed) for on-duty and off-duty respectively —
#'     same current-Level-only caveat as build_rotation_summary()'s
#'     class_avg for multi-year pulls
#'   - unmapped: any Assignment Name values classify_team_assignment()
#'     didn't recognize, across ALL kinds — should be empty; non-empty
#'     means TEAM_ASSIGNMENT_MAP needs a new entry
#' @export
build_team_summary <- function(rdm_token,
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

  .day_counts_by_team <- function(rows) {
    long <- rows |>
      dplyr::distinct(record_id, name, Level, team, role, slot, Date) |>
      dplyr::group_by(record_id, name, Level, team, role, slot) |>
      dplyr::summarise(Days = dplyr::n_distinct(Date), .groups = "drop")

    by_resident <- long |>
      dplyr::group_by(record_id, name, Level, team) |>
      dplyr::summarise(Days = sum(Days), .groups = "drop")

    wide <- by_resident |>
      tidyr::pivot_wider(names_from = team, values_from = Days, values_fill = 0)

    class_avg_wide <- by_resident |>
      dplyr::group_by(Level, team) |>
      dplyr::summarise(Avg_Days = mean(Days), .groups = "drop") |>
      tidyr::pivot_wider(names_from = team, values_from = Avg_Days, values_fill = 0)

    list(long = long, by_resident = by_resident, wide = wide, class_avg_wide = class_avg_wide)
  }

  detail <- o_only |> dplyr::filter(kind %in% c("team", "nf_coverage"))
  on_duty <- .day_counts_by_team(detail)

  # Collapsed into one generic "Off" bucket (Fred's call 2026-08-17) rather
  # than broken out per rotation — simpler, and sidesteps the label
  # ambiguity noted above (which rotation an "Intern off"/"SLU Res off" day
  # belongs to isn't always recoverable from Amion's data anyway).
  documented_off <- o_only |>
    dplyr::filter(kind == "status_off") |>
    dplyr::mutate(team = "Off", source = "documented") |>
    dplyr::select(record_id, name, Level, Date, team, role, slot, source)

  # Inferred off-days: ward-roster rotation, r-type row present, but zero
  # o-type rows of ANY kind that date (see file header). o_only is already
  # staff_type-filtered + crosswalk-joined, so this anti_join is against
  # the same population documented_off comes from.
  r_ward <- amion |>
    dplyr::filter(`Staff Type` %in% staff_types, `Assignment Type` == "r") |>
    dplyr::inner_join(crosswalk, by = c("Staff ID" = "amion_staff_id")) |>
    dplyr::mutate(category = classify_rotation(Grouping)) |>
    dplyr::filter(category %in% .WARD_ROSTER_CATEGORIES) |>
    dplyr::distinct(record_id, name, Level, Date)

  o_any_dates <- o_only |> dplyr::distinct(record_id, Date)

  inferred_off <- r_ward |>
    dplyr::anti_join(o_any_dates, by = c("record_id", "Date")) |>
    dplyr::mutate(team = "Off", role = NA_character_, slot = NA_character_, source = "inferred")

  detail_off <- dplyr::bind_rows(documented_off, inferred_off)
  off_duty <- .day_counts_by_team(detail_off)

  program_avg_long <- on_duty$long |>
    dplyr::group_by(team, role, slot) |>
    dplyr::summarise(Avg_Days = mean(Days), .groups = "drop")

  program_avg_wide <- on_duty$by_resident |>
    dplyr::group_by(team) |>
    dplyr::summarise(Avg_Days = mean(Days), .groups = "drop") |>
    tidyr::pivot_wider(names_from = team, values_from = Avg_Days, values_fill = 0)

  list(
    detail             = detail,
    detail_off         = detail_off,
    team_summary_long  = on_duty$long,
    team_summary_wide  = on_duty$wide,
    off_summary_long   = off_duty$long,
    off_summary_wide   = off_duty$wide,
    program_avg_long   = program_avg_long,
    program_avg_wide   = program_avg_wide,
    class_avg_wide     = on_duty$class_avg_wide,
    off_class_avg_wide = off_duty$class_avg_wide,
    unmapped           = unmapped
  )
}

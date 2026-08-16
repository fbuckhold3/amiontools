# =============================================================================
# End-to-end rotation-day summary: fetch Amion -> join RDM crosswalk ->
# restrict to Assignment Type == "r" -> classify Grouping -> aggregate.
#
# Formalizes what was validated by hand on 2026-08-14 (scratchpad
# validate_rotation_dedup.R): filtering to Assignment Type == "r" gives at
# most one row per resident-date with no overlap, so no capping/priority
# dedup is needed the way amion-va-report's VA-only report requires.
# =============================================================================

#' @importFrom dplyr inner_join filter mutate group_by summarise count recode
#' @importFrom tidyr pivot_wider
NULL

#' Build per-resident rotation-day counts by top-level category, plus a
#' per-PGY-class average for comparison.
#'
#' @param rdm_token RDM REDCap API token (test or prod — caller's choice).
#' @param redcap_url REDCap API URL.
#' @param amion_lo Amion Lo= program token; defaults to AMION_LO_DEFAULT.
#' @param ay_start,ay_end Academic-year start-year range to pull, e.g.
#'   ay_start = ay_end = 2026 for AY 2026-27 only. Defaults to the current AY.
#' @param staff_types Amion `Staff Type` values to include. Defaults to
#'   R1/R2/R3 (Chiefs are intentionally excluded — see amion_integration
#'   project notes: Chiefs have no active RDM resident_data record to
#'   crosswalk against, confirmed out of scope 2026-08-14).
#' @param verified_only Passed through to get_amion_crosswalk().
#' @return A list with:
#'   - detail: every type=='r' row used, with `category` and `Level` attached
#'   - summary_long: record_id/name/Level/category/Days (per resident)
#'   - summary_wide: one row per resident (+ Level), one column per category
#'   - class_avg_long: Level/category/Avg_Days (averaged across residents in
#'     that Level for the pulled date range)
#'   - class_avg_wide: one row per Level, one column per category
#'   - unmapped: any Grouping values classify_rotation() didn't recognize —
#'     should be empty; a non-empty result means ROTATION_CATEGORY_MAP needs
#'     a new entry (Amion's Grouping vocabulary can grow over time).
#'
#'   NOTE on class_avg when ay_start < ay_end (multi-year pulls, e.g. via
#'   build_rotation_summary_since_change()): class_avg here still groups by
#'   each resident's CURRENT Level, not their Level at the time of each
#'   rotation. That's fine for a single-AY pull but will misrepresent a
#'   multi-year cumulative view once residents in the window have actually
#'   changed PGY level — grouping by cohort (grad_yr) instead is the correct
#'   fix for that case and is intentionally not yet implemented (flagged in
#'   the amion_integration project notes 2026-08-14; revisit once a
#'   multi-year view is actually being built for real, not just plumbed).
#' @export
build_rotation_summary <- function(rdm_token,
                                   redcap_url,
                                   amion_lo = AMION_LO_DEFAULT,
                                   ay_start = current_ay_start(),
                                   ay_end = ay_start,
                                   staff_types = c("R1", "R2", "R3"),
                                   verified_only = TRUE) {

  crosswalk <- get_amion_crosswalk(rdm_token, redcap_url, verified_only = verified_only)

  amion <- fetch_amion_data(urls = build_amion_urls(amion_lo, start_ay = ay_start, end_ay = ay_end))

  r_only <- amion |>
    dplyr::filter(`Staff Type` %in% staff_types, `Assignment Type` == "r") |>
    dplyr::inner_join(crosswalk, by = c("Staff ID" = "amion_staff_id")) |>
    dplyr::mutate(
      category  = classify_rotation(Grouping),
      Day_Value = day_value(`Start Time`, `End Time`)
    )

  # Correction (found + fixed 2026-08-16): r-type marker rows are often
  # zero-duration (Amion just flags "this resident is on this rotation
  # today"; the REAL time detail, when it exists, is in same-day c-type
  # sessions). day_value() defaults an unrecognized/zero-duration row to a
  # full 1.0 day, which is correct for genuine full-day rotations but wrong
  # whenever c-type sessions exist that only cover part of the day (found:
  # 55.5% of Continuity Clinic days with a matching c-session were actually
  # half-days, not full days — 851.5 inflated days across the program this
  # AY). Fix: when same-DATE c-type sessions exist AND their classified
  # category matches this r-row's category, use their summed value instead.
  # Matching on category (not just date) is deliberate — many inpatient
  # rotations (SLU Floors, VA Floors, etc.) also have unrelated same-day
  # c-sessions (e.g. a continuity clinic half-day carved out of an
  # otherwise-inpatient week); those must NOT override the inpatient day's
  # value just because a c-session happens to exist that date.
  c_same_category <- amion |>
    dplyr::filter(`Staff Type` %in% staff_types, `Assignment Type` == "c") |>
    dplyr::inner_join(crosswalk, by = c("Staff ID" = "amion_staff_id")) |>
    dplyr::mutate(
      category  = classify_session(`Assignment Name`),
      c_Day_Value = day_value(`Start Time`, `End Time`)
    ) |>
    dplyr::group_by(record_id, Date, category) |>
    dplyr::summarise(c_Day_Value = sum(c_Day_Value), .groups = "drop")

  r_only <- r_only |>
    dplyr::left_join(c_same_category, by = c("record_id", "Date", "category")) |>
    dplyr::mutate(Day_Value = dplyr::coalesce(c_Day_Value, Day_Value)) |>
    dplyr::select(-c_Day_Value)

  unmapped <- r_only |>
    dplyr::filter(category == "UNMAPPED") |>
    dplyr::count(Grouping, sort = TRUE)

  summary_long <- r_only |>
    dplyr::group_by(record_id, name, Level, category) |>
    dplyr::summarise(Days = sum(Day_Value), .groups = "drop")

  summary_wide <- summary_long |>
    tidyr::pivot_wider(names_from = category, values_from = Days, values_fill = 0)

  class_avg_long <- summary_long |>
    dplyr::group_by(Level, category) |>
    dplyr::summarise(Avg_Days = mean(Days), .groups = "drop")

  class_avg_wide <- class_avg_long |>
    tidyr::pivot_wider(names_from = category, values_from = Avg_Days, values_fill = 0)

  list(
    detail          = r_only,
    summary_long    = summary_long,
    summary_wide    = summary_wide,
    class_avg_long  = class_avg_long,
    class_avg_wide  = class_avg_wide,
    unmapped        = unmapped
  )
}

#' Rotation summary since the schedule change (AMION_SCHEDULE_CHANGE_AY
#' through the current AY) — the "3-year cumulative for categorical
#' residents" entry point. Thin wrapper over build_rotation_summary(): same
#' logic, wider date range, with a floor that can't accidentally reach back
#' into the pre-change schedule.
#'
#' See build_rotation_summary()'s class_avg NOTE — this wrapper inherits the
#' same current-Level-only grouping caveat for multi-year pulls.
#'
#' @inheritParams build_rotation_summary
#' @export
build_rotation_summary_since_change <- function(rdm_token,
                                                redcap_url,
                                                amion_lo = AMION_LO_DEFAULT,
                                                ay_end = current_ay_start(),
                                                staff_types = c("R1", "R2", "R3"),
                                                verified_only = TRUE) {
  build_rotation_summary(
    rdm_token = rdm_token,
    redcap_url = redcap_url,
    amion_lo = amion_lo,
    ay_start = AMION_SCHEDULE_CHANGE_AY,
    ay_end = ay_end,
    staff_types = staff_types,
    verified_only = verified_only
  )
}

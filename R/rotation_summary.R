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

#' Build per-resident rotation-day counts by top-level category.
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
#'   - detail: every type=='r' row used, with `category` attached
#'   - summary_long: record_id/name/category/Days
#'   - summary_wide: one row per resident, one column per category
#'   - unmapped: any Grouping values classify_rotation() didn't recognize —
#'     should be empty; a non-empty result means ROTATION_CATEGORY_MAP needs
#'     a new entry (Amion's Grouping vocabulary can grow over time).
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

  unmapped <- r_only |>
    dplyr::filter(category == "UNMAPPED") |>
    dplyr::count(Grouping, sort = TRUE)

  summary_long <- r_only |>
    dplyr::group_by(record_id, name, category) |>
    dplyr::summarise(Days = sum(Day_Value), .groups = "drop")

  summary_wide <- summary_long |>
    tidyr::pivot_wider(names_from = category, values_from = Days, values_fill = 0)

  list(
    detail        = r_only,
    summary_long  = summary_long,
    summary_wide  = summary_wide,
    unmapped      = unmapped
  )
}

# =============================================================================
# Unified time-allocation summary: rolls up rotation days (r-type, corrected
# per rotation_summary.R) AND educational half-days (c-type, additive) into
# the 7 super-categories (Inpatient/Ambulatory/Continuity Clinic/Educational/
# Emergency/Elective/Other) Fred asked for 2026-08-16.
#
# Educational time is ADDED on top of the underlying rotation day, not
# carved out of it — e.g. a Wednesday afternoon "Educational Half Day"
# during an otherwise SLU Floors week still counts as an Inpatient day AND
# contributes 0.5 to Educational. This matches how protected didactic time
# is typically tracked in GME reporting (its own metric, layered on top),
# not subtracted from the underlying rotation. Flagged as a design choice,
# not an obvious fact — revisit if Fred wants it carved out instead.
#
# All other category overlap (Continuity Clinic, BRIDGE, VA/SLUH Ambulatory,
# SLUH Metabolic) is already correctly resolved inside build_rotation_
# summary()'s same-category c-override — no double-counting there.
# =============================================================================

#' @importFrom dplyr filter mutate group_by summarise bind_rows inner_join
#' @importFrom tidyr pivot_wider
NULL

#' Build per-resident time allocation by super-category (Inpatient/
#' Ambulatory/Continuity Clinic/Educational/Emergency/Elective/Other), plus
#' per-class averages.
#'
#' @inheritParams build_rotation_summary
#' @return A list with:
#'   - detail_rotation, detail_educational: the two underlying row sets
#'     combined into the total
#'   - summary_long: record_id/name/Level/super_category/Days
#'   - summary_wide: one row per resident (+ Level), one column per
#'     super-category
#'   - class_avg_wide: per-Level average per super-category (same
#'     current-Level-only caveat as build_rotation_summary()'s class_avg)
#'   - unmapped: any category values classify_super_category() didn't
#'     recognize — should be empty
#' @export
build_time_allocation_summary <- function(rdm_token,
                                          redcap_url,
                                          amion_lo = AMION_LO_DEFAULT,
                                          ay_start = current_ay_start(),
                                          ay_end = ay_start,
                                          staff_types = c("R1", "R2", "R3"),
                                          verified_only = TRUE) {

  rotation <- build_rotation_summary(
    rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
    ay_start = ay_start, ay_end = ay_end,
    staff_types = staff_types, verified_only = verified_only
  )

  crosswalk <- get_amion_crosswalk(rdm_token, redcap_url, verified_only = verified_only)
  amion <- fetch_amion_data(urls = build_amion_urls(amion_lo, start_ay = ay_start, end_ay = ay_end))

  educational <- amion |>
    dplyr::filter(`Staff Type` %in% staff_types, `Assignment Type` == "c") |>
    dplyr::inner_join(crosswalk, by = c("Staff ID" = "amion_staff_id")) |>
    dplyr::mutate(
      category  = classify_session(`Assignment Name`),
      Day_Value = day_value(`Start Time`, `End Time`)
    ) |>
    dplyr::filter(category == "Educational")

  combined_long <- dplyr::bind_rows(
    rotation$summary_long,
    educational |>
      dplyr::group_by(record_id, name, Level, category) |>
      dplyr::summarise(Days = sum(Day_Value), .groups = "drop")
  ) |>
    dplyr::group_by(record_id, name, Level, category) |>
    dplyr::summarise(Days = sum(Days), .groups = "drop") |>
    dplyr::mutate(super_category = classify_super_category(category))

  unmapped <- combined_long |>
    dplyr::filter(super_category == "UNMAPPED") |>
    dplyr::distinct(category)

  summary_long <- combined_long |>
    dplyr::group_by(record_id, name, Level, super_category) |>
    dplyr::summarise(Days = sum(Days), .groups = "drop")

  summary_wide <- summary_long |>
    tidyr::pivot_wider(names_from = super_category, values_from = Days, values_fill = 0)

  class_avg_wide <- summary_long |>
    dplyr::group_by(Level, super_category) |>
    dplyr::summarise(Avg_Days = mean(Days), .groups = "drop") |>
    tidyr::pivot_wider(names_from = super_category, values_from = Avg_Days, values_fill = 0)

  list(
    detail_rotation    = rotation$detail,
    detail_educational = educational,
    summary_long       = summary_long,
    summary_wide       = summary_wide,
    class_avg_wide     = class_avg_wide,
    unmapped           = unmapped
  )
}

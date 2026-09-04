# =============================================================================
# Per-resident rotation days broken out by training year actually lived
# (Intern / PGY2 / PGY3), not just the current academic year.
#
# Forward-only by design (Fred, 2026-09-04): only academic years from
# AMION_SCHEDULE_CHANGE_AY (2026) onward are included. A current PGY2/PGY3's
# real Intern/PGY2 year predates the schedule rebuild and used an
# incompatible naming structure the classification maps were never
# validated against -- rather than risk silently misclassifying that data
# (exactly the class of bug caught 2026-09-04 in refresh_amion.qmd), a
# training-year not yet covered by post-rebuild data comes back as NA, not
# a guessed number. Fills in naturally as each cohort accumulates real
# post-rebuild years -- this year's interns will have a genuine 3-column
# history by AY2028-29.
# =============================================================================

#' @importFrom REDCapR redcap_read
#' @importFrom gmed calculate_resident_level
#' @importFrom dplyr group_by summarise all_of
#' @importFrom tidyr pivot_longer pivot_wider
NULL

#' Build the Intern/PGY2/PGY3/Total rotation-day breakdown per resident.
#'
#' @param rdm_token RDM REDCap API token (test or prod — caller's choice).
#' @param redcap_url REDCap API URL.
#' @param amion_lo Amion Lo= program token; defaults to AMION_LO_DEFAULT.
#' @param crosswalk Optional pre-fetched crosswalk (from
#'   \code{get_amion_crosswalk()}). NULL (default): fetches it.
#'
#' @return Tibble: record_id, name, Category, Intern, PGY2, PGY3, Total.
#'   Intern/PGY2/PGY3 cells are \code{NA} for a training year not covered by
#'   post-rebuild data (not a guessed 0) — Total sums only the populated
#'   cells.
#' @export
build_rotation_by_training_year <- function(rdm_token, redcap_url,
                                             amion_lo = AMION_LO_DEFAULT,
                                             crosswalk = NULL) {
  if (is.null(crosswalk)) {
    crosswalk <- get_amion_crosswalk(rdm_token, redcap_url, verified_only = TRUE)
  }

  ays <- AMION_SCHEDULE_CHANGE_AY:current_ay_start()

  # Raw type/grad_yr pull, undecorated with a Level column -- needed to
  # recompute "Level as of THIS AY" per year below. crosswalk's own Level
  # column is always "as of today", which is only correct for the current
  # AY's iteration.
  raw <- REDCapR::redcap_read(
    redcap_uri = redcap_url, token = rdm_token,
    fields = c("record_id", "type", "grad_yr")
  )$data

  per_ay <- lapply(ays, function(ay) {
    # Level as of a safe mid-year date (avoids the July 1 rollover boundary).
    ref_date <- as.Date(sprintf("%d-11-01", ay))
    level_at_ay <- gmed::calculate_resident_level(raw, current_date = ref_date)
    level_at_ay <- level_at_ay[, c("record_id", "Level")]

    ay_rot <- build_rotation_summary(
      rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
      ay_start = ay, ay_end = ay, crosswalk = crosswalk
    )$summary_wide

    ay_rot$Level <- NULL  # drop crosswalk's "as of today" Level
    merge(ay_rot, level_at_ay, by = "record_id", all.x = TRUE)
  })

  category_cols <- setdiff(names(per_ay[[1]]), c("record_id", "name", "Level"))

  long <- do.call(rbind, lapply(per_ay, function(df) {
    # Only Intern/PGY2/PGY3 rows are meaningful here -- a resident not yet
    # in the program, or already graduated, that AY contributes nothing
    # (correctly absent, not a spurious "Graduated"/"Unknown" column).
    df <- df[df$Level %in% c("Intern", "PGY2", "PGY3") & !is.na(df$Level), ]
    if (nrow(df) == 0) return(df[0, c("record_id", "name", "Level", category_cols)])
    tidyr::pivot_longer(df, dplyr::all_of(category_cols), names_to = "Category", values_to = "Days")
  }))

  wide <- long |>
    dplyr::group_by(record_id, name, Level, Category) |>
    dplyr::summarise(Days = sum(Days, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Level, values_from = Days, values_fill = NA_real_)

  for (lvl in c("Intern", "PGY2", "PGY3")) {
    if (!lvl %in% names(wide)) wide[[lvl]] <- NA_real_
  }

  wide$Total <- rowSums(wide[, c("Intern", "PGY2", "PGY3")], na.rm = TRUE)
  wide[, c("record_id", "name", "Category", "Intern", "PGY2", "PGY3", "Total")]
}

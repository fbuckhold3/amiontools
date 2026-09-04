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
#' @param current_ay_amion Optional pre-fetched Amion data for the CURRENT
#'   AY only (e.g. the same reactive `use_amion_data()` already fetched for
#'   other modules in the same session) — reused for that one AY's
#'   iteration instead of fetching again. Any other post-rebuild AY in the
#'   loop (none yet, but this will matter starting AY2027-28) still fetches
#'   its own, since a single-year fetch can't cover multiple years. NULL
#'   (default): fetches every AY itself, same as before.
#' @return A list with two tibbles, both Category × Intern/PGY2/PGY3/Total:
#'   \code{by_resident} (record_id, name, Category, Intern, PGY2, PGY3,
#'   Total — one resident's own days) and \code{class_avg} (Category,
#'   Intern, PGY2, PGY3, Total — the average across every resident who held
#'   that Level in that AY). Intern/PGY2/PGY3 cells are \code{NA} for a
#'   training year not covered by post-rebuild data (not a guessed 0).
#' @export
build_rotation_by_training_year <- function(rdm_token, redcap_url,
                                             amion_lo = AMION_LO_DEFAULT,
                                             crosswalk = NULL,
                                             current_ay_amion = NULL) {
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

    # Reuse a pre-fetched current-AY Amion pull if this iteration IS the
    # current AY (avoids a redundant live fetch when a caller already has
    # it, e.g. ind.dash's shared use_amion_data() reactive).
    ay_amion <- if (!is.null(current_ay_amion) && ay == current_ay_start()) current_ay_amion else NULL

    ay_rot <- build_rotation_summary(
      rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
      ay_start = ay, ay_end = ay, crosswalk = crosswalk, amion = ay_amion
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

  by_resident_long <- long |>
    dplyr::group_by(record_id, name, Level, Category) |>
    dplyr::summarise(Days = sum(Days, na.rm = TRUE), .groups = "drop")

  # Class average: mean across every resident who held that Level in that
  # AY, per category -- the same "average across the cohort" concept as
  # every other table's Class Average column, just computed per training
  # year instead of only the current AY.
  class_avg_long <- by_resident_long |>
    dplyr::group_by(Level, Category) |>
    dplyr::summarise(Days = mean(Days, na.rm = TRUE), .groups = "drop")

  .to_wide_with_total <- function(df, id_cols) {
    w <- df |> tidyr::pivot_wider(names_from = Level, values_from = Days, values_fill = NA_real_)
    for (lvl in c("Intern", "PGY2", "PGY3")) {
      if (!lvl %in% names(w)) w[[lvl]] <- NA_real_
    }
    w$Total <- rowSums(w[, c("Intern", "PGY2", "PGY3")], na.rm = TRUE)
    w[, c(id_cols, "Intern", "PGY2", "PGY3", "Total")]
  }

  list(
    by_resident = .to_wide_with_total(by_resident_long, c("record_id", "name", "Category")),
    class_avg   = .to_wide_with_total(class_avg_long, "Category")
  )
}

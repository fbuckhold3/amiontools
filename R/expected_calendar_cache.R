# =============================================================================
# Build + expand the RLE-encoded expected-conference calendar cached via
# gmed::write_expected_calendar_cache()/load_cached_expected_calendar().
#
# Program-wide, every resident, every weekday of the full AY (Amion
# pre-builds the whole year, so "expected" is knowable in advance even for
# future dates -- unlike attendance itself, which can only be known once
# the day has actually happened). RLE by consecutive same-(category,
# expected) runs per resident, since rotation blocks run multi-day/week.
#
# Encodes BOTH category (e.g. "SLUH Inpatient") and expected ("SLUH"/"VA"/
# "None") -- category is what build_conference_calendar_data() needs to
# reconstruct a real detail_text ("Bronze" is more useful than "SLUH
# expected"). Measured live: adding category costs almost nothing (same
# 5,448 runs, 41.5 KB vs 34.1 KB expected-only) since the two are highly
# correlated -- the CC-VA-pm exception is the one case where the same
# category splits into two expected values, and grouping the RLE run by
# BOTH columns together (not just expected) captures that correctly.
# =============================================================================

#' @importFrom dplyr arrange group_by mutate summarise lag first
NULL

#' Build the RLE-encoded expected-conference calendar for every
#' crosswalked resident.
#'
#' @inheritParams build_rotation_summary
#' @return Data frame: record_id (integer), start, end (Date), category
#'   (character, from \code{classify_rotation()}), expected ("SLUH"/"VA"/
#'   "None").
#' @export
build_expected_calendar_rle <- function(rdm_token, redcap_url,
                                         amion_lo = AMION_LO_DEFAULT,
                                         ay_start = current_ay_start(),
                                         ay_end = ay_start,
                                         staff_types = c("R1", "R2", "R3"),
                                         verified_only = TRUE,
                                         crosswalk = NULL,
                                         amion = NULL) {

  dd <- build_daily_detail(
    rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
    ay_start = ay_start, ay_end = ay_end, staff_types = staff_types,
    verified_only = verified_only, crosswalk = crosswalk, amion = amion
  )

  dd$category <- classify_rotation(dd$Rotation)
  dd$expected <- classify_expected_conference(dd$category, dd$Clinic_Sessions)

  is_weekday <- !format(dd$Date, "%u") %in% c("6", "7")
  compact <- dd[is_weekday, c("record_id", "Date", "category", "expected")]
  compact$record_id <- as.integer(compact$record_id)

  compact |>
    dplyr::arrange(record_id, Date) |>
    dplyr::group_by(record_id) |>
    dplyr::mutate(
      new_run = category != dplyr::lag(category, default = dplyr::first(category)) |
                expected != dplyr::lag(expected, default = dplyr::first(expected)) |
                Date != dplyr::lag(Date, default = dplyr::first(Date) - 1) + 1,
      run_id = cumsum(new_run)
    ) |>
    dplyr::group_by(record_id, run_id, category, expected) |>
    dplyr::summarise(start = min(Date), end = max(Date), .groups = "drop") |>
    dplyr::arrange(record_id, start) |>
    dplyr::select(record_id, start, end, category, expected)
}

#' Expand an RLE-encoded expected-conference calendar back into one row
#' per (record_id, Date, category, expected) — the shape
#' \code{build_attendance_reconciliation()}/\code{build_conference_calendar_data()}
#' consume.
#'
#' @param calendar_rle Data frame (record_id, start, end, category,
#'   expected) — from \code{build_expected_calendar_rle()} or
#'   \code{gmed::load_cached_expected_calendar()}.
#' @return Data frame: record_id (character, matching daily_detail's own
#'   type), Date, category, expected.
#' @export
expand_expected_calendar_rle <- function(calendar_rle) {
  if (is.null(calendar_rle) || nrow(calendar_rle) == 0) {
    return(data.frame(record_id = character(), Date = as.Date(character()),
                       category = character(), expected = character(),
                       stringsAsFactors = FALSE))
  }

  rows <- lapply(seq_len(nrow(calendar_rle)), function(i) {
    r <- calendar_rle[i, ]
    dates <- seq(as.Date(r$start), as.Date(r$end), by = "day")
    data.frame(
      record_id = as.character(r$record_id),
      Date      = dates,
      category  = r$category,
      expected  = r$expected,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

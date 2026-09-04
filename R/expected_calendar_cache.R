# =============================================================================
# Build + expand the RLE-encoded expected-conference calendar cached via
# gmed::write_expected_calendar_cache()/load_cached_expected_calendar().
#
# Program-wide, every resident, every weekday of the full AY (Amion
# pre-builds the whole year, so "expected" is knowable in advance even for
# future dates -- unlike attendance itself, which can only be known once
# the day has actually happened). RLE by consecutive same-expected-value
# date runs per resident, since rotation blocks run multi-day/week.
# =============================================================================

#' @importFrom dplyr arrange group_by mutate summarise lag first
NULL

#' Build the RLE-encoded expected-conference calendar for every
#' crosswalked resident.
#'
#' @inheritParams build_rotation_summary
#' @return Data frame: record_id (integer), start, end (Date), expected
#'   ("S"/"V"/"N" — single-char codes, matching
#'   \code{EXPECTED_CONFERENCE_MAP}'s "SLUH"/"VA"/"None" but compact for
#'   the cache payload).
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
  dd$expected_full <- classify_expected_conference(dd$category, dd$Clinic_Sessions)
  dd$expected <- substr(dd$expected_full, 1, 1)  # "SLUH"/"VA"/"None" -> "S"/"V"/"N"

  is_weekday <- !format(dd$Date, "%u") %in% c("6", "7")
  compact <- dd[is_weekday, c("record_id", "Date", "expected")]
  compact$record_id <- as.integer(compact$record_id)

  compact |>
    dplyr::arrange(record_id, Date) |>
    dplyr::group_by(record_id) |>
    dplyr::mutate(
      new_run = expected != dplyr::lag(expected, default = dplyr::first(expected)) |
                Date != dplyr::lag(Date, default = dplyr::first(Date) - 1) + 1,
      run_id = cumsum(new_run)
    ) |>
    dplyr::group_by(record_id, run_id, expected) |>
    dplyr::summarise(start = min(Date), end = max(Date), .groups = "drop") |>
    dplyr::arrange(record_id, start) |>
    dplyr::select(record_id, start, end, expected)
}

#' Expand an RLE-encoded expected-conference calendar back into one row
#' per (record_id, Date, expected) — the shape
#' \code{build_attendance_reconciliation()}/\code{build_conference_calendar_data()}
#' consume.
#'
#' @param calendar_rle Data frame (record_id, start, end, expected) — from
#'   \code{build_expected_calendar_rle()} or
#'   \code{gmed::load_cached_expected_calendar()}.
#' @return Data frame: record_id (character, matching daily_detail's own
#'   type), Date, expected ("SLUH"/"VA"/"None" — expanded back from the
#'   single-char cache code).
#' @export
expand_expected_calendar_rle <- function(calendar_rle) {
  if (is.null(calendar_rle) || nrow(calendar_rle) == 0) {
    return(data.frame(record_id = character(), Date = as.Date(character()),
                       expected = character(), stringsAsFactors = FALSE))
  }

  code_to_full <- c(S = "SLUH", V = "VA", N = "None")

  rows <- lapply(seq_len(nrow(calendar_rle)), function(i) {
    r <- calendar_rle[i, ]
    dates <- seq(as.Date(r$start), as.Date(r$end), by = "day")
    data.frame(
      record_id = as.character(r$record_id),
      Date      = dates,
      expected  = unname(code_to_full[r$expected]),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

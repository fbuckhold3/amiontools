# =============================================================================
# Duty-hour log (RDM prod `duty_hour_log` repeating instrument, added
# 2026-09-12) — read + merge-over-Amion-defaults layer. Phase 2: resident-
# confirmed/entered hours now exist as real REDCap records; this file makes
# them override the Amion-derived defaults from duty_hour_summary.R
# wherever a resident has actually confirmed or entered a date.
#
# Write path deliberately lives in imslu.ind.dash, NOT here — this app's
# `.rc_save()`/`.rc_delete()` (mod_self_eval.R) are the existing generic
# REDCap repeating-instrument writers, and amiontools has no REDCap write
# capability of its own (matches the "keep write logic where it already
# lives" reuse rule; see gmed's orphaned submit_* family as the cautionary
# example of building a write layer nobody ends up using).
#
# At-Home Chart Review hours count toward Total_Hours/the 80h flag, same as
# Moonlighting — confirmed by Fred 2026-09-15 ("doing charts is actual work
# and should count"), correcting this file's original assumption (standard
# ACGME convention excludes take-home reading — Fred's call is that chart
# review specifically is real clinical work, not passive reading, so it
# counts here). Still broken out as its own Home_Hours column (see
# duty_hour_summary.R) for visibility even though it's now folded into
# Total_Hours too.
# =============================================================================

#' @importFrom REDCapR redcap_read
#' @importFrom dplyr filter mutate select bind_rows anti_join group_by summarise
NULL

#' RDM `duty_hour_log` field choice codes -> labels, exactly matching the
#' dictionary written 2026-09-12 (`redcap-dictionary-edit`) — REDCap's API
#' requires the raw numeric code on WRITE for dropdown fields (reads can use
#' raw_or_label="label", writes cannot), so any write path needs this map.
#' Centralized here (not duplicated in imslu.ind.dash) since amiontools
#' already owns the category vocabulary everywhere else in this package.
#' @export
DUTY_HOUR_CATEGORY_CODES <- c(
  "SLUH Inpatient" = "1", "ICU" = "2", "Night Float" = "3", "VA Inpatient" = "4",
  "Addiction Consults" = "5", "SLUH ID" = "6", "ACS" = "7", "BRIDGE" = "8",
  "VA Ambulatory" = "9", "SLUH Ambulatory" = "10", "SLUH Metabolic" = "11",
  "Continuity Clinic" = "12", "Elective" = "13", "VA Emergency" = "14",
  "SLUH Emergency" = "15", "Other/Admin" = "16", "Moonlighting" = "17",
  "At-Home Chart Review" = "18", "Vacation" = "19", "Day off" = "20", "Jeopardy" = "21"
)

#' The subset of DUTY_HOUR_CATEGORY_CODES a resident should actually be
#' offered as the MAIN day-category in an entry form. Moonlighting/At-Home
#' Chart Review are deliberately excluded — those are captured via their
#' own dedicated dh_moonlighting_hours/dh_home_hours fields, layered
#' alongside the main category, not selected as the main category itself
#' (selecting both would double-count — found via a synthetic-data
#' verification test 2026-09-12, not shipped). The two choice codes stay in
#' the REDCap dictionary unused rather than triggering another prod schema
#' write to remove them.
#' @export
DUTY_HOUR_CATEGORY_UI_CHOICES <- DUTY_HOUR_CATEGORY_CODES[
  !names(DUTY_HOUR_CATEGORY_CODES) %in% c("Moonlighting", "At-Home Chart Review")
]

#' @export
DUTY_HOUR_SOURCE_CODES <- c(
  "Amion default" = "1", "Resident confirmed" = "2", "Resident entered" = "3"
)

.DUTY_HOUR_LOG_FIELDS <- c(
  "record_id", "redcap_repeat_instance", "dh_date", "dh_category",
  "dh_start_time", "dh_end_time", "dh_hours", "dh_moonlighting_hours",
  "dh_home_hours", "dh_source", "dh_notes", "dh_confirmed_at"
)

#' Pull the duty_hour_log repeating instrument (RDM prod/test — caller's
#' token choice), labels not raw codes (dh_category/dh_source come back as
#' their human strings, matching the category vocabulary used throughout
#' this package).
#'
#' @param rdm_token RDM REDCap API token (test or prod — caller's choice).
#' @param redcap_url REDCap API URL.
#' @param record_id Optional single record_id to restrict the pull to (e.g.
#'   for a single-resident confirm-flow UI, cheaper than pulling everyone).
#' @return A tibble, one row per saved entry: record_id, redcap_repeat_instance
#'   (integer), dh_date (Date), dh_category, dh_start_time, dh_end_time,
#'   dh_hours/dh_moonlighting_hours/dh_home_hours (numeric), dh_source,
#'   dh_notes, dh_confirmed_at. Empty (0-row, correct columns) if the
#'   resident has no saved entries yet — repeating instruments return a
#'   blank placeholder row with no repeat_instance when empty; filtered out.
#' @export
pull_duty_hour_log <- function(rdm_token, redcap_url, record_id = NULL) {
  args <- list(
    redcap_uri = redcap_url, token = rdm_token,
    fields = .DUTY_HOUR_LOG_FIELDS, forms = "duty_hour_log",
    raw_or_label = "label"
  )
  if (!is.null(record_id)) args$records <- as.character(record_id)

  dat <- do.call(REDCapR::redcap_read, args)$data
  dat <- dat[!is.na(dat$redcap_repeat_instance) & dat$redcap_repeat_instance != "", ]

  if (nrow(dat) == 0) {
    return(data.frame(
      record_id = character(), redcap_repeat_instance = integer(),
      dh_date = as.Date(character()), dh_category = character(),
      dh_start_time = character(), dh_end_time = character(),
      dh_hours = numeric(), dh_moonlighting_hours = numeric(),
      dh_home_hours = numeric(), dh_source = character(),
      dh_notes = character(), dh_confirmed_at = character(),
      stringsAsFactors = FALSE
    ))
  }

  dat |>
    dplyr::mutate(
      record_id              = as.character(record_id),
      redcap_repeat_instance = as.integer(redcap_repeat_instance),
      dh_date                = as.Date(dh_date),
      dh_hours               = suppressWarnings(as.numeric(dh_hours)),
      dh_moonlighting_hours  = suppressWarnings(as.numeric(dh_moonlighting_hours)),
      dh_home_hours          = suppressWarnings(as.numeric(dh_home_hours))
    )
}

#' Overlay resident-confirmed/entered duty_hour_log rows onto
#' build_duty_hour_blocks()'s Amion-derived duty_blocks — for any
#' (record_id, Date) with a saved entry, the entry REPLACES whatever
#' Amion-derived row(s) existed that date (e.g. a synthetic AM+PM pair),
#' since the resident's own record is authoritative once it exists.
#' Moonlighting/at-home hours (if any, on ANY entry — confirmed or not)
#' are added as their own additional block rows, not merged into the main
#' category's hours.
#'
#' @param duty_blocks build_duty_hour_blocks()$duty_blocks (or the
#'   post-day-off-override version build_duty_hour_summary() already
#'   produces internally).
#' @param entries pull_duty_hour_log()'s return value.
#' @return duty_blocks with entry-covered dates replaced/augmented. Gains a
#'   `counts_toward_duty` logical column — currently always TRUE (both
#'   Moonlighting and At-Home Chart Review count toward Total_Hours, per
#'   Fred) but kept as a column rather than removed outright in case a
#'   future category needs excluding again; daily/weekly aggregation in
#'   duty_hour_summary.R sums Hours by category directly for the separate
#'   Home_Hours visibility column, not via this flag.
#' @export
overlay_duty_hour_entries <- function(duty_blocks, entries) {
  duty_blocks$counts_toward_duty <- TRUE

  if (is.null(entries) || nrow(entries) == 0) return(duty_blocks)

  # A resident's own saved row is authoritative regardless of dh_source —
  # "resident_entry_needed"/default Amion rows for that date are dropped
  # once ANY entry exists for it (confirmed or freshly entered).
  covered <- entries |> dplyr::distinct(record_id, dh_date)

  kept <- duty_blocks |>
    dplyr::anti_join(covered, by = c("record_id", "Date" = "dh_date"))

  # entries carries no name/Level (not part of duty_hour_log) — attach from
  # duty_blocks so daily/weekly grouping (which groups by record_id/name/
  # Level) doesn't split one resident into two inconsistent groups across
  # covered vs. uncovered dates.
  resident_names <- duty_blocks |> dplyr::distinct(record_id, name, Level)
  entries <- entries |> dplyr::left_join(resident_names, by = "record_id")

  main_rows <- entries |>
    dplyr::mutate(
      Date        = dh_date,
      category    = dh_category,
      Hours       = dh_hours,
      block_start = dh_start_time,
      block_end   = dh_end_time,
      source      = ifelse(dh_source == "Resident confirmed", "resident_confirmed", "resident_entered")
    )
  main_rows$block_start_dt <- .dt_from_hhmm(main_rows$Date, main_rows$block_start)
  start_int <- to_int_time(main_rows$block_start)
  end_int   <- to_int_time(main_rows$block_end)
  end_date  <- main_rows$Date
  wraps     <- !is.na(start_int) & !is.na(end_int) & end_int <= start_int
  end_date[wraps] <- end_date[wraps] + 1
  main_rows$block_end_dt <- .dt_from_hhmm(end_date, main_rows$block_end)
  main_rows$counts_toward_duty <- TRUE
  main_rows <- main_rows[, names(kept)]

  extra_rows <- function(hour_col, category_label, counts) {
    sub <- entries[!is.na(entries[[hour_col]]) & entries[[hour_col]] > 0, ]
    if (nrow(sub) == 0) return(kept[0, ])
    out <- data.frame(
      record_id = sub$record_id, name = sub$name, Level = sub$Level,
      Date = sub$dh_date, category = category_label, Hours = sub[[hour_col]],
      block_start = NA_character_, block_end = NA_character_,
      source = "resident_entered", block_start_dt = as.POSIXct(NA),
      block_end_dt = as.POSIXct(NA), counts_toward_duty = counts,
      stringsAsFactors = FALSE
    )
    out[, names(kept)]
  }
  moonlighting_rows <- extra_rows("dh_moonlighting_hours", "Moonlighting", TRUE)
  home_rows         <- extra_rows("dh_home_hours", "At-Home Chart Review", TRUE)

  dplyr::bind_rows(kept, main_rows, moonlighting_rows, home_rows) |>
    dplyr::arrange(record_id, Date)
}

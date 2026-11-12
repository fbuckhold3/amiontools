# =============================================================================
# Pull EVERY resident's conference-attendance log (RDM 2.0's "questions"
# repeating form) in one call, for the attendance-reconciliation join.
#
# imslu.at.noon already has get_resident_questions_history() for this same
# form, but it's scoped to one resident at a time (called on-demand as a
# resident checks their own attendance) -- reconciliation needs everyone at
# once. Deliberately kept minimal (record_id + q_date + q_conference_type
# only) since that's all the join needs.
# =============================================================================

#' @importFrom REDCapR redcap_read
NULL

#' Pull the RDM 2.0 "questions" form (conference attendance log) for every
#' resident.
#'
#' @param rdm_token RDM REDCap API token (test or prod — caller's choice).
#' @param redcap_url REDCap API URL.
#' @return Tibble: record_id, Date, q_conference_type (raw code "1"-"4",
#'   or a future 5th "Other" choice once added to the dictionary),
#'   entry_date (the calendar date the entry was actually SAVED — same as
#'   Date for a real-time submission, later for an after-the-fact one; lets
#'   callers distinguish "attended, logged live" from "attended, logged
#'   late"), entry_time (full save time as POSIXct, America/Chicago — NA
#'   if the timestamp is missing or unparseable). One row per logged entry — a resident can have zero, one, or
#'   more than one entry per date (e.g. both SLUH and VA logged the same
#'   day is possible, though unusual).
#' @param record_ids Optional character vector: read only these records, in
#'   one request (~0.2 s vs ~6 s for the whole program).
#' @export
pull_questions_log <- function(rdm_token, redcap_url, record_ids = NULL) {
  raw <- if (is.null(record_ids)) {
    REDCapR::redcap_read(redcap_uri = redcap_url, token = rdm_token, forms = "questions")$data
  } else {
    # fields = "record_id": without it a forms-only export drops record_id
    # and the redcap_repeat_* columns.
    REDCapR::redcap_read_oneshot(redcap_uri = redcap_url, token = rdm_token, forms = "questions",
                                 fields = "record_id", records = as.character(record_ids),
                                 verbose = FALSE)$data
  }

  raw <- raw[!is.na(raw$redcap_repeat_instrument) & raw$redcap_repeat_instrument == "questions", ]
  if (nrow(raw) == 0) {
    return(data.frame(record_id = character(), Date = as.Date(character()),
                       q_conference_type = character(), entry_date = as.Date(character()),
                       entry_time = as.POSIXct(character(), tz = "America/Chicago"),
                       stringsAsFactors = FALSE))
  }

  raw$record_id <- as.character(raw$record_id)
  raw$Date <- as.Date(raw$q_date)

  # Found live 2026-09-04: q_conference_type was added to this form after it
  # was already in use -- 2,419 of 2,844 total rows (legacy entries, dated
  # through 2026-07-23) predate the field and have it blank. Excluded here
  # rather than treated as "attended with unknown type", since there's no
  # way to distinguish a genuine legacy attendance entry from anything else
  # that might blank-fill this field. Known gap: residents' real attendance
  # in the first ~3 weeks of AY2026-27 (before 07-23) is undercounted by
  # this exclusion -- flagged, not silently guessed either way.
  raw <- raw[!is.na(raw$q_conference_type) & raw$q_conference_type != "", ]

  raw$entry_date <- as.Date(raw$q_entry_timestamp)
  # Full save time, for "logged during conference" checks. q_entry_timestamp
  # is stored as Central wall-clock time (at.noon's CONFERENCE_TIMEZONE;
  # ind.dash/mod_attendance too, as of 2026-10-20).
  # redcap_read() may already have guessed it as POSIXct (in UTC) — take
  # the wall-clock text back out rather than shifting it across zones.
  ts_chr <- if (inherits(raw$q_entry_timestamp, "POSIXt"))
    format(raw$q_entry_timestamp, "%Y-%m-%d %H:%M:%S") else as.character(raw$q_entry_timestamp)
  raw$entry_time <- as.POSIXct(ts_chr, tz = "America/Chicago", format = "%Y-%m-%d %H:%M:%S")

  raw[!is.na(raw$Date), c("record_id", "Date", "q_conference_type", "entry_date", "entry_time")]
}

#' Pull "did not attend" entries from the RDM `questions` form.
#'
#' These rows have a blank \code{q_conference_type}, so
#' \code{pull_questions_log()} excludes them. A failed read returns zero
#' rows: those days then show as not logged, so a resident can be asked to
#' log a day again but can never be credited for one.
#'
#' @param rdm_token,redcap_url RDM REDCap credentials.
#' @param record_ids Optional character vector: read only these records.
#' @return Tibble: record_id (chr), Date, reason (chr).
#' @export
pull_not_attended_log <- function(rdm_token, redcap_url, record_ids = NULL) {
  empty <- dplyr::tibble(record_id = character(), Date = as.Date(character()),
                          reason = character())
  raw <- tryCatch(
    REDCapR::redcap_read_oneshot(
      redcap_uri = redcap_url, token = rdm_token,
      fields = c("record_id", "q_date", "q_attended", "q_absence_reason"),
      records = if (is.null(record_ids)) NULL else as.character(record_ids),
      filter_logic = "[q_attended] = '0'",
      guess_type = FALSE, verbose = FALSE
    )$data,
    error = function(e) {
      message("[pull_not_attended_log] read failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(raw) || nrow(raw) == 0 || !"q_attended" %in% names(raw)) return(empty)
  raw <- raw[!is.na(raw$q_attended) & raw$q_attended == "0", , drop = FALSE]
  out <- dplyr::tibble(record_id = as.character(raw$record_id),
                        Date      = suppressWarnings(as.Date(raw$q_date)),
                        reason    = as.character(raw$q_absence_reason))
  out[!is.na(out$Date), , drop = FALSE]
}

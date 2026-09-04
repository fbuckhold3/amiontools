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
#'   late"). One row per logged entry — a resident can have zero, one, or
#'   more than one entry per date (e.g. both SLUH and VA logged the same
#'   day is possible, though unusual).
#' @export
pull_questions_log <- function(rdm_token, redcap_url) {
  raw <- REDCapR::redcap_read(
    redcap_uri = redcap_url,
    token      = rdm_token,
    forms      = "questions"
  )$data

  raw <- raw[!is.na(raw$redcap_repeat_instrument) & raw$redcap_repeat_instrument == "questions", ]
  if (nrow(raw) == 0) {
    return(data.frame(record_id = character(), Date = as.Date(character()),
                       q_conference_type = character(), entry_date = as.Date(character()),
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

  raw[!is.na(raw$Date), c("record_id", "Date", "q_conference_type", "entry_date")]
}

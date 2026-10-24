# attendance_entry.R ─ the one write path for the RDM `questions` form
# (noon conference / Afternoon School attendance).
#
# Pulled out of mod_attendance.R (2026-10-24) so every app that logs
# attendance — ind.dash and resident.digest via mod_attendance, and
# imslu.at.noon's live check-in — writes the same row the same way. The
# main thing this guards: the next repeat instance comes from a LIVE read,
# and a failed read means nothing is written. at.noon's own writer fell
# back to instance 1 on a failed read, which (since writes overwrite)
# could replace a resident's first entry of the year.

# Live max `questions` repeat instance for one record, read right before a
# save. Returns 0L when the record has no questions rows, and NA_integer_
# when REDCap can't be read — callers must treat NA as "don't write", since
# guessing an instance can overwrite an existing entry.
.att_live_max_question_instance <- function(record_id, redcap_url, rdm_token) {
  tryCatch({
    resp <- httr::POST(
      redcap_url,
      body = list(
        token = rdm_token, content = "record", action = "export",
        format = "json", type = "flat",
        records     = as.character(record_id),
        `forms[0]`  = "questions",
        `fields[0]` = "record_id",
        rawOrLabel = "raw", rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false", exportSurveyFields = "false",
        exportDataAccessGroups = "false", returnFormat = "json"
      ),
      encode = "form", httr::timeout(15)
    )
    if (httr::status_code(resp) != 200) return(NA_integer_)
    dat <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    # "[]" parses to an empty list (no rows); an {"error": ...} body parses
    # to a named list — the latter is a failure, not "zero instances".
    if (!is.data.frame(dat)) return(if (length(dat) == 0) 0L else NA_integer_)
    if (nrow(dat) == 0 || !"redcap_repeat_instrument" %in% names(dat)) return(0L)
    q <- dat[!is.na(dat$redcap_repeat_instrument) &
               dat$redcap_repeat_instrument == "questions", , drop = FALSE]
    inst <- suppressWarnings(as.integer(q$redcap_repeat_instance))
    inst <- inst[!is.na(inst)]
    if (length(inst) == 0) 0L else max(inst)
  }, error = function(e) {
    message("[save_attendance_entry] live instance lookup failed: ", conditionMessage(e))
    NA_integer_
  })
}

#' Save one conference-attendance entry to the RDM `questions` form.
#'
#' Shared by \code{mod_attendance_server()} (after-the-fact entry in
#' ind.dash / resident.digest) and imslu.at.noon (live check-in), so every
#' entry is written the same way. Validation of the resident-facing rules
#' (no future dates, the off-day "Save anyway?" prompt, which conferences
#' need a rotation) stays in the calling UI — this function only refuses
#' input that can't be written at all.
#'
#' @param record_id RDM record_id.
#' @param date Date the resident attended (Date, or "YYYY-MM-DD").
#' @param conference_type Raw \code{q_conference_type} code ("1"-"5";
#'   "3" = Afternoon School).
#' @param rotation Raw \code{q_rotation} code, or NULL/"" when the
#'   conference type doesn't ask for one.
#' @param answer Raw \code{q_answer} (at.noon's question of the day), or
#'   NULL/"".
#' @param rdm_token,redcap_url RDM REDCap credentials (test or prod —
#'   caller's choice).
#' @param min_instance Floor for the instance number, for callers that may
#'   have saved rows this session that a read could lag behind. The next
#'   instance is \code{max(live max, min_instance) + 1}.
#' @return A list: \code{success} (logical), \code{instance},
#'   \code{entry_timestamp} (Central wall-clock, as written), \code{date}
#'   ("YYYY-MM-DD"), \code{fields} (the named list written), \code{ts}
#'   (save time, from \code{gmed::rc_save_instance()}), and \code{message}
#'   (human-readable, on failure).
#' @export
save_attendance_entry <- function(record_id, date, conference_type,
                                  rotation = NULL, answer = NULL,
                                  rdm_token, redcap_url, min_instance = 0L) {
  fail <- function(msg) list(success = FALSE, message = msg)

  if (is.null(record_id) || !nzchar(as.character(record_id)))
    return(fail("No resident selected — nothing saved."))
  d <- tryCatch(as.Date(date), error = function(e) NA)
  if (length(d) != 1 || is.na(d))
    return(fail("Please select a date."))
  if (is.null(conference_type) || !nzchar(as.character(conference_type)))
    return(fail("Please select which conference."))

  live_max <- .att_live_max_question_instance(record_id, redcap_url, rdm_token)
  if (is.na(live_max))
    return(fail("Couldn't reach REDCap to confirm — nothing saved. Please try again."))
  floor_inst <- suppressWarnings(as.integer(min_instance))
  if (length(floor_inst) != 1 || is.na(floor_inst)) floor_inst <- 0L
  inst <- max(live_max, floor_inst) + 1L

  # Central wall-clock time, matching at.noon's CONFERENCE_TIMEZONE —
  # Sys.time() alone uses the server's zone (UTC on Connect), which would
  # skew the calendar's "logged during conference" window check.
  entry_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "America/Chicago")

  fields <- list(
    q_date             = format(d, "%Y-%m-%d"),
    q_conference_type  = as.character(conference_type),
    q_rotation         = if (is.null(rotation)) "" else as.character(rotation),
    q_answer           = if (is.null(answer)) "" else as.character(answer),
    q_entry_timestamp  = entry_timestamp,
    questions_complete = "2"
  )

  result <- gmed::rc_save_instance(record_id, "questions", inst, fields,
                                   redcap_url = redcap_url, rdm_token = rdm_token)
  if (!isTRUE(result$success))
    return(fail(if (!is.null(result$message)) result$message else "Save failed."))

  list(success = TRUE, instance = inst, entry_timestamp = entry_timestamp,
       date = fields$q_date, fields = fields, ts = result$ts, message = NULL)
}

# peer_review_counts.R ── windowed peer-review-completed count, for the
# weekly resident digest's "N peer reviews in the last 2 weeks" nudge.
#
# Separate from mod_peer_review_entry.R's own .peer_count_completed() (an
# all-time total, used for that module's summary strip) — this adds a date
# window, needed only by the digest, not the entry form itself. Same
# underlying query shape (peer_eval scanned across ALL records, filtered by
# peer_evaluator_id) — kept as its own small function rather than
# generalizing .peer_count_completed() with an optional window, to avoid
# touching that already-verified internal helper.

#' Count peer_eval instances a resident has completed (as evaluator) within
#' a trailing window.
#'
#' @param evaluator_id The resident's own record_id (peer_evaluator_id to
#'   match against).
#' @param redcap_url REDCap API base URL.
#' @param rdm_token REDCap API token (test or prod — caller's choice).
#' @param days Trailing window size in days (inclusive of today).
#' @param as_of Reference "today" — defaults to `Sys.Date()`.
#' @return A list: `n` (count within the window) and `n_total` (all-time
#'   count, same figure `.peer_count_completed()` would return).
#' @export
peer_count_completed_recent <- function(evaluator_id, redcap_url, rdm_token,
                                        days, as_of = Sys.Date()) {
  empty <- list(n = 0L, n_total = 0L)
  tryCatch({
    resp <- httr::POST(
      redcap_url,
      body = list(
        token = rdm_token, content = "record", action = "export",
        format = "json", type = "flat",
        `forms[0]`  = "peer_eval",
        `fields[0]` = "record_id",
        `fields[1]` = "peer_evaluator_id",
        `fields[2]` = "peer_date",
        rawOrLabel = "raw", rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false", exportSurveyFields = "false",
        exportDataAccessGroups = "false", returnFormat = "json"
      ),
      encode = "form", httr::timeout(30)
    )
    if (httr::status_code(resp) != 200) return(empty)
    dat <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    if (!is.data.frame(dat) || nrow(dat) == 0) return(empty)
    dat <- dat[!is.na(dat$redcap_repeat_instrument) &
                 dat$redcap_repeat_instrument == "peer_eval", , drop = FALSE]
    mine <- dat[!is.na(dat$peer_evaluator_id) &
                  trimws(as.character(dat$peer_evaluator_id)) == as.character(evaluator_id), , drop = FALSE]
    n_total <- nrow(mine)
    dates <- suppressWarnings(as.Date(mine$peer_date))
    dates <- dates[!is.na(dates)]
    n_recent <- sum(dates >= (as.Date(as_of) - days) & dates <= as.Date(as_of))
    list(n = n_recent, n_total = n_total)
  }, error = function(e) empty)
}

# =============================================================================
# RDM <-> Amion identity crosswalk.
#
# RDM's resident_data form carries amion_staff_id/amion_name/amion_verified/
# amion_match_date fields (added 2026-08-14) that link a record_id to its
# Amion Staff ID. This wraps the read so every consumer (rotation summary,
# attendance reconciliation, duty-hour tracking) pulls it the same way.
# =============================================================================

#' @importFrom REDCapR redcap_read
NULL

#' Pull the RDM record_id <-> Amion Staff ID crosswalk.
#'
#' @param rdm_token RDM REDCap API token (test or prod — caller's choice,
#'   this function does not default one for you).
#' @param redcap_url REDCap API URL.
#' @param verified_only Logical. If TRUE (default), only return residents
#'   whose match has been human-verified (`amion_verified == "1"`) rather
#'   than every non-blank row.
#' @return Tibble with record_id, name, amion_staff_id (integer),
#'   amion_verified, amion_match_date.
#' @export
get_amion_crosswalk <- function(rdm_token, redcap_url, verified_only = TRUE) {
  dat <- REDCapR::redcap_read(
    redcap_uri = redcap_url,
    token = rdm_token,
    fields = c("record_id", "name", "amion_staff_id", "amion_verified", "amion_match_date")
  )$data

  dat <- dat[!is.na(dat$amion_staff_id) & dat$amion_staff_id != "", ]
  if (isTRUE(verified_only)) {
    dat <- dat[!is.na(dat$amion_verified) & dat$amion_verified == "1", ]
  }
  dat$amion_staff_id <- as.integer(dat$amion_staff_id)
  dat[, c("record_id", "name", "amion_staff_id", "amion_verified", "amion_match_date")]
}

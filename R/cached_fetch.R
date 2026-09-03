# =============================================================================
# Cache-aware alternative to use_amion_data(): tries the REDCap app_cache
# record (written weekly by rdm-data-refresh's refresh_amion.qmd via
# gmed::write_amion_cache()) before ever touching Amion or RDM live.
#
# On a cache hit, returns reactive()s shaped exactly like the return values
# of build_rotation_summary()/build_team_summary()/build_time_allocation_
# summary() — pass them straight into mod_rotation_summary_server()/
# mod_team_summary_server()/mod_time_allocation_server()'s `summary_r` param
# and those modules skip both the live fetch AND the local aggregation
# entirely. On a cache miss/stale/malformed payload, returns NULL so the
# caller falls back to the existing use_amion_data() + crosswalk_r/amion_r
# path unchanged (see mod_schedule.R in ind.dash for the fallback wiring).
#
# Deliberately does NOT cover mod_daily_detail — that module's data
# (build_daily_detail(), ~12 MB/57K rows) is explicitly excluded from the
# REDCap cache (see gmed::write_amion_cache() docs) and stays a live fetch.
# =============================================================================

#' @importFrom shiny reactive
NULL

.AMION_CACHE_REQUIRED_FIELDS <- c(
  "rotation_summary_wide", "rotation_class_avg_wide",
  "team_summary_wide", "team_class_avg_wide",
  "team_off_summary_wide", "team_off_class_avg_wide",
  "talloc_summary_wide", "talloc_class_avg_wide"
)

#' Try loading amiontools' aggregate summaries from the REDCap app_cache
#' record instead of fetching Amion + RDM live.
#'
#' @param rdm_token RDM REDCap API token (test or prod — caller's choice).
#' @param redcap_url REDCap API URL.
#' @param cache_record_id Record ID of the app_cache record
#'   (default: \code{CACHE_RECORD_ID} env var).
#' @param max_age_hours Passed through to \code{gmed::load_cached_amion()}.
#'   Default 192h (8 days) — the cache refreshes weekly.
#'
#' @return A list with \code{rotation}, \code{team}, \code{talloc} reactives
#'   (each shaped like the matching \code{build_*_summary()} return value)
#'   on a cache hit, or \code{NULL} on a cache miss/stale/malformed payload
#'   — callers should fall back to \code{use_amion_data()} in that case.
#' @export
use_amion_data_cached <- function(rdm_token, redcap_url,
                                   cache_record_id = Sys.getenv("CACHE_RECORD_ID"),
                                   max_age_hours = 192) {

  # gmed is a hard Imports dependency of amiontools already (see DESCRIPTION,
  # crosswalk.R's calculate_resident_level() call) — no requireNamespace guard needed.
  cached <- gmed::load_cached_amion(
    rdm_token = rdm_token, redcap_url = redcap_url,
    cache_record_id = cache_record_id, max_age_hours = max_age_hours
  )

  if (is.null(cached)) return(NULL)

  missing <- setdiff(.AMION_CACHE_REQUIRED_FIELDS, names(cached))
  if (length(missing) > 0) {
    warning("use_amion_data_cached: cached payload missing field(s) [",
            paste(missing, collapse = ", "), "] — falling back to live fetch")
    return(NULL)
  }

  list(
    rotation = shiny::reactive(list(
      summary_wide   = cached$rotation_summary_wide,
      class_avg_wide = cached$rotation_class_avg_wide
    )),
    team = shiny::reactive(list(
      team_summary_wide   = cached$team_summary_wide,
      class_avg_wide      = cached$team_class_avg_wide,
      off_summary_wide    = cached$team_off_summary_wide,
      off_class_avg_wide  = cached$team_off_class_avg_wide,
      program_avg_wide    = cached$team_program_avg_wide  # may be NULL on an older cache; unread by current modules
    )),
    talloc = shiny::reactive(list(
      summary_wide   = cached$talloc_summary_wide,
      class_avg_wide = cached$talloc_class_avg_wide
    ))
  )
}

# =============================================================================
# Shared-fetch helper: pull the RDM crosswalk + Amion data ONCE and hand
# reactives to every display module, instead of each module independently
# re-fetching the same data.
#
# Found 2026-08-16: composing mod_rotation_summary + mod_team_summary +
# mod_time_allocation in one tab (ind.dash's Schedule) was doing ~4 redundant
# full-year Amion fetches + ~4 redundant RDM reads for one page load (each
# module's own reactive(), plus build_time_allocation_summary() internally
# calling build_rotation_summary() which fetches AGAIN) - a real, measured
# slowdown (load time roughly doubled), not just a theoretical concern.
#
# Not a Shiny module (no UI, no id/namespace needed) - just a function that
# creates two reactive()s in the caller's reactive domain and returns them.
# =============================================================================

#' @importFrom shiny reactive
NULL

#' Create shared crosswalk/Amion reactives for composing multiple amiontools
#' display modules (mod_rotation_summary, mod_team_summary,
#' mod_time_allocation) without each one re-fetching independently.
#'
#' Call ONCE per session (e.g. inside the orchestrating module's server,
#' before calling the individual mod_*_server() functions), then pass the
#' returned reactives into each mod_*_server() call's `crosswalk_r`/`amion_r`
#' arguments. Each build_*_summary() function also accepts `crosswalk`/
#' `amion` directly for non-Shiny (script) use — this just wires the same
#' plumbing through Shiny's reactive layer.
#'
#' Callers using shared data are responsible for ensuring every module they
#' compose actually wants the same ay_start/ay_end/staff_types/verified_only
#' — the shared reactives are fetched once with ONE set of those params.
#'
#' @inheritParams build_rotation_summary
#' @return list(crosswalk = reactive(...), amion = reactive(...))
#' @export
use_amion_data <- function(rdm_token,
                           redcap_url,
                           amion_lo = AMION_LO_DEFAULT,
                           ay_start = current_ay_start(),
                           ay_end = ay_start,
                           staff_types = c("R1", "R2", "R3"),
                           verified_only = TRUE) {
  crosswalk_r <- shiny::reactive({
    get_amion_crosswalk(rdm_token, redcap_url, verified_only = verified_only)
  })
  amion_r <- shiny::reactive({
    fetch_amion_data(urls = build_amion_urls(amion_lo, start_ay = ay_start, end_ay = ay_end))
  })
  list(crosswalk = crosswalk_r, amion = amion_r)
}

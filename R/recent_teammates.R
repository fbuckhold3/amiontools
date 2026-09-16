# =============================================================================
# Recent teammates: "who has this resident actually worked with lately"
#
# Requested by Fred 2026-09-16 to power a quick-pick suggestion list for the
# ind.dash peer-review entry form (an evaluator often wants to review someone
# they were just on service with).
#
# Bug found and fixed same day, before this ever shipped to Fred: the first
# version matched on the RAW Amion `Assignment Name` string via
# build_daily_detail()'s concatenated Team_Assignment column (e.g. "Green
# Senior"). That's a single-occupant role/slot label, not the team — no two
# residents are ever literally "Green Senior" the same day, so it silently
# matched nobody, ever (confirmed live: Oliver Chrisler had 8 real "Green
# Senior" days, zero matches came back). The actual teammates are people on
# *other* roles of the *same team* (e.g. "Green Intern A/B" while this
# resident is "Green Senior"). Fixed by building on the canonical `team`
# field from classify_team_assignment() instead — the same classification
# build_team_summary() already uses and has been live-verified against real
# rosters — and matching on (Date, team), not (Date, raw Assignment Name).
# =============================================================================

#' @importFrom dplyr filter select mutate group_by summarise ungroup arrange desc slice_max distinct inner_join
NULL

#' Find residents who shared a team assignment with a given resident recently.
#'
#' A day counts as "shared" when both residents are classified onto the same
#' `team` (via `classify_team_assignment()`, e.g. "Green", "VA Floors A") on
#' the same date, restricted to on-duty kinds ("team", "nf_coverage") — same
#' definition `build_team_summary()` uses for on-duty membership, not the
#' off/call/jeopardy status tags.
#'
#' @inheritParams build_team_summary
#' @param resident_id The resident to find recent teammates for (RDM record_id).
#' @param days How many trailing days (inclusive of today) to look back. Default 14.
#' @return A data frame: record_id, name, team (most recently shared),
#'   last_shared_date. Zero rows (not an error) if the resident has no Amion
#'   match or no recent team assignment.
#' @export
get_recent_teammates <- function(resident_id,
                                  rdm_token,
                                  redcap_url,
                                  days = 14,
                                  amion_lo = AMION_LO_DEFAULT,
                                  ay_start = current_ay_start(),
                                  ay_end = ay_start,
                                  staff_types = c("R1", "R2", "R3"),
                                  verified_only = TRUE,
                                  crosswalk = NULL,
                                  amion = NULL) {

  empty <- data.frame(record_id = character(0), name = character(0),
                       team = character(0), last_shared_date = as.Date(character(0)),
                       stringsAsFactors = FALSE)

  if (is.null(crosswalk)) {
    crosswalk <- get_amion_crosswalk(rdm_token, redcap_url, verified_only = verified_only)
  }
  if (is.null(amion)) {
    amion <- fetch_amion_data(urls = build_amion_urls(amion_lo, start_ay = ay_start, end_ay = ay_end))
  }

  resident_id <- as.character(resident_id)
  cutoff <- Sys.Date() - days

  o_only <- amion |>
    dplyr::filter(`Staff Type` %in% staff_types, `Assignment Type` == "o") |>
    dplyr::inner_join(crosswalk, by = c("Staff ID" = "amion_staff_id"))
  if (nrow(o_only) == 0) return(empty)
  o_only$record_id <- as.character(o_only$record_id)

  classified   <- classify_team_assignment(o_only$`Assignment Name`)
  o_only$kind  <- classified$kind
  o_only$team  <- classified$team

  recent <- o_only |>
    dplyr::filter(kind %in% c("team", "nf_coverage"), !is.na(team),
                  Date >= cutoff, Date <= Sys.Date()) |>
    dplyr::distinct(record_id, name, Date, team)

  mine <- recent |>
    dplyr::filter(record_id == resident_id) |>
    dplyr::select(Date, team)
  if (nrow(mine) == 0) return(empty)

  recent |>
    dplyr::filter(record_id != resident_id) |>
    dplyr::inner_join(mine, by = c("Date", "team")) |>
    dplyr::group_by(record_id, name, team) |>
    dplyr::summarise(last_shared_date = max(Date), .groups = "drop") |>
    dplyr::group_by(record_id, name) |>
    dplyr::slice_max(order_by = last_shared_date, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(last_shared_date)) |>
    as.data.frame()
}

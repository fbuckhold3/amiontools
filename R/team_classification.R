# =============================================================================
# Team-level classification of Amion's Assignment Type == "o" rows.
#
# Grouping on "o" rows is useless for this (always just "On Call" — see
# amion_integration project notes 2026-08-15). The real team/role/slot detail
# lives in Assignment Name itself, e.g. "Yellow Intern A", "MICU 1 Resident",
# "VA Floors C Senior". This is a full enumerated map (71 names), confirmed
# field-by-field with Fred, not a regex guess — Amion's naming isn't
# consistent enough to parse reliably (e.g. "Senior B" means a second,
# concurrent person on the same team, not a different sub-team, while
# "VA Floors B" IS a genuinely different sub-team from "VA Floors A" — no
# regex distinguishes those without being told).
#
# `kind` values:
#   "team"            — a real ward/ICU team assignment (Green, VA Floors C,
#                        MICU 1, etc.) — the thing this feature tracks.
#   "nf_coverage"      — Night Float's per-person coverage assignment (which
#                        service they're covering that night) — NOT a ward
#                        team, tracked separately.
#   "status_off"       — day off, not an assignment.
#   "status_call"      — a call/duty tag layered on top of (or instead of) a
#                        team assignment, e.g. "Rapid Response" or "VA Floor
#                        Intern on Call". Confirmed NOT a team (Fred corrected
#                        an earlier read where Rapid Response looked like its
#                        own elective) — but flagged as relevant to future
#                        duty-hour work, since these mark longer "call" days
#                        (~12h) vs the baseline (~10h).
#   "status_jeopardy"  — backup/jeopardy call roles, not a team.
# =============================================================================

.team_map_rows <- list(
  c("Bronze A", "team", "Bronze", "Senior", "A"),
  c("Bronze B", "team", "Bronze", "Senior", "B"),
  c("Cardiology A", "team", "Cardiology", "Senior", "A"),
  c("Cardiology B", "team", "Cardiology", "Senior", "B"),
  c("Diamond Resident", "team", "Diamond", "Resident", NA),
  c("Gold Resident", "team", "Gold", "Resident", NA),
  c("Green Senior", "team", "Green", "Senior", NA),
  c("Green Senior B", "team", "Green", "Senior", "B"),
  c("Green Intern A", "team", "Green", "Intern", "A"),
  c("Green Intern B", "team", "Green", "Intern", "B"),
  c("Red Senior", "team", "Red", "Senior", NA),
  c("Red Senior B", "team", "Red", "Senior", "B"),
  c("Red Intern A", "team", "Red", "Intern", "A"),
  c("Red Intern B", "team", "Red", "Intern", "B"),
  c("White Senior", "team", "White", "Senior", NA),
  c("White Senior B", "team", "White", "Senior", "B"),
  c("White Intern A", "team", "White", "Intern", "A"),
  c("White Intern B", "team", "White", "Intern", "B"),
  c("Yellow Senior", "team", "Yellow", "Senior", NA),
  c("Yellow Senior B", "team", "Yellow", "Senior", "B"),
  c("Yellow Intern A", "team", "Yellow", "Intern", "A"),
  c("Yellow Intern B", "team", "Yellow", "Intern", "B"),
  c("MICU 1 Resident", "team", "MICU 1", "Resident", NA),
  c("MICU 1 Intern A", "team", "MICU 1", "Intern", "A"),
  c("MICU 1 Intern B", "team", "MICU 1", "Intern", "B"),
  c("MICU 2 Resident A", "team", "MICU 2", "Resident", "A"),
  c("MICU 2 Resident B", "team", "MICU 2", "Resident", "B"),
  c("MICU 2 Intern", "team", "MICU 2", "Intern", NA),
  c("VA Floors A Senior", "team", "VA Floors A", "Senior", NA),
  c("VA Floors B Senior", "team", "VA Floors B", "Senior", NA),
  c("VA Floors C Senior", "team", "VA Floors C", "Senior", NA),
  c("VA Floors D Senior", "team", "VA Floors D", "Senior", NA),
  c("VA Floors A Intern", "team", "VA Floors A", "Intern", NA),
  c("VA Floors B Intern", "team", "VA Floors B", "Intern", NA),
  c("VA Floors C Intern", "team", "VA Floors C", "Intern", NA),
  c("VA Floors D Intern", "team", "VA Floors D", "Intern", NA),

  c("NF ICU Intern", "nf_coverage", "Night Float", "Intern", "covers ICU"),
  c("NF Intern A", "nf_coverage", "Night Float", "Intern", "A (service unclear)"),
  c("NF Intern B", "nf_coverage", "Night Float", "Intern", "B (service unclear)"),
  c("NF Med Admissions", "nf_coverage", "Night Float", NA, "covers Med Admissions"),
  c("NF Resident A (VA Night Resident)", "nf_coverage", "Night Float", "Resident", "covers VA nights"),
  c("NF Resident B (MICU)", "nf_coverage", "Night Float", "Resident", "covers MICU nights"),
  c("NF Resident C (Cards Bronze)", "nf_coverage", "Night Float", "Resident", "covers Cards Bronze nights"),

  # team attribution added 2026-08-17 where the label itself specifies a
  # rotation (Fred wanted off-days visible/reconciled against on-team days).
  # "Intern off"/"SLU Res off"/"SLU Res Off B" stay unattributed (team=NA)
  # -- genuinely ambiguous which rotation they're from, Amion just doesn't
  # say. Note MICU off-labels don't distinguish MICU 1 vs MICU 2 either
  # (coarser than the on-team attribution, which does) -- same limitation.
  c("Intern off", "status_off", NA, "Intern", NA),
  c("MICU Intern off", "status_off", "MICU", "Intern", NA),
  c("MICU Resident Off", "status_off", "MICU", "Resident", NA),
  c("NF Intern Off", "status_off", "Night Float", "Intern", NA),
  c("NF Resident Off", "status_off", "Night Float", "Resident", NA),
  c("SLU Res off", "status_off", NA, NA, NA),
  c("SLU Res Off B", "status_off", NA, NA, "B"),
  c("VA Floor Intern Off", "status_off", "VA Floors", "Intern", NA),
  c("VA Floor Resident Off", "status_off", "VA Floors", "Resident", NA),

  c("Intern A on call", "status_call", NA, "Intern", "A"),
  c("Intern B on call", "status_call", NA, "Intern", "B"),
  c("MICU Resident on Call 7A-7P", "status_call", NA, "Resident", NA),
  c("MICU Intern on Call 7A-7P", "status_call", NA, "Intern", NA),
  c("VA Floor Intern on Call 7AM-7PM", "status_call", NA, "Intern", NA),
  c("VA Floor Intern on Call 7a-7a", "status_call", NA, "Intern", NA),
  c("VA Floor Resident on Call 7AM-7PM", "status_call", NA, "Resident", NA),
  c("VA Floor Resident on Call 7a-7a", "status_call", NA, "Resident", NA),
  c("Cards-Bronze Long Call", "status_call", NA, NA, NA),
  c("SLU Rapid Response & Code Team", "status_call", NA, NA, NA),
  c("SLU Rapid Response & Code Team B", "status_call", NA, NA, "B"),

  c("1 Intern Jeopardy", "status_jeopardy", NA, "Intern", "1st backup"),
  c("2 Intern Jeopardy", "status_jeopardy", NA, "Intern", "2nd backup"),
  c("3 Intern Jeopardy", "status_jeopardy", NA, "Intern", "3rd backup"),
  c("1 Resident Jeopardy", "status_jeopardy", NA, "Resident", "1st backup"),
  c("2 Resident Jeopardy", "status_jeopardy", NA, "Resident", "2nd backup"),
  c("3 Resident Jeopardy", "status_jeopardy", NA, "Resident", "3rd backup"),
  c("4 Resident Jeopardy", "status_jeopardy", NA, "Resident", "4th backup"),
  c("Sunday Night Jeopardy", "status_jeopardy", NA, NA, NA)
)

#' Amion `o`-type Assignment Name -> {kind, team, role, slot}. Confirmed with
#' Fred field-by-field 2026-08-15 against the full current-AY inventory (71
#' names). See file header for what each `kind` means.
#' @export
TEAM_ASSIGNMENT_MAP <- do.call(rbind.data.frame, c(
  lapply(.team_map_rows, function(r) {
    data.frame(assignment_name = r[1], kind = r[2], team = r[3],
               role = r[4], slot = r[5], stringsAsFactors = FALSE)
  }),
  list(stringsAsFactors = FALSE)
))
rownames(TEAM_ASSIGNMENT_MAP) <- NULL

#' Classify a vector of Amion `Assignment Name` values (from Assignment
#' Type=='o' rows) into team/role/slot. Anything not in TEAM_ASSIGNMENT_MAP
#' comes back with kind="UNMAPPED" rather than silently dropping/guessing —
#' Amion's o-type vocabulary can grow (new team, renamed slot) and this
#' should surface, not vanish.
#' @param assignment_name Character vector of Amion `Assignment Name` values.
#' @return data.frame with columns assignment_name, kind, team, role, slot —
#'   one row per input value, same order.
#' @export
classify_team_assignment <- function(assignment_name) {
  idx <- match(assignment_name, TEAM_ASSIGNMENT_MAP$assignment_name)
  out <- TEAM_ASSIGNMENT_MAP[idx, c("kind", "team", "role", "slot")]
  out$kind[is.na(idx)] <- "UNMAPPED"
  data.frame(assignment_name = assignment_name, out, row.names = NULL)
}

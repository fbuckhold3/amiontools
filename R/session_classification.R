# =============================================================================
# Classification of Amion's Assignment Type == "c" rows (half-day clinic/
# didactic sessions — the "blue" blocks on Amion's calendar view).
#
# Grouping is useless here too (always just "Clinic" — same pattern as "o"
# rows always being "On Call"). Confirmed with Fred 2026-08-16 against the
# full 57-name current-AY inventory.
#
# Category names deliberately reuse ROTATION_CATEGORY_MAP's names where they
# mean the same thing (e.g. "Continuity Clinic") — this is what lets
# build_rotation_summary() correct the Continuity Clinic overcounting bug
# by matching c-session category against r-row category (see
# team_summary.R / rotation_summary.R comments, and amion_integration
# project notes 2026-08-16).
# =============================================================================

.session_map_rows <- list(
  c("CC CSM, am", "Continuity Clinic"),
  c("CC CSM, pm", "Continuity Clinic"),
  c("CC VA, pm", "Continuity Clinic"),
  c("Bridge Clinic, am", "BRIDGE"),
  c("Bridge Clinic, pm", "BRIDGE"),
  c("Educational Half Day, am", "Educational"),
  c("ITE, am", "Educational"),
  c("ITE, pm", "Educational"),
  c("MKSAP, am", "Educational"),
  c("MKSAP, pm", "Educational"),
  c("Journal Club, am", "Educational"),
  c("PEAC, pm", "Educational"),
  c("Afternoon School, pm", "Educational"),
  c("POCUS, pm", "Educational"),
  c("Step 3, am", "Educational"),
  c("Step 3, pm", "Educational"),
  c("VA Same Day, am", "VA Ambulatory"),
  c("VA Same Day, pm", "VA Ambulatory"),
  c("VA Nephro Clinic, am", "VA Ambulatory"),
  c("VA Nephro Clinic, pm", "VA Ambulatory"),
  c("VA Palliative Consults, am", "VA Ambulatory"),
  c("VA Palliative Consults, pm", "VA Ambulatory"),
  c("VA Palliative Clinic, am", "VA Ambulatory"),
  c("VA Palliative Clinic, pm", "VA Ambulatory"),
  c("VA Pulm, am", "VA Ambulatory"),
  c("VA QI, am", "VA Ambulatory"),
  c("VA QI, pm", "VA Ambulatory"),
  c("VA Cards Clinic, am", "VA Ambulatory"),
  c("VA Rheum, am", "VA Ambulatory"),
  c("VA Rheum, pm", "VA Ambulatory"),
  c("VA GI Endoscopy, am", "VA Ambulatory"),
  c("VA GI Clinic, am", "VA Ambulatory"),
  c("VA GI Clinic, pm", "VA Ambulatory"),
  c("VA GI Paracentesis Clinic, am", "VA Ambulatory"),
  c("VA Endo, am", "VA Ambulatory"),
  c("VA Endo, pm", "VA Ambulatory"),
  c("VA Heme/Onc, am", "VA Ambulatory"),
  c("VA Heme/Onc, pm", "VA Ambulatory"),
  c("VA Cath lab, am", "VA Ambulatory"),
  c("VA Stress Lab, am", "VA Ambulatory"),
  c("VA ID, am", "VA Ambulatory"),
  c("VA ID, pm", "VA Ambulatory"),
  c("VA Reading, am", "VA Ambulatory"),
  c("VA Reading, pm", "VA Ambulatory"),
  c("VA Radiology, am", "VA Ambulatory"),
  c("SLU Rheum, am", "SLUH Ambulatory"),
  c("SLU Rheum, pm", "SLUH Ambulatory"),
  c("SLU Pulm, am", "SLUH Ambulatory"),
  c("SLU Metabolic Clinic, am", "SLUH Metabolic"),
  c("SLU Metabolic Clinic, pm", "SLUH Metabolic"),
  c("SLU Metabolism, pm", "SLUH Metabolic"),
  c("Admin, am", "Other/Admin"),
  c("Admin, pm", "Other/Admin"),
  c("Day Off, am", "Time Off/Holiday"),
  c("Day Off, pm", "Time Off/Holiday"),
  c("Wellness Day, am", "Time Off/Holiday"),
  c("Wellness Day, pm", "Time Off/Holiday")
)

#' Amion `c`-type Assignment Name (half-day sessions) -> category. Confirmed
#' with Fred 2026-08-16 against the full 57-name current-AY inventory.
#' @export
SESSION_CATEGORY_MAP <- stats::setNames(
  vapply(.session_map_rows, `[[`, character(1), 2),
  vapply(.session_map_rows, `[[`, character(1), 1)
)

#' Roll up a vector of raw Amion `Assignment Name` values (from Assignment
#' Type=='c' rows) into session categories via SESSION_CATEGORY_MAP.
#' Anything not in the map comes back as "UNMAPPED" rather than silently
#' dropping/guessing.
#' @param assignment_name Character vector of Amion `Assignment Name` values.
#' @export
classify_session <- function(assignment_name) {
  out <- unname(SESSION_CATEGORY_MAP[assignment_name])
  out[is.na(out)] <- "UNMAPPED"
  out
}

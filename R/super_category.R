# =============================================================================
# Super-category rollup: every rotation/session category (from both
# ROTATION_CATEGORY_MAP and SESSION_CATEGORY_MAP) collapsed into the 8
# top-level buckets: Inpatient, Ambulatory, Continuity Clinic, Educational,
# Emergency, Elective, Time Off, Other. (Confirmed with Fred 2026-08-16;
# Time Off split out of the generic "Other" bucket into its own category
# 2026-08-16 — vacation/holiday days are a metric worth seeing on their
# own, not folded in alongside Jeopardy/Admin.)
# =============================================================================

#' Category (from either classify_rotation() or classify_session()) ->
#' super-category. Confirmed with Fred 2026-08-16.
#' @export
SUPER_CATEGORY_MAP <- c(
  "SLUH Inpatient"      = "Inpatient",
  "ICU"                 = "Inpatient",
  "Night Float"         = "Inpatient",
  "VA Inpatient"        = "Inpatient",
  "Addiction Consults"  = "Inpatient",

  "ACS"                 = "Ambulatory",
  "BRIDGE"              = "Ambulatory",
  "VA Ambulatory"       = "Ambulatory",
  "SLUH ID"             = "Ambulatory",
  "SLUH Metabolic"      = "Ambulatory",
  "SLUH Ambulatory"     = "Ambulatory",

  "Continuity Clinic"   = "Continuity Clinic",

  "Educational"         = "Educational",

  "VA Emergency"        = "Emergency",
  "SLUH Emergency"      = "Emergency",

  "Elective"            = "Elective",

  "Time Off/Holiday"    = "Time Off",

  "Jeopardy"            = "Other",
  "Other/Admin"         = "Other",

  # Duty-hour-log-only categories (not part of the original rotation/session
  # vocabulary) — folded into "Other" per Fred, who named Continuity Clinic/
  # Inpatient/Other as the buckets he actually wants distinguished (2026-09-15).
  "Moonlighting"           = "Other",
  "At-Home Chart Review"   = "Other"
)

#' Fixed-order categorical palette for the 8 super-categories, from the
#' dataviz skill's validated default (8 slots, CVD-safe on the ADJACENT
#' pairlist — the right gate for a stacked bar chart / adjacent calendar
#' cells; validated 2026-09-15, `node scripts/validate_palette.js`). Slot
#' ORDER is the CVD-safety mechanism (never cycle/reorder); the
#' entity-to-slot ASSIGNMENT below is ours — Inpatient/Continuity Clinic/
#' Ambulatory deliberately hold slots 1-3 (the subset that also clears the
#' stronger all-pairs gate) since Fred specifically wants Continuity
#' Clinic vs. Inpatient vs. everything-else to read clearly at a glance.
#' Light-mode hex only — this app has no dark-mode chart surface today
#' (matches every other hardcoded chart color already in this package).
#' @export
DUTY_HOUR_CATEGORY_COLORS <- c(
  "Inpatient"         = "#2a78d6",  # slot 1 blue
  "Continuity Clinic" = "#eb6834",  # slot 2 orange
  "Ambulatory"        = "#1baf7a",  # slot 3 aqua
  "Educational"       = "#eda100",  # slot 4 yellow
  "Elective"          = "#e87ba4",  # slot 5 magenta
  "Emergency"         = "#008300",  # slot 6 green
  "Time Off"          = "#4a3aa7",  # slot 7 violet
  "Other"             = "#e34948"   # slot 8 red
)

#' Roll up a vector of categories (from classify_rotation() or
#' classify_session()) into the 8 super-categories. Anything not in
#' SUPER_CATEGORY_MAP comes back as "UNMAPPED".
#' @param category Character vector of category values.
#' @export
classify_super_category <- function(category) {
  out <- unname(SUPER_CATEGORY_MAP[category])
  out[is.na(out)] <- "UNMAPPED"
  out
}

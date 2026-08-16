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
  "Other/Admin"         = "Other"
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

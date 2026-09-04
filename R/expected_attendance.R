# =============================================================================
# Expected-conference classification: for a given day's rotation category,
# which noon conference (if any) is that resident expected to attend.
#
# Rule table finalized with Fred, 2026-09-03/09-04:
#   - SLUH Inpatient, ICU, ACS, BRIDGE, SLUH ID, SLUH Metabolic, Addiction
#     Consults, SLUH Ambulatory -> SLUH
#   - VA Inpatient, VA Ambulatory -> VA
#   - Continuity Clinic -> SLUH, EXCEPT a same-date "CC VA, pm" session
#     (raw c-type Assignment Name, not the rolled-up category) -> None
#     (excused; Fred: residents doing VA afternoon clinic during their
#     continuity-clinic block don't attend SLUH conference that day)
#   - Night Float, Elective, VA/SLUH Emergency, Time Off/Holiday, Jeopardy,
#     Other/Admin -> None
#
# General attendance rule (Fred): on an expected day, ANY logged
# q_conference_type value (including a future "Other/Specialty Conference"
# choice, not yet added to the dictionary) counts as attended -- no strict
# SLUH-vs-VA type matching required. That's applied in
# attendance_reconciliation.R, not here -- this file only classifies what's
# EXPECTED, not whether it was met.
#
# Also NOT encoded in EXPECTED_CONFERENCE_MAP itself, applied by the caller:
# noon conference only happens on weekdays -- a Saturday/Sunday is never an
# expected day regardless of rotation category. Reasonably safe as an
# unstated assumption (every GME lunch conference is weekday-only), but
# flagged here rather than silently baked in with no comment.
# =============================================================================

#' Amion rotation category (from `classify_rotation()`) -> expected
#' conference site. "SLUH" / "VA" / "None".
#' @export
EXPECTED_CONFERENCE_MAP <- c(
  "SLUH Inpatient"      = "SLUH",
  "ICU"                 = "SLUH",
  "ACS"                 = "SLUH",
  "BRIDGE"              = "SLUH",
  "SLUH ID"             = "SLUH",
  "SLUH Metabolic"      = "SLUH",
  "Addiction Consults"  = "SLUH",
  "SLUH Ambulatory"     = "SLUH",

  "VA Inpatient"        = "VA",
  "VA Ambulatory"       = "VA",

  "Continuity Clinic"   = "SLUH",  # default; CC-VA-pm exception applied separately

  "Night Float"         = "None",
  "Elective"            = "None",
  "VA Emergency"        = "None",
  "SLUH Emergency"      = "None",
  "Time Off/Holiday"    = "None",
  "Jeopardy"            = "None",
  "Other/Admin"         = "None"
)

#' Raw c-type Assignment Name that excuses a Continuity Clinic day from the
#' SLUH-conference expectation (VA afternoon clinic during a CC block).
#' @export
CC_VA_EXCUSAL_SESSION <- "CC VA, pm"

#' Classify expected conference site for a vector of (rotation category,
#' clinic-session) pairs.
#'
#' @param category Character vector — rolled-up rotation category, e.g. from
#'   \code{classify_rotation(daily_detail$Rotation)}.
#' @param clinic_sessions Character vector, same length — the raw
#'   \code{Clinic_Sessions} value from \code{build_daily_detail()} (";"-
#'   joined if more than one that day), used only to detect the CC-VA-pm
#'   excusal. NA/empty is fine (means "no c-type session that day").
#' @return Character vector: "SLUH", "VA", or "None". Anything not in
#'   \code{EXPECTED_CONFERENCE_MAP} (including "UNMAPPED" from
#'   \code{classify_rotation()}) comes back "None" rather than guessed.
#' @export
classify_expected_conference <- function(category, clinic_sessions = NA_character_) {
  expected <- unname(EXPECTED_CONFERENCE_MAP[category])
  expected[is.na(expected)] <- "None"

  is_cc_va_excused <- category == "Continuity Clinic" &
    !is.na(clinic_sessions) &
    grepl(CC_VA_EXCUSAL_SESSION, clinic_sessions, fixed = TRUE)
  expected[is_cc_va_excused] <- "None"

  expected
}

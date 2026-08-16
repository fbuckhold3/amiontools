# =============================================================================
# Rotation classification: roll up Amion's raw `Grouping` field (~50 values)
# into a smaller set of top-level rotation categories.
#
# Confirmed with Fred 2026-08-14 against real current-AY data (see
# amiontools' companion validation work in the rounds/ scratchpad history).
# Deliberately granular in places he asked to keep separate (ACS, BRIDGE,
# SLUH ID, SLUH Metabolic, Addiction Consults) rather than lumped into a
# generic bucket.
# =============================================================================

#' First academic year (start-year form, e.g. 2026 for AY 2026-27) under the
#' current schedule. The program's schedule was completely rebuilt starting
#' this AY, so any multi-year cumulative rotation tracking (e.g. a
#' categorical resident's rotation mix over their full 3 years) must not
#' pull data from before this year — earlier years used a different
#' schedule structure and aren't comparable.
#' @export
AMION_SCHEDULE_CHANGE_AY <- 2026L

#' Amion `Grouping` -> top-level rotation category.
#' @export
ROTATION_CATEGORY_MAP <- c(
  "SLU Floors"            = "SLUH Inpatient",
  "Bronze"                = "SLUH Inpatient",
  "Cardiology"             = "SLUH Inpatient",
  "Diamond"                = "SLUH Inpatient",
  "DIAMOND"                = "SLUH Inpatient",
  "Gold"                   = "SLUH Inpatient",

  "ACS"                    = "ACS",
  "BRIDGE"                 = "BRIDGE",

  "MICU"                   = "ICU",

  "NF"                     = "Night Float",
  "Night Float"            = "Night Float",

  "VA Floors"              = "VA Inpatient",

  "VA Same day"            = "VA Ambulatory",
  "VA OP Cardiology"       = "VA Ambulatory",
  "VA OP Nephro"           = "VA Ambulatory",
  "VA OP Palliative"       = "VA Ambulatory",
  "VA Endo"                = "VA Ambulatory",
  "VA GI"                  = "VA Ambulatory",
  "VA HemeOnc"             = "VA Ambulatory",
  "VA ID"                  = "VA Ambulatory",
  "VA Pulm"                = "VA Ambulatory",
  "VA QI"                  = "VA Ambulatory",
  "VA Radiology"           = "VA Ambulatory",
  "VA Rheum"               = "VA Ambulatory",
  "VA Nephro & VA Rheum"   = "VA Ambulatory",

  "VA ED"                  = "VA Emergency",
  "SLU ED"                 = "SLUH Emergency",

  "SLU ID"                 = "SLUH ID",
  "SLU Metabolic"          = "SLUH Metabolic",

  "SLU Nephro"             = "SLUH Ambulatory",
  "SLU Rheum"              = "SLUH Ambulatory",
  "SLU Endo"               = "SLUH Ambulatory",
  "SLU Pulm"               = "SLUH Ambulatory",
  "SLU Heme Onc"           = "SLUH Ambulatory",
  "SLU Allergy"            = "SLUH Ambulatory",
  "SLU GI Clinic"          = "SLUH Ambulatory",
  "SLU Palliative"         = "SLUH Ambulatory",
  "SLU Sleep"              = "SLUH Ambulatory",
  "Cards Consults"         = "SLUH Ambulatory",
  "optho"                  = "SLUH Ambulatory",
  "Primary Care"           = "SLUH Ambulatory",

  "Continuity Clinic"      = "Continuity Clinic",
  "Elective"               = "Elective",

  "VACA"                   = "Time Off/Holiday",
  "Leave"                  = "Time Off/Holiday",
  "Christmas Holiday"      = "Time Off/Holiday",
  "Thanksgiving Holiday"   = "Time Off/Holiday",
  "NY Holiday"             = "Time Off/Holiday",
  "Eid Holiday"            = "Time Off/Holiday",

  "1 Jeopardy"             = "Jeopardy",
  "1 Int Jeopardy"         = "Jeopardy",

  "Addiction consults"     = "Addiction Consults",

  "Chief"                  = "Other/Admin",
  "Scholarly Activity"     = "Other/Admin",
  "Hybrid Study"           = "Other/Admin"
)

#' Roll up a vector of raw Amion `Grouping` values into top-level rotation
#' categories via ROTATION_CATEGORY_MAP. Anything not in the map comes back
#' as "UNMAPPED" rather than silently dropping/guessing — callers should
#' treat any UNMAPPED rows as a sign the map needs a new entry (Amion's
#' Grouping vocabulary can grow, e.g. a new rotation or renamed team).
#' @param grouping Character vector of raw Amion `Grouping` values.
#' @export
classify_rotation <- function(grouping) {
  out <- unname(ROTATION_CATEGORY_MAP[grouping])
  out[is.na(out)] <- "UNMAPPED"
  out
}

#' Convert an Amion HH:MM-style time string/number to a 4-digit integer
#' (e.g. "8:00" -> 800L) for comparison against known half-day blocks.
#' @param x Character or numeric time value from Amion's Start/End Time cols.
#' @export
to_int_time <- function(x) {
  s <- formatC(suppressWarnings(as.integer(x)), width = 4, flag = "0")
  suppressWarnings(as.integer(s))
}

#' Day-value (in days) for a single Amion assignment row, based on its
#' Start/End time. Standard AM (0800-1200) and PM (1300-1700) half-day
#' blocks count as 0.5. Shorter 2-hour PM blocks (1400-1600, 1500-1700 —
#' seen on "Afternoon School"/"POCUS" sessions, confirmed with Fred
#' 2026-08-16) count as 0.25. Anything else counts as a full day (1.0).
#' @param start_time,end_time Character/numeric Start Time / End Time values.
#' @export
day_value <- function(start_time, end_time) {
  s_int <- to_int_time(start_time)
  e_int <- to_int_time(end_time)
  dplyr::case_when(
    s_int ==  800 & e_int == 1200 ~ 0.5,
    s_int == 1300 & e_int == 1700 ~ 0.5,
    s_int == 1400 & e_int == 1600 ~ 0.25,
    s_int == 1500 & e_int == 1700 ~ 0.25,
    TRUE                          ~ 1.0
  )
}

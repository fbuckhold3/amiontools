# =============================================================================
# Default duty-hour assumptions per rotation category, plus the call-day
# override and a clock-time duration helper. This turns Amion's schedule
# markers (which mostly just say WHICH rotation, not what time it ran) into
# actual duty hours — a different question from build_rotation_summary()'s
# day-FRACTION metric ("Days"), which this file doesn't touch or replace.
#
# Confirmed with Fred 2026-09-07, same iterative-review pattern as
# ROTATION_CATEGORY_MAP/TEAM_ASSIGNMENT_MAP. Two things flagged as NOT fully
# settled — do not treat as verified until Fred confirms against real output:
#
#   1. "VA Floor Intern/Resident on Call 7a-7a" (TEAM_ASSIGNMENT_MAP) is
#      named like a literal 24h call block (7am to 7am the next day) —
#      distinct from the "7AM-7PM" variant of the same call. This reads as
#      inconsistent with Fred's separate statement that the program has no
#      24h shifts (save jeopardy). Per his explicit instruction ("all call
#      days are 0700-1900"), this file applies the uniform 12h call override
#      to every call-tagged day regardless of which specific tag text is
#      present — the "7a-7a" naming is NOT given special 24h treatment here.
#      Flagged in case that naming turns out to mean something real once
#      real data is reviewed.
#   2. On-campus half-day educational sessions (Afternoon School, ITE,
#      MKSAP, Journal Club, PEAC, POCUS, Step 3 — see SESSION_CATEGORY_MAP's
#      "Educational" category) are treated as CARVED OUT of the underlying
#      rotation day for HOUR totals (not added on top) — see
#      duty_hour_summary.R. This is the opposite of
#      build_time_allocation_summary()'s deliberately additive treatment of
#      the same sessions for its DAY-COUNT metric — different metric,
#      different answer on purpose (adding education hours on top of a full
#      ward day would inflate the compliance total). Flagged for Fred to
#      confirm once he sees real numbers.
# =============================================================================

#' @importFrom dplyr case_when
NULL

#' Per-rotation-category default duty-hour block.
#'
#' `default_start`/`default_end` (HHMM strings, e.g. "0700") give the
#' fallback full-day block for categories with a fixed daily schedule.
#' `call_start`/`call_end` give the override block used on any date that
#' also carries a call tag (see \code{.CALL_TAGS_EXCLUDED} below) — NA where
#' that category has no call concept.
#' `uses_c_override = TRUE` means: prefer real same-date, same-category
#' `c`-type session times (via \code{classify_session()}) over the default
#' block when they exist — mirrors the same-category override
#' \code{build_rotation_summary()} already uses for its day-fraction metric.
#' When no matching c-session exists that date, falls back to a synthetic
#' AM (0800-1200) + PM (1300-1700) pair (8h) rather than one continuous
#' 9h block — matches how these categories actually show up in the c-session
#' data (two separate half-day sessions, not one span).
#' `resident_entered = TRUE` means no default exists at all — Amion doesn't
#' predict these hours, the resident must enter them (Phase 2+).
#' @export
DUTY_HOUR_DEFAULT_MAP <- data.frame(
  category         = c("SLUH Inpatient", "ICU", "Night Float", "VA Inpatient",
                       "Addiction Consults", "SLUH ID",
                       "ACS", "BRIDGE", "VA Ambulatory", "SLUH Ambulatory", "SLUH Metabolic",
                       "Continuity Clinic",
                       "Elective", "VA Emergency", "SLUH Emergency", "Other/Admin"),
  default_start    = c("0700", "0700", "1900", "0700",
                       "0800", "0800",
                       NA, NA, NA, NA, NA,
                       NA,
                       NA, NA, NA, NA),
  default_end      = c("1700", "1700", "0800", "1500",
                       "1700", "1700",
                       NA, NA, NA, NA, NA,
                       NA,
                       NA, NA, NA, NA),
  call_start       = c("0700", "0700", NA, "0700",
                       NA, NA,
                       NA, NA, NA, NA, NA,
                       NA,
                       NA, NA, NA, NA),
  call_end         = c("1900", "1900", NA, "1900",
                       NA, NA,
                       NA, NA, NA, NA, NA,
                       NA,
                       NA, NA, NA, NA),
  uses_c_override  = c(FALSE, FALSE, FALSE, FALSE,
                       FALSE, FALSE,
                       TRUE, TRUE, TRUE, TRUE, TRUE,
                       TRUE,
                       FALSE, FALSE, FALSE, FALSE),
  resident_entered = c(FALSE, FALSE, FALSE, FALSE,
                       FALSE, FALSE,
                       FALSE, FALSE, FALSE, FALSE, FALSE,
                       FALSE,
                       TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

#' Call-tag names (from TEAM_ASSIGNMENT_MAP, kind == "status_call") that do
#' NOT independently trigger the call-hour override — per Fred (2026-09-07):
#' the Rapid Response/Code Team tag is layered on top of the same underlying
#' ward call, not a distinct call type, so it shouldn't double-drive an
#' override. A date with ONLY one of these tags and no other status_call tag
#' falls back to that day's normal category default (no call override) —
#' an edge case worth watching for in real data, flagged rather than assumed
#' impossible.
#' @export
.CALL_TAGS_EXCLUDED <- c("SLU Rapid Response & Code Team", "SLU Rapid Response & Code Team B")

#' Duration in hours between two HHMM values (character or integer, e.g.
#' "0700"/700), handling an overnight wrap (end <= start means end is the
#' next calendar day — e.g. Night Float 1900 -> 0800).
#' @param start,end HHMM character/integer vectors (same length or
#'   recycled), as produced by Amion's Start Time/End Time columns or the
#'   fixed strings in DUTY_HOUR_DEFAULT_MAP.
#' @export
duty_hour_duration <- function(start, end) {
  s <- to_int_time(start)
  e <- to_int_time(end)
  s_min <- (s %/% 100) * 60 + (s %% 100)
  e_min <- (e %/% 100) * 60 + (e %% 100)
  diff_min <- ifelse(e_min <= s_min, e_min + 24 * 60 - s_min, e_min - s_min)
  diff_min / 60
}

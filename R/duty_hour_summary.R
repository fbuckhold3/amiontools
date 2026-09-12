# =============================================================================
# Duty-hour block construction + summary/flags — Phase 1 (read-only default
# generation only; no editing or persistence yet — see amion_integration
# project notes, 2026-09-07 kickoff, for the full staged plan). Turns
# Amion's schedule markers into an actual-hours estimate per resident-date
# via DUTY_HOUR_DEFAULT_MAP (duty_hour_defaults.R), then rolls up to daily/
# weekly/rolling-4-week totals plus three informational flags — nothing
# here enforces anything, it's a read-only preview for Fred to validate the
# default-generation logic against real data before anything becomes
# editable/persisted (same staged pattern every other amiontools feature
# used).
#
# Deliberately does NOT include moonlighting or at-home chart-review time —
# Amion has no visibility into either (both are resident self-report,
# Phase 2+ work once there's a persisted log to write them into). Totals
# here are Amion-derived only; any UI showing this must label it as such.
#
# Day-off handling: a "Day off" within a ward-roster rotation still carries
# an r-type marker row (the rotation continues; the resident is just off
# that day — see build_team_summary()'s detail_off / .WARD_ROSTER_CATEGORIES
# for the documented+inferred off-day logic this file reuses rather than
# re-deriving). That means a naive block build would credit a full ward day
# on a real day off. Fixed by overriding any "default"/"call" block to 0h on
# a date build_team_summary() already identifies as Off — same override
# pattern build_rotation_summary() uses for its own same-category c-session
# correction.
# =============================================================================

#' @importFrom dplyr filter mutate group_by summarise left_join anti_join arrange lag bind_rows n distinct case_when first select
#' @importFrom lubridate floor_date
NULL

.KNOWN_ZERO_HOUR_CATEGORIES <- c("Time Off/Holiday", "Jeopardy")

#' HHMM (character/integer) + Date -> POSIXct, vectorized, NA-safe.
#' @keywords internal
.dt_from_hhmm <- function(date, hhmm) {
  out <- rep(as.POSIXct(NA), length(date))
  ok <- !is.na(hhmm) & !is.na(date)
  if (any(ok)) {
    h <- to_int_time(hhmm[ok])
    out[ok] <- as.POSIXct(paste(as.character(date[ok]), sprintf("%04d", h)),
                          format = "%Y-%m-%d %H%M", tz = "America/Chicago")
  }
  out
}

#' Build one row per resident duty block (a contiguous span of estimated
#' duty time) for the current AY — the unit build_duty_hour_summary()
#' aggregates from.
#'
#' @inheritParams build_rotation_summary
#' @return A list with:
#'   - duty_blocks: record_id, name, Level, Date, category, Hours,
#'     block_start/block_end (HHMM character, NA where Hours is 0/unknown),
#'     block_start_dt/block_end_dt (POSIXct, NA where unknown), source (one
#'     of "default", "call", "c_session", "default_am"/"default_pm"
#'     (synthetic AM/PM fallback), "vacation", "jeopardy",
#'     "resident_entry_needed", "unmapped_category")
#'   - educational_detail: the c-type "Educational" session rows
#'     (Afternoon School, ITE, MKSAP, etc.) — carved OUT of duty_blocks'
#'     hour totals on purpose, see file header in duty_hour_defaults.R;
#'     returned here so callers can see/verify what was excluded.
#' @export
build_duty_hour_blocks <- function(rdm_token,
                                   redcap_url,
                                   amion_lo = AMION_LO_DEFAULT,
                                   ay_start = current_ay_start(),
                                   ay_end = ay_start,
                                   staff_types = c("R1", "R2", "R3"),
                                   verified_only = TRUE,
                                   crosswalk = NULL,
                                   amion = NULL) {

  if (is.null(crosswalk)) {
    crosswalk <- get_amion_crosswalk(rdm_token, redcap_url, verified_only = verified_only)
  }
  if (is.null(amion)) {
    amion <- fetch_amion_data(urls = build_amion_urls(amion_lo, start_ay = ay_start, end_ay = ay_end))
  }

  r_only <- amion |>
    dplyr::filter(`Staff Type` %in% staff_types, `Assignment Type` == "r") |>
    dplyr::inner_join(crosswalk, by = c("Staff ID" = "amion_staff_id")) |>
    dplyr::mutate(category = classify_rotation(Grouping)) |>
    dplyr::distinct(record_id, name, Level, Date, category)

  o_only <- amion |>
    dplyr::filter(`Staff Type` %in% staff_types, `Assignment Type` == "o") |>
    dplyr::inner_join(crosswalk, by = c("Staff ID" = "amion_staff_id"))
  o_only$kind <- classify_team_assignment(o_only$`Assignment Name`)$kind

  # Excludes .CALL_TAGS_EXCLUDED (Rapid Response/Code Team) per Fred
  # 2026-09-07 — layered on the same underlying ward call, not a distinct
  # trigger.
  call_dates <- o_only |>
    dplyr::filter(kind == "status_call", !(`Assignment Name` %in% .CALL_TAGS_EXCLUDED)) |>
    dplyr::distinct(record_id, Date) |>
    dplyr::mutate(has_call = TRUE)

  c_only <- amion |>
    dplyr::filter(`Staff Type` %in% staff_types, `Assignment Type` == "c") |>
    dplyr::inner_join(crosswalk, by = c("Staff ID" = "amion_staff_id")) |>
    dplyr::mutate(
      category  = classify_session(`Assignment Name`),
      seg_hours = duty_hour_duration(`Start Time`, `End Time`)
    )

  educational_detail <- c_only |> dplyr::filter(category == "Educational")

  c_sessions <- c_only |>
    dplyr::filter(category != "Educational") |>
    dplyr::group_by(record_id, name, Level, Date, category) |>
    dplyr::summarise(
      # min-start/max-end (not first()/first()) so a day with BOTH a real
      # AM and PM session (e.g. two Bridge Clinic sessions) reports the
      # true full-day span for gap-checking purposes, not just the first
      # segment's times while Hours silently reflects both — found via
      # live-data verification (2026-09-07): a Liam Arnold BRIDGE day
      # showed Hours=8 but block_start/end="0800-1200" (4h) before this fix.
      block_start = as.character(`Start Time`[which.min(to_int_time(`Start Time`))]),
      block_end   = as.character(`End Time`[which.max(to_int_time(`End Time`))]),
      Hours = sum(seg_hours),
      .groups = "drop"
    )

  base <- r_only |>
    dplyr::left_join(call_dates, by = c("record_id", "Date")) |>
    dplyr::mutate(has_call = !is.na(has_call))

  # -- fixed 0h: Vacation (Time Off/Holiday), Jeopardy (not activated) ------
  zero_hour <- base |>
    dplyr::filter(category %in% .KNOWN_ZERO_HOUR_CATEGORIES) |>
    dplyr::mutate(Hours = 0, block_start = NA_character_, block_end = NA_character_,
                  source = ifelse(category == "Time Off/Holiday", "vacation", "jeopardy")) |>
    dplyr::select(record_id, name, Level, Date, category, Hours, block_start, block_end, source)

  remaining <- base |>
    dplyr::filter(!category %in% .KNOWN_ZERO_HOUR_CATEGORIES) |>
    dplyr::left_join(DUTY_HOUR_DEFAULT_MAP, by = "category")

  # Category not in DUTY_HOUR_DEFAULT_MAP at all (includes classify_rotation()'s
  # own "UNMAPPED") — surfaced, not silently dropped, matching package
  # convention elsewhere (should normally be empty).
  unmapped_category <- remaining |>
    dplyr::filter(is.na(resident_entered)) |>
    dplyr::mutate(Hours = NA_real_, block_start = NA_character_, block_end = NA_character_,
                  source = "unmapped_category") |>
    dplyr::select(record_id, name, Level, Date, category, Hours, block_start, block_end, source)

  entry_needed <- remaining |>
    dplyr::filter(!is.na(resident_entered), resident_entered) |>
    dplyr::mutate(Hours = NA_real_, block_start = NA_character_, block_end = NA_character_,
                  source = "resident_entry_needed") |>
    dplyr::select(record_id, name, Level, Date, category, Hours, block_start, block_end, source)

  fixed <- remaining |>
    dplyr::filter(!is.na(resident_entered), !resident_entered, !uses_c_override) |>
    dplyr::mutate(
      use_call    = has_call & !is.na(call_start),
      block_start = ifelse(use_call, call_start, default_start),
      block_end   = ifelse(use_call, call_end, default_end),
      Hours       = duty_hour_duration(block_start, block_end),
      source      = ifelse(use_call, "call", "default")
    ) |>
    dplyr::select(record_id, name, Level, Date, category, Hours, block_start, block_end, source)

  # AM/PM-pattern categories: prefer real same-date/same-category c-session
  # time; fall back to a synthetic AM+PM pair (8h total) when none exists.
  # Two rows possible per resident-date here (kept separate, not merged) so
  # the rest-gap check sees the real intra-day structure (a lunch gap
  # between two same-category blocks is always exempt anyway — see
  # .SHORT_FLEXIBLE_CATEGORIES below).
  c_pattern_dates <- remaining |>
    dplyr::filter(!is.na(resident_entered), !resident_entered, uses_c_override) |>
    dplyr::distinct(record_id, name, Level, Date, category)

  matched_sessions <- c_pattern_dates |>
    dplyr::inner_join(c_sessions, by = c("record_id", "name", "Level", "Date", "category")) |>
    dplyr::mutate(source = "c_session") |>
    dplyr::select(record_id, name, Level, Date, category, Hours, block_start, block_end, source)

  unmatched <- c_pattern_dates |>
    dplyr::anti_join(c_sessions, by = c("record_id", "name", "Level", "Date", "category"))

  synthetic_am <- unmatched |>
    dplyr::mutate(Hours = duty_hour_duration("0800", "1200"),
                  block_start = "0800", block_end = "1200", source = "default_am") |>
    dplyr::select(record_id, name, Level, Date, category, Hours, block_start, block_end, source)
  synthetic_pm <- unmatched |>
    dplyr::mutate(Hours = duty_hour_duration("1300", "1700"),
                  block_start = "1300", block_end = "1700", source = "default_pm") |>
    dplyr::select(record_id, name, Level, Date, category, Hours, block_start, block_end, source)

  duty_blocks <- dplyr::bind_rows(
    zero_hour, unmapped_category, entry_needed, fixed,
    matched_sessions, synthetic_am, synthetic_pm
  ) |>
    dplyr::arrange(record_id, Date)

  duty_blocks$block_start_dt <- .dt_from_hhmm(duty_blocks$Date, duty_blocks$block_start)

  # block_end_dt computed from the block_end HHMM directly (wrap-aware),
  # NOT from block_start_dt + Hours*3600 — Hours legitimately excludes a
  # same-day lunch gap for multi-segment c_session days (see fix above),
  # so deriving the end time from Hours would silently understate the
  # real departure time whenever that happens. end_int <= start_int means
  # an overnight wrap (e.g. Night Float 1900 -> 0800) — bump to Date+1.
  start_int <- to_int_time(duty_blocks$block_start)
  end_int   <- to_int_time(duty_blocks$block_end)
  end_date  <- duty_blocks$Date
  wraps     <- !is.na(start_int) & !is.na(end_int) & end_int <= start_int
  end_date[wraps] <- end_date[wraps] + 1
  duty_blocks$block_end_dt <- .dt_from_hhmm(end_date, duty_blocks$block_end)

  list(duty_blocks = duty_blocks, educational_detail = educational_detail)
}

# Rest-gap exemption list (Fred, 2026-09-07): a gap is only ever evaluated
# when the PRECEDING block is NOT one of these short/flexible types —
# covers both his examples (clinic->clinic, clinic->moonlighting) without
# needing per-pair special-casing. "Moonlighting" is listed pre-emptively
# for Phase 2 (not producible from Amion data yet, so never actually
# appears as a source in Phase 1's duty_blocks).
.SHORT_FLEXIBLE_CATEGORIES <- c("ACS", "BRIDGE", "VA Ambulatory", "SLUH Ambulatory",
                                "SLUH Metabolic", "Continuity Clinic", "Elective",
                                "Moonlighting")

#' @keywords internal
.compute_rest_gap_flags <- function(duty_blocks, hard_gap_hours, soft_gap_hours) {
  known <- duty_blocks |>
    dplyr::filter(!is.na(block_start_dt), !is.na(block_end_dt)) |>
    dplyr::arrange(record_id, block_start_dt)

  known |>
    dplyr::group_by(record_id) |>
    dplyr::mutate(
      prev_category = dplyr::lag(category),
      prev_end      = dplyr::lag(block_end_dt),
      gap_hours     = as.numeric(difftime(block_start_dt, prev_end, units = "hours"))
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      !is.na(prev_end), gap_hours < soft_gap_hours,
      !(prev_category %in% .SHORT_FLEXIBLE_CATEGORIES),
      # Found via live-data verification (2026-09-07): Night Float's
      # confirmed end (0800) is definitionally later than every other
      # category's confirmed/synthetic start (0700-0800), so a Night
      # Float -> different-category transition mathematically produces a
      # ~0h "gap" on essentially EVERY occurrence, regardless of what
      # actually happens at that handoff -- 100% of the 67 flags this
      # produced program-wide were gap_hours in {-1, 0}. That's an
      # artifact of two independently-given default boundary times
      # colliding, not a detected violation, so it's excluded here rather
      # than shipped as false-positive noise. Flagged to Fred: what
      # actually happens between the last Night Float night and the next
      # rotation's first day (a scheduled recovery day? a later start
      # time Amion doesn't show?) -- once known, this exclusion should be
      # replaced with a real default for that specific transition instead
      # of a blanket skip. Same-category NF->NF transitions (mid-block
      # nights) are NOT affected by this exclusion and are still checked.
      !(prev_category == "Night Float" & category != "Night Float")
    ) |>
    dplyr::mutate(severity = ifelse(gap_hours < hard_gap_hours, "hard", "soft")) |>
    dplyr::select(record_id, name, from_category = prev_category, from_end = prev_end,
                  to_category = category, to_start = block_start_dt, gap_hours, severity)
}

#' Build daily/weekly/rolling-4-week duty-hour totals per resident, plus
#' three informational flags — Phase 1, read-only, nothing here enforces
#' anything:
#'   - flag_80h: rolling 4-week average total hours > 80 (a single hard
#'     week bracketed by lighter ones is NOT flagged in isolation — only
#'     the rolling average is, per Fred 2026-09-07)
#'   - rest_gap_flags: a "hard" (< hard_gap_hours) or "soft" (< soft_gap_hours)
#'     flagged transition between two known-time duty blocks, per the
#'     exemption rule in .SHORT_FLEXIBLE_CATEGORIES above
#'   - flag_low_days: cumulative average days-worked-per-week (vacation
#'     weeks excluded) below days_per_week_target
#'
#' Moonlighting and at-home chart-review time are NOT included — see file
#' header.
#'
#' @inheritParams build_rotation_summary
#' @param hard_gap_hours,soft_gap_hours Rest-gap thresholds in hours.
#'   Default 2 (hard) / 10 (soft) per Fred (2026-09-07, picked as the
#'   midpoint of his stated "1-3 hour" range) — easy to retune once real
#'   data is reviewed.
#' @param days_per_week_target Reference line for the days-worked-per-week
#'   metric (default 5.5, per Fred).
#' @param entries Optional pre-fetched \code{pull_duty_hour_log()} result —
#'   pass this to overlay resident-confirmed/entered hours over the Amion
#'   defaults (see duty_hour_entries.R). NULL (default) fetches internally
#'   via \code{rdm_token}/\code{redcap_url} — pass \code{entries =
#'   data.frame()} explicitly to skip the overlay entirely (Amion-only,
#'   the original Phase 1 behavior).
#' @return A list:
#'   - duty_blocks, educational_detail: from build_duty_hour_blocks(), with
#'     duty_blocks' "default"/"call" rows on a real day off (see
#'     build_team_summary()$detail_off) overridden to Hours=0/source="day_off"
#'   - daily: record_id/name/Level/Date/Total_Hours/is_vacation
#'   - weekly: + week_start/Total_Hours/Days_Worked/rolling_4wk_avg_hours/
#'     flag_80h/rolling_days_per_week/flag_low_days
#'   - rest_gap_flags: one row per flagged transition
#'   - unmapped: duty_blocks rows with Hours NA that are NOT the expected
#'     resident_entry_needed case (i.e. source == "unmapped_category") —
#'     should be empty; non-empty means DUTY_HOUR_DEFAULT_MAP needs a new entry
#' @export
build_duty_hour_summary <- function(rdm_token,
                                    redcap_url,
                                    amion_lo = AMION_LO_DEFAULT,
                                    ay_start = current_ay_start(),
                                    ay_end = ay_start,
                                    staff_types = c("R1", "R2", "R3"),
                                    verified_only = TRUE,
                                    crosswalk = NULL,
                                    amion = NULL,
                                    hard_gap_hours = 2,
                                    soft_gap_hours = 10,
                                    days_per_week_target = 5.5,
                                    entries = NULL) {

  if (is.null(crosswalk)) {
    crosswalk <- get_amion_crosswalk(rdm_token, redcap_url, verified_only = verified_only)
  }
  if (is.null(amion)) {
    amion <- fetch_amion_data(urls = build_amion_urls(amion_lo, start_ay = ay_start, end_ay = ay_end))
  }

  built <- build_duty_hour_blocks(
    rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
    ay_start = ay_start, ay_end = ay_end, staff_types = staff_types,
    verified_only = verified_only, crosswalk = crosswalk, amion = amion
  )
  duty_blocks <- built$duty_blocks

  # Reuses build_team_summary()'s existing documented+inferred Off logic
  # rather than re-deriving it (see file header). Recomputes from the
  # already-fetched crosswalk/amion — no additional live fetch.
  team <- build_team_summary(
    rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
    ay_start = ay_start, ay_end = ay_end, staff_types = staff_types,
    verified_only = verified_only, crosswalk = crosswalk, amion = amion
  )
  day_off_dates <- team$detail_off |>
    dplyr::distinct(record_id, Date) |>
    dplyr::mutate(is_day_off = TRUE)

  duty_blocks <- duty_blocks |>
    dplyr::left_join(day_off_dates, by = c("record_id", "Date")) |>
    dplyr::mutate(is_day_off = !is.na(is_day_off))
  override <- duty_blocks$is_day_off & duty_blocks$source %in% c("default", "call")
  duty_blocks$Hours[override]          <- 0
  duty_blocks$block_start[override]    <- NA_character_
  duty_blocks$block_end[override]      <- NA_character_
  duty_blocks$block_start_dt[override] <- as.POSIXct(NA)
  duty_blocks$block_end_dt[override]   <- as.POSIXct(NA)
  duty_blocks$source[override]         <- "day_off"
  duty_blocks$is_day_off <- NULL

  # Resident-confirmed/entered rows (Phase 2, duty_hour_log) override
  # everything above for their date — a saved resident record is
  # authoritative over both the Amion default AND the inferred day-off
  # override. entries=data.frame() (explicit empty) skips this entirely.
  if (is.null(entries)) {
    entries <- pull_duty_hour_log(rdm_token = rdm_token, redcap_url = redcap_url)
  }
  duty_blocks <- overlay_duty_hour_entries(duty_blocks, entries)

  daily <- duty_blocks |>
    dplyr::filter(!is.na(Hours)) |>
    dplyr::group_by(record_id, name, Level, Date) |>
    dplyr::summarise(
      Total_Hours = sum(Hours[counts_toward_duty]),
      Home_Hours  = sum(Hours[!counts_toward_duty]),
      .groups = "drop"
    ) |>
    dplyr::arrange(record_id, Date)

  is_vacation_date <- duty_blocks |>
    dplyr::filter(source == "vacation") |>
    dplyr::distinct(record_id, Date) |>
    dplyr::mutate(is_vacation = TRUE)

  daily <- daily |>
    dplyr::left_join(is_vacation_date, by = c("record_id", "Date")) |>
    dplyr::mutate(is_vacation = !is.na(is_vacation))

  weekly <- daily |>
    dplyr::mutate(week_start = as.Date(lubridate::floor_date(Date, "week", week_start = 7))) |>
    dplyr::group_by(record_id, name, Level, week_start) |>
    dplyr::summarise(
      Total_Hours = sum(Total_Hours),
      Home_Hours  = sum(Home_Hours),
      Days_Worked = sum(Total_Hours > 0 & !is_vacation),
      all_vacation = all(is_vacation),
      .groups = "drop"
    ) |>
    dplyr::arrange(record_id, week_start) |>
    dplyr::group_by(record_id) |>
    dplyr::mutate(
      rolling_4wk_avg_hours = sapply(seq_along(Total_Hours), function(i) {
        lo <- max(1, i - 3)
        mean(Total_Hours[lo:i])
      }),
      flag_80h = rolling_4wk_avg_hours > 80,
      # Cumulative average days-worked-per-week over all NON-all-vacation
      # weeks up to and including this one ("absent vacation" per Fred) —
      # not a fixed 4-week window like the hours flag, since this is meant
      # to read as a longer-run trend.
      rolling_days_per_week = {
        keep <- !all_vacation
        sapply(seq_along(Days_Worked), function(i) {
          idx <- which(keep[1:i])
          if (length(idx) == 0) return(NA_real_)
          mean(Days_Worked[idx])
        })
      },
      flag_low_days = rolling_days_per_week < days_per_week_target
    ) |>
    dplyr::ungroup()

  rest_gap_flags <- .compute_rest_gap_flags(duty_blocks, hard_gap_hours, soft_gap_hours)

  unmapped <- duty_blocks |> dplyr::filter(source == "unmapped_category")

  list(
    duty_blocks         = duty_blocks,
    educational_detail  = built$educational_detail,
    daily               = daily,
    weekly              = weekly,
    rest_gap_flags      = rest_gap_flags,
    unmapped            = unmapped
  )
}

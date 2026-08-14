# =============================================================================
# Amion data fetch layer.
#
# Amion's CSV export is public/unauthenticated, keyed by a program token
# (Lo=), not a secret. This mirrors the pattern originally built for
# amion-va-report — moved here so every app (VA report, rotation analysis,
# attendance reconciliation, duty-hour tracking) shares one fetch
# implementation instead of each reimplementing it.
# =============================================================================

#' @importFrom httr GET stop_for_status content timeout
#' @importFrom lubridate mdy year month days_in_month
NULL

AMION_COLS <- c(
  "Name", "Staff ID", "Backup Staff ID", "Assignment Name",
  "Assignment ID", "Backup Assignment ID", "Date", "Start Time",
  "End Time", "Staff Type", "Pager", "Tel", "Email", "Messagable",
  "Shift Note", "Assignment Type", "Grouping"
)

AMION_BASE <- "http://www.amion.com/cgi-bin/ocs?Lo=%s&Rpt=625c&Month=%s&Days=%d"

#' Amion program identifier. This is NOT a secret — it's the public lookup
#' code for the SLU IM Residency on amion.com. Override via the AMION_LO
#' environment variable if a different program ever needs to use this
#' package.
#' @export
AMION_LO_DEFAULT <- "ADMINSLUIM"

#' Earliest academic year Amion data is fetched from by default. Bump this
#' if older history is needed.
#' @export
AMION_START_AY <- 2022L

#' Academic-year start year (e.g. 2026 for AY 2026-27) containing `today`.
#' AY runs Jul 1 - Jun 30.
#' @param today Date to evaluate; defaults to today.
#' @export
current_ay_start <- function(today = Sys.Date()) {
  yr <- as.integer(format(today, "%Y"))
  if (as.integer(format(today, "%m")) >= 7) yr else yr - 1L
}

#' Format a Date/date-like column into an academic-year label, e.g. "2026-2027"
#' @param d Date vector
#' @export
academic_year <- function(d) {
  yr      <- lubridate::year(d)
  start_y <- ifelse(lubridate::month(d) >= 7, yr, yr - 1)
  paste0(start_y, "-", start_y + 1)
}

#' Build the list of Amion year-block URLs to fetch
#' @param amion_lo The Lo= program token
#' @param start_ay Earliest AY start year to include (e.g. 2022 for AY 2022-23)
#' @param end_ay   Latest AY start year to include; defaults to the AY of
#'                 today (i.e. always covers the *current* year through next
#'                 June 30) so callers keep working without code changes.
#' @export
build_amion_urls <- function(amion_lo,
                             start_ay = AMION_START_AY,
                             end_ay   = current_ay_start()) {
  ay_starts <- start_ay:end_ay
  vapply(ay_starts, function(yr) {
    # AY runs Jul 1 of yr through Jun 30 of yr+1; Feb 29 falls in yr+1
    yp1  <- yr + 1L
    leap <- (yp1 %% 4 == 0 && yp1 %% 100 != 0) || (yp1 %% 400 == 0)
    days <- if (leap) 366L else 365L
    sprintf(AMION_BASE, amion_lo,
            sprintf("7-%02d", yr %% 100),
            days)
  }, character(1))
}

#' Fetch all Amion year-blocks and return a single combined data frame.
#' Use fetch_amion_month() instead when only one month is needed — much
#' cheaper than a full academic-year pull.
#' @param amion_lo The Lo= value (program token) from Amion
#' @param urls Optional vector of full URLs; otherwise auto-built from the
#'             start AY through the current AY (covers past + current year).
#' @export
fetch_amion_data <- function(amion_lo = Sys.getenv("AMION_LO", AMION_LO_DEFAULT),
                             urls = NULL) {
  if (!nzchar(amion_lo)) amion_lo <- AMION_LO_DEFAULT
  if (is.null(urls)) urls <- build_amion_urls(amion_lo)
  read_one <- function(url) {
    resp <- httr::GET(url, httr::timeout(60))
    httr::stop_for_status(resp)
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    lines <- unlist(strsplit(txt, "\n"))
    lines <- lines[-(1:8)]
    df <- read.csv(text = paste(lines, collapse = "\n"),
                   header = FALSE, stringsAsFactors = FALSE)
    colnames(df) <- AMION_COLS
    df
  }
  data_all <- do.call(rbind, lapply(urls, read_one))
  data_all$Date <- lubridate::mdy(data_all$Date)
  data_all$Academic_Year <- academic_year(data_all$Date)
  data_all
}

#' Fetch ONE month from Amion. Builds a single URL like Month=4-26&Days=30
#' instead of pulling whole academic years.
#' @param month integer 1-12
#' @param year  integer 4-digit year
#' @export
fetch_amion_month <- function(month, year,
                              amion_lo = Sys.getenv("AMION_LO", AMION_LO_DEFAULT)) {
  if (!nzchar(amion_lo)) amion_lo <- AMION_LO_DEFAULT
  first_of_month <- as.Date(sprintf("%d-%02d-01", year, month))
  days_in_month  <- as.integer(lubridate::days_in_month(first_of_month))
  url <- sprintf(AMION_BASE, amion_lo,
                 sprintf("%d-%02d", month, year %% 100),
                 days_in_month)
  fetch_amion_data(amion_lo = amion_lo, urls = url)
}

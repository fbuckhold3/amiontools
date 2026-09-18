# mod_duty_hour_page.R ─ Duty Hours: full-page composer
#
# Promoted from imslu.ind.dash's mod_duty_hours.R / imslu.resident.digest's
# mod_duty_entry.R (2026-09-19) — those two files were byte-for-byte
# identical composers, hand-duplicated because at the time this was "a
# plain Shiny app, not a package ind.dash could depend on." Both apps
# already depend on amiontools directly, so that reasoning no longer
# applies; this is now the single source of truth for the page, matching
# the same promotion already done for mod_duty_hour_calendar/_confirm
# (2026-09-16, commit f855751).
#
# Owns the shared reactives (entries_r, amion_blocks_r, refresh,
# selected_date) that mod_duty_hour_calendar and mod_duty_hour_confirm
# both need and must stay in sync on — a save in the confirm form bumps
# `refresh`, which both entries_r() and the calendar's coloring depend on;
# a calendar click sets `selected_date`, which the confirm form reads to
# decide what to show. Composes:
#   - mod_duty_hour_calendar : month grid, click a day to select it
#   - mod_duty_hour_confirm  : the edit form for the selected (or oldest
#     unconfirmed) day, writes to duty_hour_log
#   - mod_duty_hour_summary  : the read-only weekly chart/flags (Amion
#     defaults merged with whatever's been saved)
#
# Also threads amiontools::use_amion_data() through all three children so
# there's exactly one crosswalk+Amion fetch per page load, not three —
# found live 2026-09-15 (Fred: slow load, "doom loop" in the console)
# before this sharing was wired in.

#' @importFrom shiny NS moduleServer reactiveVal reactive req tagList tags
#' @importFrom dplyr filter
NULL

#' Duty Hours page (calendar + confirm form + summary chart)
#'
#' Full-page UI: intro copy, the month calendar, the confirm/edit form for
#' the selected day, and the read-only weekly summary chart underneath.
#'
#' @param id Shiny module id.
#' @export
mod_duty_hour_page_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      class = "small text-muted", style = "margin-bottom: 16px; max-width: 720px;",
      shiny::tags$p(style = "margin-bottom: 4px;",
        shiny::tags$strong("What this page does: "),
        "Your calendar and schedule below are pre-filled from Amion — your program's schedule ",
        "isn't a record of what you actually worked, just what you were assigned. Click any past ",
        "or current day to confirm it's accurate, or correct it if it isn't. Add moonlighting and ",
        "at-home chart-review time too — both count toward your duty hours."
      ),
      shiny::tags$p(style = "margin-bottom: 0;",
        "Accurate reporting matters: it's how the program and GME office track ACGME compliance, ",
        "and how excessive-hours patterns get caught before they become a problem for you."
      )
    ),
    mod_duty_hour_calendar_ui(ns("calendar")),
    shiny::tags$hr(style = "margin: 20px 0;"),
    mod_duty_hour_confirm_ui(ns("confirm")),
    shiny::tags$hr(style = "margin: 24px 0;"),
    mod_duty_hour_summary_ui(ns("summary"))
  )
}

#' Duty Hours page server
#'
#' @param id Shiny module id — must match \code{mod_duty_hour_page_ui}'s.
#' @param resident_id Reactive returning the current resident's record_id.
#' @param rdm_token RDM REDCap API token.
#' @param redcap_url REDCap base URL.
#' @export
mod_duty_hour_page_server <- function(id, resident_id, rdm_token, redcap_url) {
  shiny::moduleServer(id, function(input, output, session) {
    refresh <- shiny::reactiveVal(0)
    selected_date <- shiny::reactiveVal(NULL)

    shared <- use_amion_data(rdm_token = rdm_token, redcap_url = redcap_url)

    entries_r <- shiny::reactive({
      refresh()
      shiny::req(resident_id())
      pull_duty_hour_log(rdm_token, redcap_url, record_id = resident_id())
    })

    # Amion-only defaults (entries = data.frame() skips the overlay) — both
    # children need the RAW defaults (to find gaps / color cells), not the
    # already-merged view build_duty_hour_summary() normally returns.
    amion_blocks_r <- shiny::reactive({
      shiny::req(resident_id())
      summ <- build_duty_hour_summary(
        rdm_token = rdm_token, redcap_url = redcap_url,
        crosswalk = shared$crosswalk(), amion = shared$amion(),
        entries = data.frame()
      )
      summ$duty_blocks |> dplyr::filter(record_id == resident_id())
    })

    mod_duty_hour_calendar_server("calendar", resident_id = resident_id,
                                  entries_r = entries_r, amion_blocks_r = amion_blocks_r,
                                  selected_date = selected_date)
    mod_duty_hour_confirm_server("confirm", resident_id = resident_id,
                                 entries_r = entries_r, amion_blocks_r = amion_blocks_r,
                                 refresh = refresh, selected_date = selected_date,
                                 redcap_url = redcap_url, rdm_token = rdm_token)
    mod_duty_hour_summary_server(
      "summary",
      resident_id = resident_id,
      rdm_token   = rdm_token,
      redcap_url  = redcap_url,
      crosswalk_r = shared$crosswalk,
      amion_r     = shared$amion,
      entries_r   = entries_r
    )
  })
}

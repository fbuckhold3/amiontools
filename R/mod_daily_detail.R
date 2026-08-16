# =============================================================================
# Shiny module: per-resident day-by-day detail log.
#
# Reconstructs the Amion calendar view in table form — rotation, team
# assignment, off/on-call status, clinic sessions per date — so the
# aggregate numbers elsewhere in the Schedule tab can be checked against
# the real record. Requested by Fred 2026-08-17.
#
# Unlike the other 3 Schedule sections, this is a genuinely long table (up
# to ~365 rows/resident/AY), not a ~10-20 row comparison — uses a
# searchable+paginated DT rather than the "dom = 't'" compact style.
# =============================================================================

#' @importFrom shiny NS moduleServer reactive req validate need renderUI uiOutput tagList h5 p
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT renderDT DTOutput datatable
#' @importFrom dplyr filter select
NULL

#' @rdname mod_daily_detail
#' @export
mod_daily_detail_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("header")),
    shinycssloaders::withSpinner(DT::DTOutput(ns("table")), type = 6, color = "#2a78d6", size = 0.5)
  )
}

#' Daily detail log module — UI + server.
#'
#' @param id Module namespace id.
#' @param resident_id Reactive returning the currently-selected RDM
#'   record_id (character/numeric coercible).
#' @param rdm_token RDM REDCap API token (test or prod — caller's choice).
#' @param redcap_url REDCap API URL.
#' @param amion_lo Amion Lo= program token; defaults to AMION_LO_DEFAULT.
#' @param crosswalk_r,amion_r Optional reactives (e.g. from
#'   use_amion_data()) returning pre-fetched crosswalk/Amion data — pass
#'   these when composing this module alongside others in one session to
#'   fetch once instead of each module independently re-fetching. NULL
#'   (default): fetches its own data, same as before.
#' @name mod_daily_detail
#' @export
mod_daily_detail_server <- function(id, resident_id, rdm_token, redcap_url,
                                    amion_lo = AMION_LO_DEFAULT,
                                    crosswalk_r = NULL,
                                    amion_r = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    daily_data <- shiny::reactive({
      build_daily_detail(
        rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
        crosswalk = if (!is.null(crosswalk_r)) crosswalk_r() else NULL,
        amion     = if (!is.null(amion_r)) amion_r() else NULL
      )
    })

    resident_days <- shiny::reactive({
      shiny::req(resident_id())
      daily_data() |> dplyr::filter(record_id == resident_id())
    })

    display_table <- shiny::reactive({
      shiny::req(daily_data())
      shiny::validate(
        shiny::need(nrow(resident_days()) > 0,
                    "No Amion schedule data available for this resident.")
      )
      resident_days() |>
        dplyr::select(Date, Rotation, Team_Assignment, Off_Status, Call_Status,
                      Clinic_Sessions, Other_Assignment)
    })

    output$header <- shiny::renderUI({
      shiny::req(nrow(resident_days()) > 0)
      shiny::tagList(
        shiny::h5(paste0("Daily Detail — ", resident_days()$Level[1], " class")),
        shiny::p(class = "text-muted small",
                 "Day-by-day rotation, team, status, and clinic detail, current academic year — search by date or rotation to check specific days.")
      )
    })

    output$table <- DT::renderDT({
      DT::datatable(
        display_table(),
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 15, order = list(list(0, "desc")),
                       dom = "lfrtip")
      )
    })
  })
}

# =============================================================================
# Shiny module: one resident's rotation days broken out by training year
# actually lived (Intern/PGY2/PGY3), plus a Total. Forward-only -- see
# build_rotation_by_training_year()'s file header for why a training year
# can show as "—" (not available) rather than a number.
#
# Deliberately NOT wired through the REDCap cache yet -- brand new,
# unvalidated by Fred beyond one spot-check. Live fetch only until it earns
# the same trust the other 3 aggregate sections have. Not composed with
# use_amion_data()'s shared crosswalk_r/amion_r either, since
# build_rotation_by_training_year() loops per-AY internally and needs its
# own raw type/grad_yr pull for the Level-as-of-each-AY computation.
# =============================================================================

#' @importFrom shiny NS moduleServer reactive req validate need renderUI uiOutput tagList h5 p
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT renderDT DTOutput datatable
#' @importFrom dplyr filter select any_of arrange
NULL

#' @rdname mod_rotation_by_year
#' @export
mod_rotation_by_year_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("header")),
    shinycssloaders::withSpinner(DT::DTOutput(ns("table")), type = 6, color = "#2a78d6", size = 0.5)
  )
}

#' Rotation-by-training-year module — UI + server.
#'
#' @param id Module namespace id.
#' @param resident_id Reactive returning the currently-selected RDM
#'   record_id (character/numeric coercible).
#' @param rdm_token RDM REDCap API token (test or prod — caller's choice).
#' @param redcap_url REDCap API URL.
#' @param amion_lo Amion Lo= program token; defaults to AMION_LO_DEFAULT.
#' @name mod_rotation_by_year
#' @export
mod_rotation_by_year_server <- function(id, resident_id, rdm_token, redcap_url,
                                        amion_lo = AMION_LO_DEFAULT) {
  shiny::moduleServer(id, function(input, output, session) {

    by_year_data <- shiny::reactive({
      build_rotation_by_training_year(rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo)
    })

    resident_rows <- shiny::reactive({
      shiny::req(resident_id())
      by_year_data() |>
        dplyr::filter(record_id == resident_id(), Total > 0) |>
        dplyr::arrange(dplyr::desc(Total)) |>
        dplyr::select(dplyr::any_of(c("Category", "Intern", "PGY2", "PGY3", "Total")))
    })

    output$header <- shiny::renderUI({
      shiny::req(by_year_data())
      shiny::validate(
        shiny::need(nrow(resident_rows()) > 0,
                    "No Amion schedule data available for this resident.")
      )
      shiny::tagList(
        shiny::h5("Rotation Days — By Training Year"),
        shiny::p(class = "text-muted small",
                 "Days by rotation category for each training year actually lived, since the ",
                 "schedule was rebuilt for AY2026-27. A blank cell means that training year ",
                 "isn't covered by post-rebuild data yet (not zero days) — fills in over the ",
                 "next couple of years as each class progresses.")
      )
    })

    output$table <- DT::renderDT({
      shiny::req(nrow(resident_rows()) > 0)
      DT::datatable(
        resident_rows(),
        rownames = FALSE,
        options = list(pageLength = 20, dom = "t"),
      ) |>
        DT::formatRound(columns = c("Intern", "PGY2", "PGY3", "Total"), digits = 1)
    })
  })
}

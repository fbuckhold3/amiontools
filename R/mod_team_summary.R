# =============================================================================
# Shiny module: per-resident team-assignment days vs. class average.
#
# Drill-down layer beneath mod_rotation_summary's category view (e.g. "SLU
# Floors: 12 days" -> here: "8 on Green, 4 on Yellow"). Meant to be composed
# alongside mod_rotation_summary in the same tab, not as a standalone nav
# entry — see amion_integration project notes 2026-08-15.
#
# Uses class-matched averages (build_team_summary()'s class_avg_wide), not
# the flat program-wide average — comparing an intern's 0 Bronze/Cardiology
# days against a program-wide average that's dominated by seniors would be
# misleading. Program-wide averages are for the future cross-resident
# dashboard, not this per-resident view.
# =============================================================================

#' @importFrom shiny NS moduleServer reactive req validate need renderUI uiOutput tagList h5 p
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT renderDT DTOutput datatable formatRound
#' @importFrom dplyr filter select any_of
#' @importFrom tidyr pivot_longer
NULL

#' @rdname mod_team_summary
#' @export
mod_team_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("header")),
    shinycssloaders::withSpinner(DT::DTOutput(ns("table")), type = 6, color = "#2a78d6", size = 0.5)
  )
}

#' Team-assignment summary module — UI + server.
#'
#' @param id Module namespace id.
#' @param resident_id Reactive returning the currently-selected RDM
#'   record_id (character/numeric coercible).
#' @param rdm_token RDM REDCap API token (test or prod — caller's choice).
#' @param redcap_url REDCap API URL.
#' @param amion_lo Amion Lo= program token; defaults to AMION_LO_DEFAULT.
#' @name mod_team_summary
#' @export
mod_team_summary_server <- function(id, resident_id, rdm_token, redcap_url,
                                    amion_lo = AMION_LO_DEFAULT) {
  shiny::moduleServer(id, function(input, output, session) {

    team_data <- shiny::reactive({
      build_team_summary(rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo)
    })

    resident_row <- shiny::reactive({
      shiny::req(resident_id())
      team_data()$team_summary_wide |>
        dplyr::filter(record_id == resident_id())
    })

    comparison_table <- shiny::reactive({
      shiny::req(team_data())
      shiny::validate(
        shiny::need(nrow(resident_row()) > 0,
                    "No Amion schedule data available for this resident.")
      )

      res_row  <- resident_row()
      level    <- res_row$Level[1]
      class_row <- team_data()$class_avg_wide |>
        dplyr::filter(Level == level)

      team_cols <- setdiff(names(res_row), c("record_id", "name", "Level"))

      res_long <- res_row |>
        dplyr::select(dplyr::any_of(team_cols)) |>
        tidyr::pivot_longer(dplyr::everything(), names_to = "Team", values_to = "You")

      class_long <- class_row |>
        dplyr::select(dplyr::any_of(team_cols)) |>
        tidyr::pivot_longer(dplyr::everything(), names_to = "Team",
                            values_to = "Class Average")

      merged <- merge(res_long, class_long, by = "Team", all = TRUE)
      # Drop teams this resident and their whole class both show zero on —
      # e.g. an intern's row full of Bronze/Cardiology/Diamond/Gold zeros —
      # keeps the table to teams actually relevant to this resident's class.
      merged <- merged[!(merged$You == 0 & merged$`Class Average` == 0), ]
      merged[order(-merged$You), ]
    })

    output$header <- shiny::renderUI({
      shiny::req(nrow(resident_row()) > 0)
      shiny::tagList(
        shiny::h5(paste0("Team Assignments — ", resident_row()$Level[1], " class")),
        shiny::p(class = "text-muted small",
                 "Days on each specific team (e.g. Green, MICU 1, VA Floors C), current academic year, vs. this resident's class average.")
      )
    })

    output$table <- DT::renderDT({
      DT::datatable(
        comparison_table(),
        rownames = FALSE,
        options = list(pageLength = 20, dom = "t"),
      ) |>
        DT::formatRound(columns = c("You", "Class Average"), digits = 1)
    })
  })
}

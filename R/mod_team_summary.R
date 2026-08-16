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
#' @importFrom dplyr filter select any_of bind_rows
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
#' @param crosswalk_r,amion_r Optional reactives (e.g. from
#'   use_amion_data()) returning pre-fetched crosswalk/Amion data — pass
#'   these when composing this module alongside others in one session to
#'   fetch once instead of each module independently re-fetching. NULL
#'   (default): fetches its own data, same as before.
#' @name mod_team_summary
#' @export
mod_team_summary_server <- function(id, resident_id, rdm_token, redcap_url,
                                    amion_lo = AMION_LO_DEFAULT,
                                    crosswalk_r = NULL,
                                    amion_r = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    team_data <- shiny::reactive({
      build_team_summary(
        rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
        crosswalk = if (!is.null(crosswalk_r)) crosswalk_r() else NULL,
        amion     = if (!is.null(amion_r)) amion_r() else NULL
      )
    })

    resident_row <- shiny::reactive({
      shiny::req(resident_id())
      team_data()$team_summary_wide |>
        dplyr::filter(record_id == resident_id())
    })

    # Builds a You/Class-Average long table from one wide/class_avg_wide
    # pair (on-duty or off-duty), optionally suffixing team names (e.g.
    # "MICU" -> "MICU (Off)") so both can be combined into one table.
    .build_long <- function(wide, class_wide, level, suffix = "") {
      res_row <- wide |> dplyr::filter(record_id == resident_id())
      if (nrow(res_row) == 0) return(NULL)

      class_row <- class_wide |> dplyr::filter(Level == level)
      team_cols <- setdiff(names(res_row), c("record_id", "name", "Level"))
      if (length(team_cols) == 0) return(NULL)

      res_long <- res_row |>
        dplyr::select(dplyr::any_of(team_cols)) |>
        tidyr::pivot_longer(dplyr::everything(), names_to = "Team", values_to = "You")

      class_long <- class_row |>
        dplyr::select(dplyr::any_of(team_cols)) |>
        tidyr::pivot_longer(dplyr::everything(), names_to = "Team",
                            values_to = "Class Average")

      merged <- merge(res_long, class_long, by = "Team", all = TRUE)
      merged$Team <- paste0(merged$Team, suffix)
      merged
    }

    comparison_table <- shiny::reactive({
      shiny::req(team_data())
      shiny::validate(
        shiny::need(nrow(resident_row()) > 0,
                    "No Amion schedule data available for this resident.")
      )

      level <- resident_row()$Level[1]

      on_duty_long  <- .build_long(team_data()$team_summary_wide, team_data()$class_avg_wide, level)
      # off_summary_wide is now a single "Off" column (all status_off
      # entries collapsed together, not suffixed per-team) - no suffix needed.
      off_duty_long <- .build_long(team_data()$off_summary_wide, team_data()$off_class_avg_wide, level)

      merged <- dplyr::bind_rows(on_duty_long, off_duty_long)
      # Drop rows this resident and their whole class both show zero on —
      # e.g. an intern's row full of Bronze/Cardiology/Diamond/Gold zeros —
      # keeps the table to teams/off-buckets actually relevant to this
      # resident's class.
      merged <- merged[!(merged$You == 0 & merged$`Class Average` == 0), ]
      merged[order(-merged$You), ]
    })

    output$header <- shiny::renderUI({
      shiny::req(nrow(resident_row()) > 0)
      shiny::tagList(
        shiny::h5(paste0("Team Assignments — ", resident_row()$Level[1], " class")),
        shiny::p(class = "text-muted small",
                 "Days on each specific team (e.g. Green, MICU 1, VA Floors C) plus total days off, current academic year, vs. this resident's class average.")
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

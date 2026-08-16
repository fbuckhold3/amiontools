# =============================================================================
# Shiny module: per-resident rotation-day summary vs. class average.
#
# Meant to be dropped into any resident-scoped dashboard (ind.dash first;
# ccc.dashboard/coach.dash are plausible future consumers, which is why this
# lives in amiontools rather than being written directly into ind.dash).
#
# Pulls build_rotation_summary() ONCE per session (both RDM and Amion calls
# are non-trivial round-trips) and filters down to whichever resident_id is
# selected, rather than re-fetching per resident.
# =============================================================================

#' @importFrom shiny NS moduleServer reactive req validate need renderUI uiOutput tagList h5 p
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT renderDT DTOutput datatable
#' @importFrom dplyr filter select any_of
#' @importFrom tidyr pivot_longer
NULL

#' @rdname mod_rotation_summary
#' @export
mod_rotation_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("header")),
    shinycssloaders::withSpinner(DT::DTOutput(ns("table")), type = 6, color = "#2a78d6", size = 0.5)
  )
}

#' Rotation summary module — UI + server.
#'
#' @param id Module namespace id.
#' @param resident_id Reactive returning the currently-selected RDM
#'   record_id (character/numeric coercible).
#' @param rdm_token RDM REDCap API token (test or prod — caller's choice).
#' @param redcap_url REDCap API URL.
#' @param amion_lo Amion Lo= program token; defaults to AMION_LO_DEFAULT.
#' @param since_change Logical. If TRUE, use
#'   build_rotation_summary_since_change() (AY2026-27 -> present) instead of
#'   the current-AY-only build_rotation_summary(). Default FALSE (current AY
#'   only) — matches the near-term "this year vs this year's class" ask;
#'   flip once a genuine multi-year view is wanted (see the class_avg
#'   grouping caveat documented on build_rotation_summary()).
#' @name mod_rotation_summary
#' @export
mod_rotation_summary_server <- function(id, resident_id, rdm_token, redcap_url,
                                        amion_lo = AMION_LO_DEFAULT,
                                        since_change = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {

    rotation_data <- shiny::reactive({
      fn <- if (isTRUE(since_change)) build_rotation_summary_since_change else build_rotation_summary
      fn(rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo)
    })

    resident_row <- shiny::reactive({
      shiny::req(resident_id())
      rotation_data()$summary_wide |>
        dplyr::filter(record_id == resident_id())
    })

    comparison_table <- shiny::reactive({
      shiny::req(rotation_data())
      shiny::validate(
        shiny::need(nrow(resident_row()) > 0,
                    "No Amion schedule data available for this resident.")
      )

      res_row  <- resident_row()
      level    <- res_row$Level[1]
      class_row <- rotation_data()$class_avg_wide |>
        dplyr::filter(Level == level)

      category_cols <- setdiff(names(res_row), c("record_id", "name", "Level"))

      res_long <- res_row |>
        dplyr::select(dplyr::any_of(category_cols)) |>
        tidyr::pivot_longer(dplyr::everything(), names_to = "Category", values_to = "You")

      class_long <- class_row |>
        dplyr::select(dplyr::any_of(category_cols)) |>
        tidyr::pivot_longer(dplyr::everything(), names_to = "Category",
                            values_to = "Class Average")

      merge(res_long, class_long, by = "Category", all = TRUE) |>
        (\(df) df[order(-df$You), ])()
    })

    output$header <- shiny::renderUI({
      shiny::req(nrow(resident_row()) > 0)
      shiny::tagList(
        shiny::h5(paste0("Rotation Days — ", resident_row()$Level[1], " class")),
        shiny::p(class = "text-muted small",
                 "Days by rotation category, current academic year, vs. the average for this resident's class.")
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

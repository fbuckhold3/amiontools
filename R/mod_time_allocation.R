# =============================================================================
# Shiny module: resident time allocation by super-category, visualized.
#
# The "pop"/dynamic view Fred asked for 2026-08-16 — sits alongside
# mod_rotation_summary (category tables) and mod_team_summary (team tables)
# as the third section of the same Schedule tab. Unlike those two, this one
# is chart-first: a horizontal grouped bar (You vs Class Average per
# super-category), plus a compact data table underneath per the dataviz
# skill's accessibility requirement (a non-chart fallback must exist).
#
# Chart library: plotly. Deliberately NOT a new dependency — already loaded
# elsewhere in this exact app (gmed pulls it in). Color: the dataviz skill's
# validated default categorical slots 1 (blue, #2a78d6) and 2 (orange,
# #eb6834) for the two series — validated via scripts/validate_palette.js
# (CVD ΔE 24.7, normal-vision ΔE 33.6, both clear of the floor). Only 2
# series need distinct color here (You / Class Average) — the 7
# super-categories are bar GROUPS, not color slots.
# =============================================================================

#' @importFrom shiny NS moduleServer reactive req validate need renderUI uiOutput tagList h5 p div HTML tags
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plot_ly add_trace layout renderPlotly plotlyOutput config
#' @importFrom DT renderDT DTOutput datatable formatRound
#' @importFrom dplyr filter select any_of arrange
#' @importFrom tidyr pivot_longer
NULL

.TIME_ALLOC_YOU_COLOR   <- "#2a78d6"
.TIME_ALLOC_CLASS_COLOR <- "#eb6834"

#' @rdname mod_time_allocation
#' @export
mod_time_allocation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("header")),
    shiny::tags$div(
      class = "amiontools-chart-pop-in",
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(ns("chart"), height = "420px"),
        type = 6, color = .TIME_ALLOC_YOU_COLOR, size = 0.6
      )
    ),
    shiny::tags$details(
      style = "margin-top: 12px;",
      shiny::tags$summary(style = "cursor: pointer; color: var(--ssm-text-muted, #6b7d82); font-size: 0.85rem;",
                          "View as table"),
      DT::DTOutput(ns("table"))
    ),
    shiny::tags$style(shiny::HTML(sprintf(
      "@keyframes amiontoolsChartPopIn { from { opacity: 0; transform: scale(0.96); } to { opacity: 1; transform: scale(1); } }
       .amiontools-chart-pop-in { animation: amiontoolsChartPopIn 0.45s ease-out; }"
    )))
  )
}

#' Time-allocation module — UI + server.
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
#'   fetch once instead of each module independently re-fetching (this
#'   module in particular used to trigger its OWN extra internal re-fetch
#'   via build_rotation_summary() — see time_allocation_summary.R). NULL
#'   (default): fetches its own data, same as before.
#' @name mod_time_allocation
#' @export
mod_time_allocation_server <- function(id, resident_id, rdm_token, redcap_url,
                                       amion_lo = AMION_LO_DEFAULT,
                                       crosswalk_r = NULL,
                                       amion_r = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    alloc_data <- shiny::reactive({
      build_time_allocation_summary(
        rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
        crosswalk = if (!is.null(crosswalk_r)) crosswalk_r() else NULL,
        amion     = if (!is.null(amion_r)) amion_r() else NULL
      )
    })

    resident_row <- shiny::reactive({
      shiny::req(resident_id())
      alloc_data()$summary_wide |>
        dplyr::filter(record_id == resident_id())
    })

    comparison_table <- shiny::reactive({
      shiny::req(alloc_data())
      shiny::validate(
        shiny::need(nrow(resident_row()) > 0,
                    "No Amion schedule data available for this resident.")
      )

      res_row   <- resident_row()
      level     <- res_row$Level[1]
      class_row <- alloc_data()$class_avg_wide |>
        dplyr::filter(Level == level)

      super_cols <- setdiff(names(res_row), c("record_id", "name", "Level"))

      res_long <- res_row |>
        dplyr::select(dplyr::any_of(super_cols)) |>
        tidyr::pivot_longer(dplyr::everything(), names_to = "Category", values_to = "You")

      class_long <- class_row |>
        dplyr::select(dplyr::any_of(super_cols)) |>
        tidyr::pivot_longer(dplyr::everything(), names_to = "Category",
                            values_to = "Class Average")

      merged <- merge(res_long, class_long, by = "Category", all = TRUE)
      merged[order(-merged$You), ]
    })

    output$header <- shiny::renderUI({
      shiny::req(nrow(resident_row()) > 0)
      shiny::tagList(
        shiny::h5(paste0("Time Allocation — ", resident_row()$Level[1], " class")),
        shiny::p(class = "text-muted small",
                 "How this resident's time breaks down (current academic year) vs. this resident's class average.")
      )
    })

    output$chart <- plotly::renderPlotly({
      tbl <- comparison_table()
      # Preserve the You-descending order on the y-axis (plotly renders
      # bottom-up by default, so reverse for top = highest).
      cats_ordered <- rev(tbl$Category)

      plotly::plot_ly(
        data = tbl,
        y = ~factor(Category, levels = cats_ordered),
        x = ~You, type = "bar", orientation = "h", name = "You",
        marker = list(color = .TIME_ALLOC_YOU_COLOR),
        text = ~sprintf("%.1f days", You), textposition = "outside",
        hovertemplate = "%{y}<br>You: %{x:.1f} days<extra></extra>"
      ) |>
        plotly::add_trace(
          x = ~`Class Average`, name = "Class Average",
          marker = list(color = .TIME_ALLOC_CLASS_COLOR),
          text = ~sprintf("%.1f days", `Class Average`), textposition = "outside",
          hovertemplate = "%{y}<br>Class Average: %{x:.1f} days<extra></extra>"
        ) |>
        plotly::layout(
          barmode = "group",
          xaxis = list(title = "Days", zeroline = FALSE, gridcolor = "#e9eff0"),
          yaxis = list(title = ""),
          legend = list(orientation = "h", x = 0, y = 1.08),
          margin = list(l = 140),
          plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "inherit")
        ) |>
        plotly::config(displaylogo = FALSE, displayModeBar = FALSE)
    })

    output$table <- DT::renderDT({
      DT::datatable(
        comparison_table(),
        rownames = FALSE,
        options = list(pageLength = 10, dom = "t"),
      ) |>
        DT::formatRound(columns = c("You", "Class Average"), digits = 1)
    })
  })
}

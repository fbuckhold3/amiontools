# =============================================================================
# Shiny module: duty-hour summary chart — read-only display (the actual
# editing happens in imslu.ind.dash's mod_duty_hour_confirm, which writes
# to RDM's duty_hour_log; this module just reflects whatever
# build_duty_hour_summary() computes, Amion defaults merged with any saved
# resident entries). Weekly hours chart with a 4-week rolling-average line
# and an 80h reference line, plus the rest-gap and low-days flags surfaced
# as plain text. Nothing here is enforcement — it's a summary view.
# =============================================================================

#' @importFrom shiny NS moduleServer reactive req validate need renderUI uiOutput tagList h5 p div tags
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plot_ly add_trace layout renderPlotly plotlyOutput config
#' @importFrom DT renderDT DTOutput datatable formatRound
#' @importFrom dplyr filter arrange select mutate
NULL

.DUTY_HOURS_COLOR        <- "#2a78d6"
.DUTY_HOURS_FUTURE_COLOR <- "#a9c7e8"
.DUTY_HOURS_AVG_COLOR    <- "#eb6834"
.DUTY_HOURS_LIMIT_COLOR  <- "#c0392b"
.DUTY_HOURS_TODAY_COLOR  <- "#6b7d82"

#' @rdname mod_duty_hour_summary
#' @export
mod_duty_hour_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("header")),
    shiny::tags$div(
      style = "margin-bottom: 8px;",
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(ns("chart"), height = "380px"),
        type = 6, color = .DUTY_HOURS_COLOR, size = 0.6
      )
    ),
    shiny::uiOutput(ns("flags")),
    shiny::tags$details(
      style = "margin-top: 16px;",
      shiny::tags$summary(style = "cursor: pointer; color: var(--ssm-text-muted, #6b7d82); font-size: 0.85rem;",
                          "View weekly table"),
      DT::DTOutput(ns("weekly_table"))
    )
  )
}

#' Duty-hour summary module — UI + server. Phase 1: read-only.
#'
#' @param id Module namespace id.
#' @param resident_id Reactive returning the currently-selected RDM
#'   record_id (character/numeric coercible).
#' @param rdm_token RDM REDCap API token (test or prod — caller's choice).
#' @param redcap_url REDCap API URL.
#' @param amion_lo Amion Lo= program token; defaults to AMION_LO_DEFAULT.
#' @param crosswalk_r,amion_r Optional reactives (e.g. from
#'   use_amion_data()) returning pre-fetched crosswalk/Amion data — pass
#'   these when composing this module alongside the other Schedule-tab
#'   modules to fetch once instead of re-fetching. NULL (default): fetches
#'   its own data.
#' @param entries_r Optional reactive returning a pre-fetched
#'   pull_duty_hour_log() result — pass this when composing alongside
#'   another module that already fetched it (e.g. the confirm-flow's own
#'   entries_r) to skip this module's internal unrestricted (all-resident)
#'   pull_duty_hour_log() call. A single-resident-scoped entries_r works
#'   fine here even though build_duty_hour_summary() normally expects
#'   program-wide entries — this module filters its result down to
#'   resident_id() anyway, so the narrower input is equivalent. NULL
#'   (default): fetches its own (all-resident) entries, unchanged from
#'   before this param existed.
#' @name mod_duty_hour_summary
#' @export
mod_duty_hour_summary_server <- function(id, resident_id, rdm_token, redcap_url,
                                         amion_lo = AMION_LO_DEFAULT,
                                         crosswalk_r = NULL,
                                         amion_r = NULL,
                                         entries_r = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    duty_data <- shiny::reactive({
      build_duty_hour_summary(
        rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
        crosswalk = if (!is.null(crosswalk_r)) crosswalk_r() else NULL,
        amion     = if (!is.null(amion_r)) amion_r() else NULL,
        entries   = if (!is.null(entries_r)) entries_r() else NULL
      )
    })

    resident_weekly <- shiny::reactive({
      shiny::req(resident_id())
      duty_data()$weekly |> dplyr::filter(record_id == resident_id()) |> dplyr::arrange(week_start)
    })

    resident_gaps <- shiny::reactive({
      shiny::req(resident_id())
      duty_data()$rest_gap_flags |> dplyr::filter(record_id == resident_id())
    })

    output$header <- shiny::renderUI({
      shiny::req(nrow(resident_weekly()) > 0)
      shiny::tagList(
        shiny::h5(paste0("Duty Hours — ", resident_weekly()$Level[1], " class")),
        shiny::p(class = "text-muted small",
                 "Estimated hours from your Amion schedule (current academic year). Confirmed/edited days, plus any moonlighting or at-home chart-review hours you've logged, are reflected once saved via the confirm flow above."),
        shiny::p(class = "text-muted small",
                 shiny::tags$strong("Weeks after today (lighter bars) are your upcoming scheduled hours"),
                 " — hours you're expected to work based on your posted schedule, not hours you've actually worked yet.")
      )
    })

    output$chart <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(nrow(resident_weekly()) > 0,
                    "No Amion schedule data available for this resident.")
      )
      # Amion pre-builds the whole academic year, so `wk` routinely extends
      # months past today — split into two bar traces (past/current vs
      # future) so it reads as "hours worked" vs "hours scheduled," not one
      # undifferentiated bar series. Split on week_END (week_start + 6d) <
      # today, so a week that's only partly elapsed still counts as past/
      # current rather than future.
      wk <- resident_weekly() |>
        dplyr::mutate(period = ifelse(week_start + 6 < Sys.Date(), "past", "future"))
      wk_past   <- wk |> dplyr::filter(period == "past")
      wk_future <- wk |> dplyr::filter(period == "future")

      p <- plotly::plot_ly()
      if (nrow(wk_past) > 0) {
        p <- p |> plotly::add_trace(
          data = wk_past, x = ~week_start, y = ~Total_Hours, type = "bar", name = "Hours worked",
          marker = list(color = .DUTY_HOURS_COLOR),
          hovertemplate = "Week of %{x}<br>%{y:.1f} hours worked<extra></extra>"
        )
      }
      if (nrow(wk_future) > 0) {
        p <- p |> plotly::add_trace(
          data = wk_future, x = ~week_start, y = ~Total_Hours, type = "bar", name = "Hours scheduled (upcoming)",
          marker = list(color = .DUTY_HOURS_FUTURE_COLOR),
          hovertemplate = "Week of %{x}<br>%{y:.1f} hours scheduled<extra></extra>"
        )
      }

      today_shape <- if (nrow(wk_future) > 0 || nrow(wk_past) > 0) {
        list(type = "line", x0 = Sys.Date(), x1 = Sys.Date(), xref = "x",
             y0 = 0, y1 = 1, yref = "paper",
             line = list(color = .DUTY_HOURS_TODAY_COLOR, dash = "dot", width = 1.5))
      }
      today_annotation <- if (nrow(wk_future) > 0 || nrow(wk_past) > 0) {
        list(x = Sys.Date(), xref = "x", y = 1, yref = "paper", yanchor = "bottom",
             text = "Today", showarrow = FALSE, font = list(color = .DUTY_HOURS_TODAY_COLOR, size = 11))
      }

      p |>
        plotly::add_trace(
          data = wk, x = ~week_start, y = ~rolling_4wk_avg_hours, type = "scatter", mode = "lines+markers",
          name = "4-week rolling avg", line = list(color = .DUTY_HOURS_AVG_COLOR, width = 2),
          hovertemplate = "Week of %{x}<br>4wk avg: %{y:.1f} hours<extra></extra>"
        ) |>
        plotly::layout(
          xaxis = list(title = "", gridcolor = "#e9eff0"),
          yaxis = list(title = "Hours", zeroline = FALSE, gridcolor = "#e9eff0"),
          shapes = list(
            list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                 y0 = 80, y1 = 80, yref = "y",
                 line = list(color = .DUTY_HOURS_LIMIT_COLOR, dash = "dash", width = 1.5)),
            today_shape
          ),
          annotations = list(
            list(x = 1, xref = "paper", y = 80, yref = "y", xanchor = "right", yanchor = "bottom",
                 text = "80h reference", showarrow = FALSE,
                 font = list(color = .DUTY_HOURS_LIMIT_COLOR, size = 11)),
            today_annotation
          ),
          barmode = "overlay",
          legend = list(orientation = "h", x = 0, y = 1.15),
          plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "inherit")
        ) |>
        plotly::config(displaylogo = FALSE, displayModeBar = FALSE)
    })

    output$flags <- shiny::renderUI({
      shiny::req(nrow(resident_weekly()) > 0)
      wk     <- resident_weekly()
      latest <- wk[nrow(wk), ]
      gaps   <- resident_gaps()

      items <- list(
        shiny::p(
          class = "small",
          sprintf("Current 4-week average: %.1f hours/week. Cumulative days worked/week (excl. vacation): %.1f (target 5.5).",
                 latest$rolling_4wk_avg_hours, latest$rolling_days_per_week)
        )
      )

      if (isTRUE(latest$flag_80h)) {
        items[[length(items) + 1]] <- shiny::div(
          class = "small", style = "color:#c0392b; font-weight:600;",
          "⚠ 4-week rolling average is over 80 hours/week."
        )
      }
      if (isTRUE(latest$flag_low_days)) {
        items[[length(items) + 1]] <- shiny::div(
          class = "small", style = "color:#b8860b;",
          "⚠ Average days worked/week is below the 5.5 target."
        )
      }
      if (nrow(gaps) > 0) {
        hard_n <- sum(gaps$severity == "hard")
        soft_n <- sum(gaps$severity == "soft")
        items[[length(items) + 1]] <- shiny::div(
          class = "small", style = "color:#c0392b;",
          sprintf("⚠ %d rest-gap flag(s) this year (%d hard, %d soft) — see table below.",
                 nrow(gaps), hard_n, soft_n)
        )
      }

      shiny::tagList(items)
    })

    output$weekly_table <- DT::renderDT({
      shiny::req(nrow(resident_weekly()) > 0)
      tbl <- resident_weekly() |>
        dplyr::mutate(Period = ifelse(week_start + 6 < Sys.Date(), "Worked", "Scheduled")) |>
        dplyr::select(week_start, Period, Total_Hours, Days_Worked, rolling_4wk_avg_hours,
                      flag_80h, rolling_days_per_week, flag_low_days) |>
        dplyr::arrange(dplyr::desc(week_start))
      DT::datatable(
        tbl, rownames = FALSE,
        colnames = c("Week of", "Period", "Hours", "Days Worked", "4wk Avg Hours",
                    "80h Flag", "Days/wk Avg", "Low-Days Flag"),
        options = list(pageLength = 10, order = list(list(0, "desc")))
      ) |>
        DT::formatRound(columns = c("Total_Hours", "rolling_4wk_avg_hours", "rolling_days_per_week"), digits = 1)
    })
  })
}

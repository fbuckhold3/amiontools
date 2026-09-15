# =============================================================================
# Shiny module: duty-hour summary chart — read-only display (the actual
# editing happens in imslu.ind.dash's mod_duty_hour_confirm, which writes
# to RDM's duty_hour_log; this module just reflects whatever
# build_duty_hour_summary() computes, Amion defaults merged with any saved
# resident entries). Stacked weekly-hours chart by rotation super-category
# (DUTY_HOUR_CATEGORY_COLORS), each category split into a solid "verified"
# segment and a lighter "anticipated" segment — verified = the resident has
# actually confirmed/entered that day, anticipated = still just an Amion
# default, REGARDLESS of whether the date is past or future (Fred
# 2026-09-15: this doubles as the verified/not-verified indicator he asked
# for, not a separate encoding). Plus a 4-week rolling-average line, an
# 80h reference line, and the rest-gap/low-days flags as plain text.
# Nothing here is enforcement — it's a summary view.
#
# Both the chart and the weekly table are padded to the full academic year
# (Jul 1 - Jun 30, per Fred) rather than stopping wherever Amion's actual
# schedule build-out currently ends (interns in particular have a second
# half of the year that isn't built out yet — see amion_integration
# project notes) — padded weeks just show as empty/zero, not missing.
# =============================================================================

#' @importFrom shiny NS moduleServer reactive req validate need renderUI uiOutput tagList h5 p div tags
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plot_ly add_trace layout renderPlotly plotlyOutput config
#' @importFrom DT renderDT DTOutput datatable formatRound
#' @importFrom dplyr filter arrange select mutate left_join
NULL

.DUTY_HOURS_AVG_COLOR    <- "#eb6834"
.DUTY_HOURS_LIMIT_COLOR  <- "#c0392b"
.DUTY_HOURS_TODAY_COLOR  <- "#6b7d82"
.DUTY_HOURS_VERIFIED_ALPHA   <- 1
.DUTY_HOURS_ANTICIPATED_ALPHA <- 0.35

#' @keywords internal
.hex_to_rgba <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1, ], rgb[2, ], rgb[3, ], alpha)
}

#' Weekly (Sunday-start) dates spanning the full academic year (Jul 1 -
#' Jun 30) containing `ref_date` — the display range for the chart/table,
#' independent of how far Amion's actual schedule build-out currently
#' reaches.
#' @keywords internal
.ay_week_starts <- function(ref_date = Sys.Date()) {
  ay_start_year <- current_ay_start(ref_date)
  start_date <- as.Date(sprintf("%d-07-01", ay_start_year))
  end_date   <- as.Date(sprintf("%d-06-30", ay_start_year + 1L))
  start_week <- as.Date(lubridate::floor_date(start_date, "week", week_start = 7))
  end_week   <- as.Date(lubridate::floor_date(end_date, "week", week_start = 7))
  seq(start_week, end_week, by = "week")
}

#' @rdname mod_duty_hour_summary
#' @export
mod_duty_hour_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("header")),
    shiny::tags$div(
      style = "margin-bottom: 8px;",
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(ns("chart"), height = "420px"),
        type = 6, color = "#2a78d6", size = 0.6
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

#' Duty-hour summary module — UI + server. Read-only display.
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

    # Unpadded — used only to pull the resident's Level for the header and
    # to confirm there's any data at all before rendering.
    resident_weekly_raw <- shiny::reactive({
      shiny::req(resident_id())
      duty_data()$weekly |> dplyr::filter(record_id == resident_id()) |> dplyr::arrange(week_start)
    })

    # Padded to the full academic year (Jul 1 - Jun 30) — see file header.
    resident_weekly <- shiny::reactive({
      all_weeks <- data.frame(week_start = .ay_week_starts())
      all_weeks |>
        dplyr::left_join(resident_weekly_raw(), by = "week_start") |>
        dplyr::mutate(Total_Hours = ifelse(is.na(Total_Hours), 0, Total_Hours),
                      Home_Hours  = ifelse(is.na(Home_Hours), 0, Home_Hours)) |>
        dplyr::arrange(week_start)
    })

    # Full week x category x verified grid, 0-filled — guarantees the
    # stacked chart's x-axis spans the whole AY even where no data exists
    # yet for a given (week, category) combination.
    resident_by_cat <- shiny::reactive({
      shiny::req(resident_id())
      raw <- duty_data()$weekly_by_category |> dplyr::filter(record_id == resident_id())
      grid <- expand.grid(
        week_start = .ay_week_starts(),
        super_category = names(DUTY_HOUR_CATEGORY_COLORS),
        verified = c(TRUE, FALSE),
        stringsAsFactors = FALSE
      )
      grid |>
        dplyr::left_join(raw, by = c("week_start", "super_category", "verified")) |>
        dplyr::mutate(Hours = ifelse(is.na(Hours), 0, Hours))
    })

    resident_gaps <- shiny::reactive({
      shiny::req(resident_id())
      duty_data()$rest_gap_flags |> dplyr::filter(record_id == resident_id())
    })

    output$header <- shiny::renderUI({
      shiny::req(nrow(resident_weekly_raw()) > 0)
      shiny::tagList(
        shiny::h5(paste0("Duty Hours — ", resident_weekly_raw()$Level[1], " class")),
        shiny::p(class = "text-muted small",
                 "Bars are colored by rotation type; the legend and hover text name each one. ",
                 shiny::tags$strong("Solid = verified"),
                 " (you've confirmed or entered that day). ",
                 shiny::tags$strong("Lighter = anticipated"),
                 " — still just your Amion schedule, not yet confirmed, whether that day is past or future.")
      )
    })

    output$chart <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(nrow(resident_weekly_raw()) > 0,
                    "No Amion schedule data available for this resident.")
      )
      wk  <- resident_weekly()
      cat <- resident_by_cat()
      cats <- names(DUTY_HOUR_CATEGORY_COLORS)

      p <- plotly::plot_ly()
      for (cc in cats) {
        base_color <- unname(DUTY_HOUR_CATEGORY_COLORS[[cc]])
        verified_rows   <- cat[cat$super_category == cc & cat$verified, ]
        anticipated_rows <- cat[cat$super_category == cc & !cat$verified, ]
        verified_rows   <- verified_rows[order(verified_rows$week_start), ]
        anticipated_rows <- anticipated_rows[order(anticipated_rows$week_start), ]

        p <- p |> plotly::add_trace(
          data = verified_rows, x = ~week_start, y = ~Hours, type = "bar",
          name = cc, legendgroup = cc, showlegend = TRUE,
          marker = list(color = .hex_to_rgba(base_color, .DUTY_HOURS_VERIFIED_ALPHA)),
          hovertemplate = paste0(cc, " (verified)<br>Week of %{x}<br>%{y:.1f} hours<extra></extra>")
        )
        p <- p |> plotly::add_trace(
          data = anticipated_rows, x = ~week_start, y = ~Hours, type = "bar",
          name = cc, legendgroup = cc, showlegend = FALSE,
          marker = list(color = .hex_to_rgba(base_color, .DUTY_HOURS_ANTICIPATED_ALPHA)),
          hovertemplate = paste0(cc, " (anticipated)<br>Week of %{x}<br>%{y:.1f} hours<extra></extra>")
        )
      }

      today_x <- as.numeric(Sys.Date()) * 24 * 60 * 60 * 1000  # ms epoch, matches plotly's date axis

      p |>
        plotly::add_trace(
          data = wk, x = ~week_start, y = ~rolling_4wk_avg_hours, type = "scatter", mode = "lines+markers",
          name = "4-week rolling avg", legendgroup = "avg",
          line = list(color = .DUTY_HOURS_AVG_COLOR, width = 2),
          marker = list(color = .DUTY_HOURS_AVG_COLOR, size = 5),
          hovertemplate = "Week of %{x}<br>4wk avg: %{y:.1f} hours<extra></extra>"
        ) |>
        plotly::layout(
          barmode = "stack",
          xaxis = list(
            title = "", gridcolor = "#e9eff0",
            dtick = 14 * 24 * 60 * 60 * 1000,  # 2-week ticks (ms) — more granular than plotly's month-level default
            tickformat = "%b %d", tickangle = -45
          ),
          yaxis = list(title = "Hours", zeroline = FALSE, gridcolor = "#e9eff0"),
          shapes = list(
            list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                 y0 = 80, y1 = 80, yref = "y",
                 line = list(color = .DUTY_HOURS_LIMIT_COLOR, dash = "dash", width = 1.5)),
            list(type = "line", x0 = today_x, x1 = today_x, xref = "x",
                 y0 = 0, y1 = 1, yref = "paper",
                 line = list(color = .DUTY_HOURS_TODAY_COLOR, dash = "dot", width = 1.5))
          ),
          annotations = list(
            list(x = 1, xref = "paper", y = 80, yref = "y", xanchor = "right", yanchor = "bottom",
                 text = "80h reference", showarrow = FALSE,
                 font = list(color = .DUTY_HOURS_LIMIT_COLOR, size = 11)),
            list(x = today_x, xref = "x", y = 1, yref = "paper", yanchor = "bottom",
                 text = "Today", showarrow = FALSE, font = list(color = .DUTY_HOURS_TODAY_COLOR, size = 11))
          ),
          legend = list(orientation = "h", x = 0, y = 1.18),
          margin = list(b = 90),
          plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "inherit")
        ) |>
        plotly::config(displaylogo = FALSE, displayModeBar = FALSE)
    })

    output$flags <- shiny::renderUI({
      shiny::req(nrow(resident_weekly_raw()) > 0)
      wk_raw <- resident_weekly_raw()
      latest <- wk_raw[nrow(wk_raw), ]
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
      shiny::req(nrow(resident_weekly_raw()) > 0)
      tbl <- resident_weekly() |>
        dplyr::select(week_start, Total_Hours, Home_Hours, Days_Worked, rolling_4wk_avg_hours,
                      flag_80h, rolling_days_per_week, flag_low_days) |>
        dplyr::arrange(dplyr::desc(week_start))
      DT::datatable(
        tbl, rownames = FALSE,
        colnames = c("Week of", "Hours", "At-Home Hours", "Days Worked", "4wk Avg Hours",
                    "80h Flag", "Days/wk Avg", "Low-Days Flag"),
        options = list(pageLength = 10, order = list(list(0, "desc")))
      ) |>
        DT::formatRound(columns = c("Total_Hours", "Home_Hours", "rolling_4wk_avg_hours", "rolling_days_per_week"), digits = 1)
    })
  })
}

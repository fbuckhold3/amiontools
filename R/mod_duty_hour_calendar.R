# mod_duty_hour_calendar.R ─ Duty Hours: calendar grid (Phase 2)
#
# Promoted from imslu.ind.dash (2026-09) so both ind.dash and
# imslu.resident.digest can compose it — no app_config coupling to remove,
# this module was already fully parameter-driven. No behavior change from
# the ind.dash original beyond dropping the (now-redundant, same-package)
# `amiontools::` prefix on sibling calls.
#
# Month-at-a-time grid, one cell per day. Clicking a day cell sets the
# shared `selected_date` reactiveVal (owned by the parent orchestrator) so
# mod_duty_hour_confirm shows/edits that specific date instead of the
# queue's oldest. Own month-navigation state is local (not shared — no
# other module needs to know which month is displayed).
#
# Color scheme (Fred 2026-09-15, matches the weekly chart exactly — same
# DUTY_HOUR_CATEGORY_COLORS palette):
#   hue      = rotation super-category (Inpatient/Continuity Clinic/
#              Ambulatory/etc.) — what kind of day it is
#   opacity  = verified (full) vs. anticipated (light) — whether the
#              resident has actually confirmed/entered that day, not
#              whether the date itself is past or future
#   dashed red border = needs entry (no Amion default AND no saved entry —
#              actionable, distinct from "anticipated")
#   very light gray, no border = no data at all (outside Amion's pull
#              range, or genuinely unmatched)
# A native `title` attribute gives every cell a hover tooltip (date,
# category, hours, status) — no JS/library needed for that.
#
# Click wiring uses a plain onclick -> Shiny.setInputValue() pattern (one
# shared input id for every cell, the clicked date as the value) rather
# than one actionButton per day — registering up to 31 separate
# observeEvent()s per month, recreated on every render, would be fragile
# (duplicate-firing / cleanup risk); one observer on one input handles
# every cell regardless of how many days are in the month.

#' @importFrom shiny NS tagList p tags div actionButton h5 textOutput uiOutput moduleServer reactiveVal reactive observeEvent req renderText renderUI
#' @importFrom grDevices col2rgb
NULL

.DH_CAL_NO_DATA_COLOR <- "#eef2f3"
.DH_CAL_NEEDS_ENTRY_BORDER <- "#c0392b"
.DH_CAL_VERIFIED_ALPHA    <- 1
.DH_CAL_ANTICIPATED_ALPHA <- 0.30

.dh_cal_hex_to_rgba <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1, ], rgb[2, ], rgb[3, ], alpha)
}

#' @export
mod_duty_hour_calendar_ui <- function(id) {
  ns <- NS(id)
  cats <- names(DUTY_HOUR_CATEGORY_COLORS)
  legend_items <- lapply(cats, function(cc) {
    tags$span(
      tags$span(style = sprintf(
        "display:inline-block;width:10px;height:10px;background:%s;border-radius:2px;margin-right:4px;",
        unname(DUTY_HOUR_CATEGORY_COLORS[[cc]])
      )), cc
    )
  })
  tagList(
    p(class = "small text-muted", style = "margin-bottom: 8px;",
      "Each day's ", tags$strong("color"), " shows what kind of day it was; its ",
      tags$strong("shade"), " shows whether you've confirmed it. Hover any day for details, or click it to confirm/edit."),
    div(class = "d-flex justify-content-between align-items-center mb-2",
      actionButton(ns("prev_month"), "‹ Prev", class = "btn btn-sm btn-outline-secondary"),
      h5(textOutput(ns("month_label"), inline = TRUE), style = "margin: 0;"),
      actionButton(ns("next_month"), "Next ›", class = "btn btn-sm btn-outline-secondary")
    ),
    uiOutput(ns("grid")),
    div(class = "d-flex gap-3 mt-2 small text-muted flex-wrap", legend_items),
    # Visual swatches, not text alone — "solid vs. light" is hard to picture
    # from a description, especially on a month with no confirmed days yet
    # to look at (Fred, 2026-09-16). Uses the same color/alpha the grid
    # cells themselves use, so this is a literal example, not an approximation.
    div(class = "d-flex align-items-center gap-4 mt-1 mb-1 small text-muted flex-wrap",
      div(class = "d-flex align-items-center gap-2",
        tags$span(style = sprintf(
          "display:inline-block;width:16px;height:16px;border-radius:3px;background:%s;",
          .dh_cal_hex_to_rgba(unname(DUTY_HOUR_CATEGORY_COLORS[[1]]), .DH_CAL_VERIFIED_ALPHA)
        )),
        "Solid = you've confirmed it"),
      div(class = "d-flex align-items-center gap-2",
        tags$span(style = sprintf(
          "display:inline-block;width:16px;height:16px;border-radius:3px;background:%s;",
          .dh_cal_hex_to_rgba(unname(DUTY_HOUR_CATEGORY_COLORS[[1]]), .DH_CAL_ANTICIPATED_ALPHA)
        )),
        "Light = still just your Amion schedule, not yet confirmed")
    ),
    div(class = "d-flex gap-3 mt-1 small text-muted flex-wrap",
      tags$span(style = sprintf("border: 2px dashed %s; padding: 0 4px; border-radius:2px;", .DH_CAL_NEEDS_ENTRY_BORDER), "Needs entry — no default, log it yourself")
    )
  )
}

#' Duty-hour calendar module server
#'
#' @param id Module id.
#' @param resident_id reactive() -> record_id.
#' @param entries_r,amion_blocks_r Shared reactives from the parent
#'   orchestrator.
#' @param selected_date Shared reactiveVal — clicking a cell sets it.
#' @export
mod_duty_hour_calendar_server <- function(id, resident_id, entries_r, amion_blocks_r, selected_date) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    month_start <- reactiveVal(as.Date(format(Sys.Date(), "%Y-%m-01")))

    observeEvent(input$prev_month, month_start(seq(month_start(), length.out = 2, by = "-1 month")[2]))
    observeEvent(input$next_month, month_start(seq(month_start(), length.out = 2, by = "1 month")[2]))
    observeEvent(input$day_click, {
      req(input$day_click)
      selected_date(as.Date(input$day_click))
    })

    output$month_label <- renderText(format(month_start(), "%B %Y"))

    # One row per visible day: super_category (color hue), verified
    # (opacity), needs_entry (dashed border), has_data, Hours, tooltip text.
    day_info <- reactive({
      ms <- month_start()
      me <- seq(ms, length.out = 2, by = "1 month")[2] - 1
      db <- amion_blocks_r() |> dplyr::filter(Date >= ms, Date <= me)
      entries <- entries_r()
      covered <- entries$dh_date[entries$dh_date >= ms & entries$dh_date <= me]
      all_days <- seq(ms, me, by = "day")

      rows <- lapply(all_days, function(d) {
        drows <- db[db$Date == d, ]
        is_verified <- d %in% covered
        if (nrow(drows) == 0 && !is_verified) {
          return(data.frame(Date = d, has_data = FALSE, super_category = NA_character_,
                            verified = FALSE, needs_entry = FALSE, Hours = NA_real_,
                            category_label = NA_character_, stringsAsFactors = FALSE))
        }
        is_off <- nrow(drows) > 0 && all(drows$source %in% c("vacation", "day_off", "jeopardy"))
        # dominant category = most hours that day; ties/all-NA -> first row's category
        cat_label <- if (is_off) "Time Off/Holiday"
                     else if (nrow(drows) == 0) "Confirmed"  # verified but not in amion_blocks (shouldn't normally happen)
                     else if (all(is.na(drows$Hours))) drows$category[1]
                     else drows$category[which.max(ifelse(is.na(drows$Hours), -1, drows$Hours))]
        super_cat <- classify_super_category(cat_label)
        if (is.na(super_cat) || super_cat == "UNMAPPED") super_cat <- "Other"
        needs_entry <- !is_verified && nrow(drows) > 0 && all(is.na(drows$Hours))
        total_hours <- if (nrow(drows) == 0) NA_real_ else sum(drows$Hours, na.rm = TRUE)
        data.frame(Date = d, has_data = TRUE, super_category = super_cat,
                  verified = is_verified, needs_entry = needs_entry, Hours = total_hours,
                  category_label = cat_label, stringsAsFactors = FALSE)
      })
      do.call(rbind, rows)
    })

    output$grid <- renderUI({
      ms <- month_start()
      me <- seq(ms, length.out = 2, by = "1 month")[2] - 1
      all_days <- seq(ms, me, by = "day")
      info <- day_info()
      lead_blanks <- as.integer(format(ms, "%w"))  # 0 = Sunday

      cells <- lapply(seq_along(all_days), function(i) {
        d <- all_days[i]
        row <- info[i, ]
        is_future_no_data <- !row$has_data && d > Sys.Date()
        clickable <- d <= Sys.Date() || row$has_data

        bg <- if (!row$has_data) .DH_CAL_NO_DATA_COLOR
              else .dh_cal_hex_to_rgba(
                unname(DUTY_HOUR_CATEGORY_COLORS[[row$super_category]]),
                if (row$verified) .DH_CAL_VERIFIED_ALPHA else .DH_CAL_ANTICIPATED_ALPHA
              )
        border <- if (isTRUE(row$needs_entry)) sprintf("2px dashed %s", .DH_CAL_NEEDS_ENTRY_BORDER) else "none"

        tooltip <- if (!row$has_data) format(d, "%a, %b %d, %Y")
                   else sprintf(
                     "%s — %s%s%s",
                     format(d, "%a, %b %d, %Y"), row$category_label,
                     if (!is.na(row$Hours)) sprintf(" — %.1fh", row$Hours) else "",
                     if (row$verified) " — Verified" else if (row$needs_entry) " — Needs entry" else " — Anticipated"
                   )

        tags$div(
          class = "dh-cal-cell",
          title = tooltip,
          style = sprintf(
            "background:%s; border:%s; opacity:%s; cursor:%s; border-radius:4px; padding:6px; text-align:center; min-height:44px;",
            bg, border, if (is_future_no_data) "0.5" else "1",
            if (clickable && !is_future_no_data) "pointer" else "default"
          ),
          onclick = if (clickable && !is_future_no_data)
            sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("day_click"), as.character(d))
          else NULL,
          tags$div(format(d, "%e"), style = "font-weight:600; font-size:0.85rem;")
        )
      })

      blanks <- lapply(seq_len(lead_blanks), function(i) tags$div())

      tags$div(
        style = "display:grid; grid-template-columns: repeat(7, 1fr); gap: 4px;",
        tags$div("S", class = "text-muted small text-center"), tags$div("M", class = "text-muted small text-center"),
        tags$div("T", class = "text-muted small text-center"), tags$div("W", class = "text-muted small text-center"),
        tags$div("T", class = "text-muted small text-center"), tags$div("F", class = "text-muted small text-center"),
        tags$div("S", class = "text-muted small text-center"),
        blanks, cells
      )
    })
  })
}

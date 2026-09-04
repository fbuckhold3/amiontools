# =============================================================================
# Shiny module: one resident's rotation days broken out by training year
# actually lived (Intern/PGY2/PGY3), plus a Total, each paired with the
# class average for that same training year. Forward-only -- see
# build_rotation_by_training_year()'s file header for why a training year
# can show blank (not available) rather than a number.
#
# The current AY's own data isn't fetched or recomputed at all when a
# cached_rotation_r reactive is supplied (e.g. from
# use_amion_data_cached()$rotation) -- it's the exact same data already
# sitting in the REDCap cache, just reshaped into the Intern/PGY2/PGY3
# breakdown. Falls back to crosswalk_r/amion_r (reuse mod_daily_detail's
# live fetch) or a fully independent fetch, in that priority order, if the
# cache isn't available. Only post-rebuild AYs other than the current one
# (none exist yet, but this will matter starting AY2027-28) ever need a
# real fetch of their own.
# =============================================================================

#' @importFrom shiny NS moduleServer reactive req validate need renderUI uiOutput tagList h5 p
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT renderDT DTOutput datatable formatRound
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
#' @param crosswalk_r,amion_r Optional reactives (e.g. from
#'   use_amion_data()) returning pre-fetched crosswalk/current-AY Amion
#'   data — reused for the current AY's iteration instead of fetching
#'   again, if `cached_rotation_r` isn't also supplied. NULL (default):
#'   fetches its own data.
#' @param cached_rotation_r Optional reactive (e.g.
#'   \code{use_amion_data_cached()$rotation}) returning the current AY's
#'   pre-computed rotation summary straight from the REDCap cache — when
#'   supplied, the current AY's contribution to this table costs nothing
#'   extra (no live fetch, no recompute). Takes priority over
#'   crosswalk_r/amion_r when both are supplied. NULL (default): current AY
#'   computed like any other.
#' @name mod_rotation_by_year
#' @export
mod_rotation_by_year_server <- function(id, resident_id, rdm_token, redcap_url,
                                        amion_lo = AMION_LO_DEFAULT,
                                        crosswalk_r = NULL,
                                        amion_r = NULL,
                                        cached_rotation_r = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    by_year_data <- shiny::reactive({
      build_rotation_by_training_year(
        rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
        crosswalk = if (!is.null(crosswalk_r)) crosswalk_r() else NULL,
        current_ay_amion = if (!is.null(amion_r)) amion_r() else NULL,
        current_ay_rotation = if (!is.null(cached_rotation_r)) cached_rotation_r() else NULL
      )
    })

    # Column order interleaved per training year (Intern You/Avg, PGY2
    # You/Avg, ...) rather than merge()'s default x-block-then-y-block, so
    # each year's comparison reads together.
    .LEVEL_COLS <- c("Intern", "PGY2", "PGY3", "Total")
    .DISPLAY_ORDER <- c("Category", unlist(lapply(.LEVEL_COLS, function(l) paste0(l, c(" (You)", " (Class Avg)")))))

    resident_rows <- shiny::reactive({
      shiny::req(resident_id())
      you <- by_year_data()$by_resident |>
        dplyr::filter(record_id == resident_id(), Total > 0) |>
        dplyr::select(dplyr::any_of(c("Category", .LEVEL_COLS)))
      avg <- by_year_data()$class_avg |>
        dplyr::select(dplyr::any_of(c("Category", .LEVEL_COLS)))

      shiny::validate(shiny::need(nrow(you) > 0, "No Amion schedule data available for this resident."))

      merged <- merge(you, avg, by = "Category", suffixes = c(" (You)", " (Class Avg)"), all.x = TRUE)
      merged <- merged[, intersect(.DISPLAY_ORDER, names(merged))]
      merged[order(-merged$`Total (You)`), ]
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
                 "Days by rotation category for each training year actually lived, vs. the ",
                 "average for everyone who held that year since the schedule was rebuilt for ",
                 "AY2026-27. A blank cell means that training year isn't covered by ",
                 "post-rebuild data yet (not zero days) — fills in over the next couple of ",
                 "years as each class progresses.")
      )
    })

    output$table <- DT::renderDT({
      shiny::req(nrow(resident_rows()) > 0)
      DT::datatable(
        resident_rows(),
        rownames = FALSE,
        options = list(pageLength = 20, dom = "t", scrollX = TRUE),
      ) |>
        DT::formatRound(columns = setdiff(names(resident_rows()), "Category"), digits = 1)
    })
  })
}

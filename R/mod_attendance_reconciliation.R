# =============================================================================
# Shiny module: conference attendance reconciliation — days a resident was
# EXPECTED at a noon conference (from their Amion rotation) vs. days they
# actually logged attendance in RDM's "questions" form.
#
# Rule table + general attendance rule: see expected_attendance.R's header.
# The "questions" log is deliberately never cached (see the caching-
# architecture decision, 2026-09-03) -- residents log it in real time, and
# stale attendance status would be actively misleading. Amion/RDM crosswalk
# side reuses the shared crosswalk_r/amion_r pattern like every other
# module, but isn't cache-fed yet either -- brand new, not yet validated by
# Fred beyond the build-time spot checks.
# =============================================================================

#' @importFrom shiny NS moduleServer reactive req validate need renderUI uiOutput tagList h5 p
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT renderDT DTOutput datatable
#' @importFrom dplyr filter
NULL

#' @rdname mod_attendance_reconciliation
#' @export
mod_attendance_reconciliation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("header")),
    shinycssloaders::withSpinner(DT::DTOutput(ns("table")), type = 6, color = "#2a78d6", size = 0.5)
  )
}

#' Attendance-reconciliation module — UI + server.
#'
#' @param id Module namespace id.
#' @param resident_id Reactive returning the currently-selected RDM
#'   record_id (character/numeric coercible).
#' @param rdm_token RDM REDCap API token (test or prod — caller's choice).
#' @param redcap_url REDCap API URL.
#' @param amion_lo Amion Lo= program token; defaults to AMION_LO_DEFAULT.
#' @param crosswalk_r,amion_r Optional reactives (e.g. from
#'   use_amion_data()) returning pre-fetched crosswalk/Amion data — pass
#'   these to fetch once instead of independently re-fetching. NULL
#'   (default): fetches its own data.
#' @name mod_attendance_reconciliation
#' @export
mod_attendance_reconciliation_server <- function(id, resident_id, rdm_token, redcap_url,
                                                  amion_lo = AMION_LO_DEFAULT,
                                                  crosswalk_r = NULL,
                                                  amion_r = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    recon_data <- shiny::reactive({
      build_attendance_reconciliation(
        rdm_token = rdm_token, redcap_url = redcap_url, amion_lo = amion_lo,
        crosswalk = if (!is.null(crosswalk_r)) crosswalk_r() else NULL,
        amion     = if (!is.null(amion_r)) amion_r() else NULL
      )
    })

    resident_row <- shiny::reactive({
      shiny::req(resident_id())
      recon_data()$summary_wide |>
        dplyr::filter(record_id == resident_id())
    })

    comparison_table <- shiny::reactive({
      shiny::req(recon_data())
      shiny::validate(
        shiny::need(nrow(resident_row()) > 0,
                    "No Amion schedule data available for this resident.")
      )

      res  <- resident_row()
      level <- res$Level[1]
      class_row <- recon_data()$class_avg_wide |>
        dplyr::filter(Level == level)

      # Pre-formatted as display strings (not left to DT's row-selective
      # formatting, which is fragile to mix a count row and a percentage
      # row in one column) -- Days rounded to 1 decimal (class average is a
      # mean, so rarely a whole number), rate as a whole-number percentage.
      data.frame(
        Metric = c("Expected Days", "Attended Days", "Attendance Rate"),
        You = c(sprintf("%.1f", res$Expected_Days), sprintf("%.1f", res$Attended_Days),
                sprintf("%.0f%%", res$Attendance_Rate * 100)),
        `Class Average` = c(sprintf("%.1f", class_row$Avg_Expected_Days), sprintf("%.1f", class_row$Avg_Attended_Days),
                             sprintf("%.0f%%", class_row$Avg_Attendance_Rate * 100)),
        check.names = FALSE
      )
    })

    output$header <- shiny::renderUI({
      shiny::req(nrow(resident_row()) > 0)
      shiny::tagList(
        shiny::h5(paste0("Conference Attendance — ", resident_row()$Level[1], " class")),
        shiny::p(class = "text-muted small",
                 "Weekdays this resident was expected at a noon conference (based on their Amion ",
                 "rotation) vs. days they logged attendance, current academic year through today, ",
                 "vs. this resident's class average. \"Expected\" reflects only rotations that put ",
                 "them at SLUH or the VA around noon — electives, night float, time off, and similar ",
                 "are excluded, not counted as missed.")
      )
    })

    output$table <- DT::renderDT({
      shiny::req(nrow(resident_row()) > 0)
      DT::datatable(
        comparison_table(),
        rownames = FALSE,
        options = list(pageLength = 5, dom = "t")
      )
    })
  })
}

# mod_conference_catchup.R ─ log many missed conference days at once
#
# The self-eval gate needs every expected conference day accounted for, and
# most residents are far behind. mod_attendance logs one date at a time;
# this lists the unlogged days so each takes one click. Every save goes
# through save_attendance_entry(), the one write path for `questions`.

# Expected-conference label -> q_conference_type code for an attended day.
.catchup_conf_code <- c(SLUH = "1", VA = "2")

#' Conference catch-up list UI
#' @param id Module id.
#' @export
mod_conference_catchup_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("rows")),
    shiny::uiOutput(ns("status"))
  )
}

#' Conference catch-up list server
#'
#' Rows are answered Attended or Did not attend (a reason is required for
#' the latter). Attended rows are saved with a blank rotation, since the
#' list is built for speed.
#'
#' @param id Module id.
#' @param resident_id reactive() -> record_id.
#' @param days_r reactive() -> data frame of unlogged expected days with
#'   \code{Date}, \code{conf_expected} ("SLUH"/"VA") and \code{Rotation}.
#' @param rdm_token,redcap_url REDCap credentials (test or prod: caller's
#'   choice).
#' @return reactive() that increases by one after each save that wrote at
#'   least one row.
#' @export
mod_conference_catchup_server <- function(id, resident_id, days_r, rdm_token, redcap_url) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    saved_dates <- shiny::reactiveVal(as.Date(character()))
    saved_count <- shiny::reactiveVal(0L)
    status <- shiny::reactiveVal(NULL)
    highest_instance <- shiny::reactiveVal(0L)

    shiny::observeEvent(resident_id(), {
      saved_dates(as.Date(character())); status(NULL); highest_instance(0L)
    })

    open_days <- shiny::reactive({
      d <- days_r()
      shiny::req(d)
      d <- d[!d$Date %in% saved_dates(), , drop = FALSE]
      d[order(d$Date, decreasing = TRUE), , drop = FALSE]
    })

    row_id <- function(date) format(date, "%Y%m%d")

    output$rows <- shiny::renderUI({
      d <- open_days()
      if (nrow(d) == 0) {
        return(shiny::tags$p(class = "text-success mb-0",
                             "Every expected conference day has an entry."))
      }
      rows <- lapply(seq_len(nrow(d)), function(i) {
        key <- row_id(d$Date[i])
        rot <- d$Rotation[i]
        shiny::tags$div(
          class = "d-flex flex-wrap align-items-center gap-3 py-2 border-bottom",
          shiny::tags$div(
            style = "min-width:190px;",
            shiny::tags$strong(format(d$Date[i], "%a %b %d")),
            shiny::tags$div(class = "small text-muted",
                            paste0(d$conf_expected[i], " conference",
                                   if (!is.na(rot) && nzchar(rot)) paste0(" · ", rot) else ""))
          ),
          shiny::radioButtons(ns(paste0("ans_", key)), label = NULL, inline = TRUE,
                              choices = c("Attended" = "1", "Did not attend" = "0"),
                              selected = character(0)),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == '0'", ns(paste0("ans_", key))),
            shiny::textInput(ns(paste0("why_", key)), label = NULL,
                             placeholder = "Reason (required)", width = "280px")
          )
        )
      })
      shiny::tagList(
        rows,
        shiny::tags$div(
          class = "mt-3",
          shiny::actionButton(ns("save"), "Save answered days", class = "btn btn-primary btn-sm")
        )
      )
    })

    output$status <- shiny::renderUI({
      s <- status()
      if (is.null(s)) return(NULL)
      shiny::tags$div(class = paste0("mt-2 small ", if (s$ok) "text-success" else "text-danger"), s$text)
    })

    shiny::observeEvent(input$save, {
      d <- open_days()
      keys <- row_id(d$Date)
      answers <- vapply(keys, function(k) {
        v <- input[[paste0("ans_", k)]]
        if (is.null(v) || length(v) == 0) "" else as.character(v)
      }, character(1))
      reasons <- vapply(keys, function(k) trimws(input[[paste0("why_", k)]] %||% ""), character(1))

      todo <- which(answers != "")
      if (length(todo) == 0) {
        status(list(ok = FALSE, text = "Answer at least one day first."))
        return()
      }
      no_reason <- todo[answers[todo] == "0" & reasons[todo] == ""]
      if (length(no_reason) > 0) {
        status(list(ok = FALSE, text = paste0(
          "Nothing saved. Add a reason for: ",
          paste(format(d$Date[no_reason], "%b %d"), collapse = ", "), ".")))
        return()
      }

      failed <- character(); done <- as.Date(character())
      floor_inst <- highest_instance()
      shiny::withProgress(message = "Saving conference days", value = 0, {
        for (i in todo) {
          attended <- answers[i] == "1"
          res <- save_attendance_entry(
            record_id = resident_id(), date = d$Date[i],
            conference_type = if (attended) unname(.catchup_conf_code[d$conf_expected[i]]) else NULL,
            attended = attended, absence_reason = reasons[i],
            rdm_token = rdm_token, redcap_url = redcap_url, min_instance = floor_inst
          )
          if (isTRUE(res$success)) {
            floor_inst <- max(floor_inst, res$instance)
            done <- c(done, d$Date[i])
          } else {
            failed <- c(failed, format(d$Date[i], "%b %d"))
          }
          shiny::incProgress(1 / length(todo))
        }
      })
      highest_instance(floor_inst)

      if (length(done) > 0) {
        saved_dates(c(saved_dates(), done))
        saved_count(saved_count() + 1L)
      }
      status(if (length(failed) > 0) {
        list(ok = FALSE, text = paste0("Saved ", length(done), ". Could not save: ",
                                       paste(failed, collapse = ", "), ". Please try those again."))
      } else {
        list(ok = TRUE, text = paste0("Saved ", length(done), if (length(done) == 1) " day." else " days."))
      })
    })

    saved_count
  })
}

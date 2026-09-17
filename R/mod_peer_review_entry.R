# mod_peer_review_entry.R ─ Peer Review (resident-to-resident)
#
# Promoted from imslu.ind.dash (2026-09) so both ind.dash and
# imslu.resident.digest can compose it. The outer server signature already
# took `rdm_token`/`redcap_url` as params; the 4 internal REDCap helpers
# (.peer_next_instance, .peer_count_completed, .peer_fetch_own_reviews,
# .peer_submit) did NOT — they read `app_config$rdm_token`/
# `app_config$redcap_url` directly, an ind.dash-only global. This promotion
# threads the already-passed params through to all 4, and drops the
# `amiontools::` prefix on get_recent_teammates() (now same-package). No
# other behavior change.
#
# Resident-only entry into the peer_eval repeating instrument (schema added
# 2026-09-16: peer_date/peer_plus/peer_delta already existed; this session
# added peer_item1-5 (radio, 6-pt frequency scale) and peer_evaluator_id).
#
# Data model, confirmed with Fred: when resident X reviews resident Y, the
# WHOLE instance (ratings + peer_plus/peer_delta + peer_evaluator_id) is
# written under Y's record_id — never X's. peer_evaluator_id rides along on
# that same instance as a hidden bookkeeping field: never surfaced to the
# reviewee or in any aggregate peer-review view, used only to power X's own
# "reviews completed" count in the summary strip below (computed via a
# server-side-filtered query on that field alone, across ALL residents'
# records — never pulls anyone else's ratings/comments into this app).
#
# Steps: "search" (recently-worked-with quick picks + free-text name search)
#   -> "form" (5-item scale + plus/delta) -> "done".
#
# Placeholder/non-reviewable RDM records (same exclusions used elsewhere in
# this ecosystem, e.g. resident.assessment / amiontools crosswalk): 157
# (shared rotator placeholder, blank name), 999 (blank/unused), 2039
# (leftover test record).

#' @importFrom shiny NS tagList uiOutput div tags singleton HTML moduleServer reactiveVal observeEvent reactive req renderUI textInput textAreaInput actionButton showNotification
NULL

.PEER_SCALE_CHOICES <- c(
  "Never" = "1", "Rarely" = "2", "Sometimes" = "3",
  "Often" = "4", "Always" = "5", "Not observed" = "9"
)

.PEER_ITEMS <- list(
  list(field = "peer_item1", label = "Functions effectively as a member of the healthcare team."),
  list(field = "peer_item2", label = "Treats colleagues, nurses, and other staff with respect."),
  list(field = "peer_item3", label = "Communicates clearly and reliably during handoffs and shared patient care."),
  list(field = "peer_item4", label = "Is dependable — takes initiative, is well-prepared, and completes assigned tasks."),
  list(field = "peer_item5", label = "Can be trusted to seek help appropriately and acknowledge uncertainty or errors.")
)

.PEER_NON_REVIEWABLE_IDS <- c("157", "999", "2039")

# ── Rating question UI: 6-option segmented-pill widget ───────────────────────
# Mirrors ind.dash's mod_faculty_eval.R .fe_rating_q() pattern (custom button
# group + hidden input + JS onclick), adapted for 6 labeled categorical
# options instead of a 1-5 numeric scale. "Not observed" is visually
# separated (a divider + dashed/grey styling) since it means "N/A", not a
# low rating on the same ordinal continuum as the other 5.
.peer_rating_q <- function(ns, field, label) {
  nid <- ns(field)
  ordinal <- .PEER_SCALE_CHOICES[1:5]
  na_opt  <- .PEER_SCALE_CHOICES[6]

  mk_btn <- function(lbl, val, i, na = FALSE) {
    tags$button(
      type = "button", id = paste0(nid, "_b", i),
      class = paste("peer-scale-btn", if (na) "peer-scale-btn-na" else ""),
      onclick = sprintf("peerRate('%s', %d, '%s')", nid, i, val),
      lbl
    )
  }

  div(class = "mb-4",
    tags$p(label, style = "font-weight:600; font-size:0.9rem; color:#2d3748; margin-bottom:10px;"),
    div(style = "display:flex; flex-wrap:wrap; align-items:center; gap:8px;",
      div(style = "display:flex; flex-wrap:wrap; gap:6px;",
        Map(mk_btn, names(ordinal), unname(ordinal), seq_along(ordinal))
      ),
      div(style = "width:1px; align-self:stretch; background:var(--gmed-border,#dde5ed); margin:2px 2px;"),
      mk_btn(names(na_opt), unname(na_opt), 6L, na = TRUE)
    ),
    tags$input(type = "hidden", id = nid, name = nid, value = "")
  )
}

.peer_rating_css_js <- singleton(tags$head(
  tags$style(HTML("
    .peer-scale-btn {
      border: 2px solid var(--gmed-border, #dde5ed);
      background: white;
      color: #2d3748;
      border-radius: 20px;
      padding: 6px 14px;
      font-size: 0.82rem;
      font-weight: 600;
      cursor: pointer;
      transition: all 0.15s ease;
    }
    .peer-scale-btn:hover { border-color: var(--gmed-secondary, #0f8a94); }
    .peer-scale-btn.selected {
      background: var(--gmed-secondary, #0f8a94);
      border-color: var(--gmed-secondary, #0f8a94);
      color: white;
    }
    .peer-scale-btn-na { color: #888; border-style: dashed; }
    .peer-scale-btn-na.selected { background: #888; border-color: #888; color: white; }
  ")),
  tags$script(HTML("
    function peerRate(qId, idx, val) {
      for (var i = 1; i <= 6; i++) {
        var b = document.getElementById(qId + '_b' + i);
        if (!b) continue;
        b.classList.toggle('selected', i === idx);
      }
      var h = document.getElementById(qId);
      if (h) h.value = val;
      Shiny.setInputValue(qId, val, {priority: 'event'});
    }
  "))
))

# ── REDCap helpers ────────────────────────────────────────────────────────
# All 4 now take redcap_url/rdm_token explicitly (were implicit via
# ind.dash's app_config global before this promotion).

.peer_next_instance <- function(target_record_id, redcap_url, rdm_token) {
  tryCatch({
    resp <- httr::POST(
      redcap_url,
      body = list(
        token = rdm_token, content = "record", action = "export",
        format = "json", type = "flat",
        records    = as.character(target_record_id),
        `forms[0]` = "peer_eval",
        `fields[0]` = "record_id",
        rawOrLabel = "raw", rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false", exportSurveyFields = "false",
        exportDataAccessGroups = "false", returnFormat = "json"
      ),
      encode = "form"
    )
    if (httr::status_code(resp) == 200) {
      dat <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
      if (is.data.frame(dat) && nrow(dat) > 0) {
        pe <- dat[!is.na(dat$redcap_repeat_instrument) &
                    dat$redcap_repeat_instrument == "peer_eval", ]
        if (nrow(pe) > 0) {
          inst <- suppressWarnings(as.numeric(pe$redcap_repeat_instance))
          return(max(inst[!is.na(inst)], 0L) + 1L)
        }
      }
    }
    1L
  }, error = function(e) 1L)
}

# Counts how many peer_eval instances (anywhere, under anyone's record) carry
# this resident as peer_evaluator_id. Restricted to just those two fields —
# never pulls peer_item*/peer_plus/peer_delta content into this app.
.peer_count_completed <- function(evaluator_id, redcap_url, rdm_token) {
  tryCatch({
    resp <- httr::POST(
      redcap_url,
      body = list(
        token = rdm_token, content = "record", action = "export",
        format = "json", type = "flat",
        `forms[0]`  = "peer_eval",
        `fields[0]` = "record_id",
        `fields[1]` = "peer_evaluator_id",
        rawOrLabel = "raw", rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false", exportSurveyFields = "false",
        exportDataAccessGroups = "false", returnFormat = "json"
      ),
      encode = "form"
    )
    if (httr::status_code(resp) != 200) return(0L)
    dat <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    if (!is.data.frame(dat) || nrow(dat) == 0) return(0L)
    dat <- dat[!is.na(dat$redcap_repeat_instrument) &
                 dat$redcap_repeat_instrument == "peer_eval", , drop = FALSE]
    sum(!is.na(dat$peer_evaluator_id) &
          trimws(as.character(dat$peer_evaluator_id)) == as.character(evaluator_id))
  }, error = function(e) 0L)
}

# Fetches this resident's OWN peer_eval instances (as the reviewee) for the
# results section below. Deliberately requests ONLY peer_date/peer_item*/
# peer_plus/peer_delta as explicit fields[] — peer_evaluator_id is never
# even requested here, so there is no path for it to leak into this
# reviewee-facing view (not filtered out after the fact — never asked for).
.peer_fetch_own_reviews <- function(record_id, redcap_url, rdm_token) {
  tryCatch({
    item_fields <- vapply(.PEER_ITEMS, `[[`, character(1), "field")
    fields <- c("record_id", "peer_date", item_fields, "peer_plus", "peer_delta")
    body <- c(
      list(token = rdm_token, content = "record", action = "export",
           format = "json", type = "flat",
           records    = as.character(record_id),
           `forms[0]` = "peer_eval",
           rawOrLabel = "raw", rawOrLabelHeaders = "raw",
           exportCheckboxLabel = "false", exportSurveyFields = "false",
           exportDataAccessGroups = "false", returnFormat = "json"),
      setNames(as.list(fields), paste0("fields[", seq_along(fields) - 1L, "]"))
    )
    resp <- httr::POST(redcap_url, body = body, encode = "form", httr::timeout(30))
    if (httr::status_code(resp) != 200) return(data.frame())
    dat <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    if (!is.data.frame(dat) || nrow(dat) == 0) return(data.frame())
    dat <- dat[!is.na(dat$redcap_repeat_instrument) &
                 dat$redcap_repeat_instrument == "peer_eval", , drop = FALSE]
    if (nrow(dat) == 0) return(data.frame())
    dat$peer_date <- suppressWarnings(as.Date(dat$peer_date))
    dat
  }, error = function(e) data.frame())
}

# ── Reveal-eligibility computation (pure, no REDCap calls) ───────────────────
# Quarter-anchored, count-based batch reveal (Fred's rule, 2026-09-16):
# reviews accumulate in an "unrevealed pool"; at each calendar-quarter
# boundary (Jan 1 / Apr 1 / Jul 1 / Oct 1) that has actually passed, if the
# pool has grown to >= 5 since the last reveal, the WHOLE pool becomes
# visible (permanently — revealed sets only grow, never shrink) and resets
# to empty. No age/time embargo — purely a count threshold, checked on a
# fixed quarterly cadence rather than continuously, so visibility can't
# creep forward day-by-day and get correlated with "who just rotated off."
# Stateless by design: replays full history every call from the raw dates
# alone, so there is no separate "last revealed" field to keep in sync or
# ever drift out of correctness.
.peer_quarter_start <- function(d) {
  d   <- as.Date(d)
  yr  <- as.integer(format(d, "%Y"))
  mo  <- as.integer(format(d, "%m"))
  qmo <- ((mo - 1L) %/% 3L) * 3L + 1L
  as.Date(sprintf("%d-%02d-01", yr, qmo))
}

# Returns indices into the ORIGINAL (unsorted) `dates` vector that are
# revealed as of `as_of`. Zero-length integer(0) if nothing qualifies yet.
.peer_reveal_eligible_dates <- function(dates, as_of = Sys.Date()) {
  dates <- as.Date(dates)
  ord   <- order(dates)
  valid <- !is.na(dates[ord])
  sorted_idx   <- ord[valid]
  sorted_dates <- dates[sorted_idx]
  n <- length(sorted_dates)
  if (n == 0) return(integer(0))

  first_q <- .peer_quarter_start(sorted_dates[1])
  last_q  <- .peer_quarter_start(as.Date(as_of))
  if (last_q < first_q) return(integer(0))
  boundaries <- seq(first_q, last_q, by = "3 months")

  revealed_idx <- integer(0)
  pool_idx     <- integer(0)
  cursor       <- 0L

  for (i in seq_along(boundaries)) {
    b <- boundaries[i]
    while (cursor < n && sorted_dates[cursor + 1L] <= b) {
      cursor   <- cursor + 1L
      pool_idx <- c(pool_idx, sorted_idx[cursor])
    }
    if (length(pool_idx) >= 5L) {
      revealed_idx <- c(revealed_idx, pool_idx)
      pool_idx <- integer(0)
    }
  }
  revealed_idx
}

.peer_submit <- function(target_record_id, evaluator_id, fields, redcap_url, rdm_token) {
  next_inst <- .peer_next_instance(target_record_id, redcap_url, rdm_token)
  full_data <- c(
    list(record_id = as.character(target_record_id),
         redcap_repeat_instrument = "peer_eval",
         redcap_repeat_instance   = as.character(next_inst),
         peer_date          = format(Sys.Date(), "%Y-%m-%d"),
         peer_evaluator_id  = as.character(evaluator_id)),
    fields
  )
  rc_df <- as.data.frame(
    lapply(full_data, function(x) if (is.null(x) || is.na(x)) "" else as.character(x)),
    stringsAsFactors = FALSE
  )
  resp <- httr::POST(
    redcap_url,
    body = list(
      token = rdm_token, content = "record",
      format = "json", type = "flat",
      overwriteBehavior = "overwrite",
      data = jsonlite::toJSON(rc_df, auto_unbox = TRUE),
      returnContent = "ids", returnFormat = "json"
    ),
    encode = "form", httr::timeout(30)
  )
  status <- httr::status_code(resp)
  body   <- httr::content(resp, "text", encoding = "UTF-8")
  has_err <- grepl("\"error\"", body, fixed = TRUE) || grepl("^ERROR", body, ignore.case = TRUE)
  if (status != 200 || has_err) {
    stop("REDCap (HTTP ", status, "): ", substr(body, 1, 300))
  }
  invisible(full_data)
}

# ═════════════════════════════════════════════════════════════════════════════
# UI
# ═════════════════════════════════════════════════════════════════════════════
#' @export
mod_peer_review_entry_ui <- function(id) {
  ns <- NS(id)
  tagList(
    .peer_rating_css_js,
    uiOutput(ns("summary_strip")),
    div(class = "mt-3",
      div(class = "gmed-card",
        div(class = "card-body",
          tags$p(class = "fw-semibold mb-3",
                 style = "font-size:0.9rem; color:var(--ssm-primary-blue);",
                 tags$i(class = "bi bi-people-fill me-2"),
                 "Give Feedback to a Fellow Resident"),
          uiOutput(ns("main_ui"))
        )
      )
    ),
    div(class = "mt-4",
      div(class = "gmed-card",
        div(class = "card-body",
          tags$p(class = "fw-semibold mb-3",
                 style = "font-size:0.9rem; color:var(--ssm-primary-blue);",
                 tags$i(class = "bi bi-bar-chart-fill me-2"),
                 "Your Peer Review Results"),
          uiOutput(ns("results_ui")),
          DT::dataTableOutput(ns("comments_table"))
        )
      )
    )
  )
}

# ═════════════════════════════════════════════════════════════════════════════
# Server
# ═════════════════════════════════════════════════════════════════════════════
#' Peer review entry module server
#'
#' @param id Module id.
#' @param resident_id reactive() -> record_id (the reviewer).
#' @param all_residents_r reactive() -> full resident roster (record_id, name).
#' @param rdm_token REDCap API token (test or prod — caller's choice).
#' @param redcap_url REDCap API base URL.
#' @export
mod_peer_review_entry_server <- function(id, resident_id, all_residents_r, rdm_token, redcap_url) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    step        <- reactiveVal("search")   # "search" | "form" | "done"
    target_res  <- reactiveVal(NULL)       # data.frame row: record_id, name
    n_completed <- reactiveVal(NA_integer_)

    reset_form <- function() { target_res(NULL); step("search") }

    # Refresh the completed-count once per login (cheap, minimal-field query)
    observeEvent(resident_id(), {
      n_completed(.peer_count_completed(resident_id(), redcap_url, rdm_token))
    }, ignoreNULL = TRUE)

    # ── Your Peer Review Results ─────────────────────────────────────────────
    # Own reviews-received, fetched once per login. Revealed subset (per the
    # quarter-anchored >=5 rule) and its shuffled row order are both computed
    # once here and held — NOT recomputed on every render — since the
    # underlying "which reviews are revealed" fact doesn't change within a
    # session, and reshuffling on every re-render would add jank without any
    # real privacy benefit (the set itself carries no chronological signal
    # once order is randomized once).
    own_reviews    <- reactiveVal(data.frame())
    revealed_order <- reactiveVal(integer(0))  # shuffled row-order into own_reviews()

    observeEvent(resident_id(), {
      df <- .peer_fetch_own_reviews(resident_id(), redcap_url, rdm_token)
      own_reviews(df)
      if (nrow(df) == 0) {
        revealed_order(integer(0))
        return()
      }
      idx <- .peer_reveal_eligible_dates(df$peer_date)
      revealed_order(sample(idx))
    }, ignoreNULL = TRUE)

    revealed_df <- reactive({
      idx <- revealed_order()
      df  <- own_reviews()
      if (length(idx) == 0) return(df[0, , drop = FALSE])
      df[idx, , drop = FALSE]
    })

    output$results_ui <- renderUI({
      rdf <- revealed_df()
      if (nrow(rdf) < 5) {
        return(tags$p(class = "text-muted fst-italic", style = "font-size:0.85rem;",
                      "Not enough peer reviews yet to show results. Check back later."))
      }
      avg_rows <- lapply(.PEER_ITEMS, function(it) {
        vals <- suppressWarnings(as.numeric(rdf[[it$field]]))
        vals[vals == 9] <- NA_real_   # "Not observed" excluded from the average
        n_rated <- sum(!is.na(vals))
        avg <- if (n_rated > 0) round(mean(vals, na.rm = TRUE), 1) else NA_real_
        tags$tr(
          tags$td(it$label, style = "font-size:0.85rem;"),
          tags$td(if (is.na(avg)) "—" else avg,
                  style = "font-weight:700; text-align:center; color:var(--ssm-primary-blue);"),
          tags$td(n_rated, style = "text-align:center; color:#888; font-size:0.8rem;")
        )
      })
      tagList(
        tags$table(class = "table table-sm mb-4",
          tags$thead(tags$tr(
            tags$th("Question"),
            tags$th("Average (1–5)", style = "text-align:center;"),
            tags$th("# Rated", style = "text-align:center;")
          )),
          tags$tbody(avg_rows)
        ),
        tags$p(class = "text-muted mb-2",
               style = "font-size:0.75rem; text-transform:uppercase; letter-spacing:.07em;",
               "Comments (order randomized — not chronological)")
      )
    })

    output$comments_table <- DT::renderDataTable({
      rdf <- revealed_df()
      req(nrow(rdf) >= 5)
      DT::datatable(
        data.frame(
          "What they do well"      = rdf$peer_plus,
          "Where they can improve" = rdf$peer_delta,
          check.names = FALSE, stringsAsFactors = FALSE
        ),
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE)
      )
    })

    output$summary_strip <- renderUI({
      n <- n_completed()
      div(class = "row g-3 mb-1",
        div(class = "col-sm-4",
          div(class = "card text-center border-0 shadow-sm py-3 px-2",
              style = "background:#fff; border-radius:8px;",
              tags$i(class = "bi bi-clipboard2-check-fill",
                     style = "font-size:1.5rem; color:var(--ssm-primary-blue); opacity:.85;"),
              tags$p(class = "mb-0 mt-2",
                     style = "font-size:1.8rem; font-weight:700; color:var(--ssm-primary-blue); line-height:1.1;",
                     if (is.na(n)) "—" else n),
              tags$p(class = "mb-0 mt-1",
                     style = "font-size:0.72rem; color:var(--ssm-text-muted); text-transform:uppercase; letter-spacing:.07em;",
                     "Peer Reviews Completed")
          )
        )
      )
    })

    # ── Eligible resident roster (excludes self + known placeholders) ────────
    eligible_residents <- reactive({
      req(all_residents_r(), resident_id())
      df <- all_residents_r()
      req(is.data.frame(df), "record_id" %in% names(df), "name" %in% names(df))
      df[!(df$record_id %in% c(as.character(resident_id()), .PEER_NON_REVIEWABLE_IDS)), , drop = FALSE]
    })

    # ── Recently-worked-with quick picks (Amion, last 14 days) ────────────────
    # Best-effort convenience only — quietly empty (no error shown) if the
    # resident has no Amion match or the fetch fails, same fallback style
    # used throughout the Schedule tab's other amiontools modules.
    recent_teammates <- reactive({
      req(resident_id())
      tryCatch(
        get_recent_teammates(
          resident_id = resident_id(), rdm_token = rdm_token, redcap_url = redcap_url, days = 14
        ),
        error = function(e) data.frame(record_id = character(0), name = character(0),
                                        team = character(0), last_shared_date = as.Date(character(0)))
      )
    })

    output$recent_teammates_ui <- renderUI({
      rt <- recent_teammates()
      elig_ids <- eligible_residents()$record_id
      rt <- rt[rt$record_id %in% elig_ids, , drop = FALSE]
      if (nrow(rt) == 0) return(NULL)
      tagList(
        tags$p(style = "font-size:0.78rem; color:#888; margin-bottom:6px;",
               "Recently worked with (last 14 days):"),
        div(class = "mb-3",
          lapply(seq_len(nrow(rt)), function(i) {
            r <- rt[i, ]
            tags$button(
              type = "button", class = "btn btn-sm btn-outline-secondary me-2 mb-2",
              onclick = sprintf("Shiny.setInputValue('%s',%s,{priority:'event'})",
                                ns("quick_pick"), i),
              paste0(r$name, " — ", r$team)
            )
          })
        )
      )
    })

    observeEvent(input$quick_pick, {
      rt <- recent_teammates()
      i  <- input$quick_pick
      req(i >= 1, i <= nrow(rt))
      row <- eligible_residents()[eligible_residents()$record_id == rt$record_id[i], , drop = FALSE]
      req(nrow(row) > 0)
      target_res(row[1, ])
      step("form")
    })

    # ── Free-text search (separate output so typing doesn't lose focus) ──────
    output$search_results <- renderUI({
      term <- trimws(input$res_search %||% "")
      if (nchar(term) < 2)
        return(tags$p(style = "font-size:0.82rem; color:#888; margin:0;",
                      "Type at least 2 characters…"))
      roster <- eligible_residents()
      hits <- roster[grepl(term, roster$name, ignore.case = TRUE), , drop = FALSE]
      hits <- head(hits, 10)
      if (nrow(hits) == 0)
        return(tags$p(style = "font-size:0.82rem; color:#888; margin:0;", "No resident found."))
      div(class = "list-group mt-2",
        lapply(seq_len(nrow(hits)), function(i) {
          r <- hits[i, ]
          tags$button(
            type = "button", class = "list-group-item list-group-item-action py-2",
            style = "font-size:0.88rem; text-align:left;",
            onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                              ns("res_pick"), r$record_id),
            r$name
          )
        })
      )
    })

    observeEvent(input$res_pick, {
      row <- eligible_residents()[eligible_residents()$record_id == input$res_pick, , drop = FALSE]
      req(nrow(row) > 0)
      target_res(row[1, ])
      step("form")
    })

    # ── Submit ────────────────────────────────────────────────────────────────
    observeEvent(input$btn_submit, {
      tgt <- target_res(); req(tgt)
      missing_items <- Filter(function(it) is.null(input[[it$field]]), .PEER_ITEMS)
      plus_txt  <- trimws(input$peer_plus  %||% "")
      delta_txt <- trimws(input$peer_delta %||% "")
      missing_comments <- !nzchar(plus_txt) || !nzchar(delta_txt)

      if (length(missing_items) > 0 || missing_comments) {
        msg <- if (length(missing_items) > 0 && missing_comments)
                 "Please answer all 5 questions and fill in both comment boxes before submitting."
               else if (length(missing_items) > 0)
                 "Please answer all 5 questions before submitting."
               else
                 "Please fill in both comment boxes before submitting."
        showNotification(msg, type = "warning")
        return()
      }
      fields <- setNames(
        lapply(.PEER_ITEMS, function(it) input[[it$field]]),
        vapply(.PEER_ITEMS, function(it) it$field, character(1))
      )
      fields$peer_plus  <- plus_txt
      fields$peer_delta <- delta_txt

      tryCatch({
        .peer_submit(tgt$record_id, resident_id(), fields, redcap_url, rdm_token)
        n_completed(.peer_count_completed(resident_id(), redcap_url, rdm_token))
        step("done")
      }, error = function(e) {
        showNotification(paste("Submission failed:", e$message), type = "error", duration = 8)
      })
    })

    observeEvent(input$btn_cancel,        { reset_form() })
    observeEvent(input$btn_review_another, { reset_form() })

    # ── Main panel (search / form / done) ─────────────────────────────────────
    output$main_ui <- renderUI({
      s <- step()

      if (s == "search") {
        return(tagList(
          uiOutput(ns("recent_teammates_ui")),
          div(class = "mb-2",
              textInput(ns("res_search"), label = NULL,
                        placeholder = "Type a resident's name…", width = "100%")),
          uiOutput(ns("search_results"))
        ))
      }

      if (s == "form") {
        tgt <- target_res(); req(tgt)
        return(tagList(
          tags$p(
            tags$span("Reviewing: ", style = "color:#888; font-size:0.85rem;"),
            tags$span(tgt$name, style = "font-weight:700; color:var(--ssm-primary-blue);")
          ),
          tags$hr(style = "margin:10px 0 16px;"),
          lapply(.PEER_ITEMS, function(it) .peer_rating_q(ns, it$field, it$label)),
          tags$hr(style = "margin:10px 0 16px;"),
          textAreaInput(ns("peer_plus"), "What does this resident do well? *",
                       placeholder = "Required", rows = 3, width = "100%"),
          textAreaInput(ns("peer_delta"), "Where can this resident improve? *",
                       placeholder = "Required", rows = 3, width = "100%"),
          div(class = "d-flex gap-2 mt-3",
            actionButton(ns("btn_submit"), "Submit Review", class = "btn-primary"),
            actionButton(ns("btn_cancel"), "Cancel", class = "btn-outline-secondary")
          )
        ))
      }

      if (s == "done") {
        return(tagList(
          div(class = "text-center py-4",
            tags$i(class = "bi bi-check-circle-fill",
                   style = "font-size:2.5rem; color:var(--ssm-success-green, #2e7d32);"),
            tags$p(class = "mt-3", style = "font-weight:600;", "Peer review submitted."),
            actionButton(ns("btn_review_another"), "Review Someone Else", class = "btn-primary mt-2")
          )
        ))
      }
    })

  })
}

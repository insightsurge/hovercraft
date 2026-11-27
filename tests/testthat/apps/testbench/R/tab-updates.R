# Table updates: what DT and reactable do to tip_table() when the data changes, and what DT's own inputs say with and
# without it. Four sections, each with the ids its checks use (all start with `updates_`). They are not behaviour
# scenarios, so they have no spot: test-practice-table-browser.R drives them. Open this tab alone with ?only=updates.

upd_scores <- data.frame(
  team = c("Owls", "Foxes", "Hares", "Lynx"),
  wins = c(7, 5, 2, 9),
  note = c("a", NA, "c", "d")
)
upd_updated <- data.frame(
  team = c("Zürich ✓", "Hawks", "Wrens", "Kites", "Extra"),
  wins = c(12, 8, 4, 15, 3),
  note = c("n1", "n2", NA, "n4", "n5")
)
upd_renamed <- transform(upd_updated, team = paste0("N-", team))

# The four DT tables of the inputs section: with and without tip_table(), with and without row names.
upd_inputs_tables <- c(
  tips = "updates_inputs_dts",
  plain = "updates_inputs_dtp",
  tips_names = "updates_inputs_dtsn",
  plain_names = "updates_inputs_dtpn"
)
upd_inputs_names <- stats::setNames(c(FALSE, FALSE, TRUE, TRUE), unname(upd_inputs_tables))

# Firms whose names hold characters that DT escapes before the browser sees a row.
upd_firms <- data.frame(
  name = c("AT&T", "<b>x</b>", "Q>5", "say \"hi\" it's", "literal &amp; kept"),
  tag = c("a&b", "<i>", "c", "d", "e"),
  stringsAsFactors = FALSE
)

tab_updates_ui <- function() {
  htmltools::tagList(
    htmltools::tags$h3(class = "h6", "Proxies: DT::replaceData() and reactable::updateReactable()"),
    shiny::actionButton("updates_proxy_replace_dt", "Replace DT"),
    shiny::actionButton("updates_proxy_update_rt", "Update reactable"),
    DT::DTOutput("updates_proxy_dt"),
    reactable::reactableOutput("updates_proxy_rt"),
    shiny::verbatimTextOutput("updates_proxy_rt_sel"),
    htmltools::tags$h3(class = "h6", "The tips follow the data: server and client DT, and reactable"),
    shiny::actionButton("updates_follow_replace", "Replace"),
    DT::DTOutput("updates_follow_dt_server"),
    DT::DTOutput("updates_follow_dt_client"),
    reactable::reactableOutput("updates_follow_rt"),
    htmltools::tags$h3(class = "h6", "DT's own inputs, with and without tip_table()"),
    shiny::actionButton("updates_inputs_replace", "Replace"),
    shiny::actionButton("updates_inputs_replace_again", "Replace again"),
    lapply(unname(upd_inputs_tables), DT::DTOutput),
    shiny::verbatimTextOutput("updates_inputs_report"),
    shiny::verbatimTextOutput("updates_inputs_report_names"),
    htmltools::tags$h3(class = "h6", "DT's escaping of the data"),
    DT::DTOutput("updates_escape_client"),
    DT::DTOutput("updates_escape_server"),
    DT::DTOutput("updates_escape_server_names"),
    DT::DTOutput("updates_escape_server_off"),
    DT::DTOutput("updates_escape_server_name_only"),
    DT::DTOutput("updates_escape_client_name_only")
  )
}

tab_updates_server <- function(input, output, session) {
  # Proxies.
  proxy_scores <- upd_scores[c("team", "wins")]
  proxy_updated <- data.frame(team = c("Eagles", "Hawks", "Wrens", "Kites"), wins = c(12, 8, 4, 15))
  output$updates_proxy_dt <- DT::renderDT(
    hovercraft::tip_table(
      DT::datatable(proxy_scores, rownames = FALSE, options = list(pageLength = 2)),
      rows = "**{{ team }}**: {{ wins | number:0 }} wins"
    )
  )
  shiny::observeEvent(input$updates_proxy_replace_dt, {
    DT::replaceData(DT::dataTableProxy("updates_proxy_dt"), proxy_updated, rownames = FALSE)
  })
  output$updates_proxy_rt <- reactable::renderReactable(
    hovercraft::tip_table(
      reactable::reactable(proxy_scores, defaultPageSize = 2, selection = "single"),
      rows = "**{{ team }}**: {{ wins | number:0 }} wins"
    )
  )
  shiny::observeEvent(input$updates_proxy_update_rt, {
    reactable::updateReactable("updates_proxy_rt", data = proxy_updated)
  })
  output$updates_proxy_rt_sel <- shiny::renderText(
    paste("selected:", paste(reactable::getReactableState("updates_proxy_rt", "selected"), collapse = ","))
  )

  # The tips follow the data.
  follow_tips <- function(widget, id) {
    hovercraft::tip_table(
      widget,
      rows = "{{ team }}: {{ wins | number:0 }} wins",
      cells = c(note = "Note: {{ note }}"),
      id = id
    )
  }
  follow_dt <- function(data) DT::datatable(data, rownames = FALSE, options = list(pageLength = 3))
  output$updates_follow_dt_server <- DT::renderDT(
    follow_tips(follow_dt(upd_scores), "updates_follow_dts"),
    server = TRUE
  )
  output$updates_follow_dt_client <- DT::renderDT(
    follow_tips(follow_dt(upd_scores), "updates_follow_dtc"),
    server = FALSE
  )
  output$updates_follow_rt <- reactable::renderReactable(
    follow_tips(reactable::reactable(upd_scores, defaultPageSize = 3), "updates_follow_rt")
  )
  shiny::observeEvent(input$updates_follow_replace, {
    DT::replaceData(DT::dataTableProxy("updates_follow_dt_server"), upd_updated, rownames = FALSE)
    reactable::updateReactable("updates_follow_rt", data = upd_updated)
  })

  # DT's inputs with and without tip_table().
  make_inputs_table <- function(id) {
    widget <- DT::datatable(
      upd_scores,
      rownames = upd_inputs_names[[id]],
      filter = "top",
      options = list(pageLength = 3, stateSave = TRUE)
    )
    if (id %in% upd_inputs_tables[c("tips", "tips_names")]) {
      hovercraft::tip_table(widget, rows = "{{ team }}: {{ wins | number:0 }} wins", id = id)
    } else {
      widget
    }
  }
  for (id in unname(upd_inputs_tables)) {
    local({
      key <- id
      output[[key]] <- DT::renderDT(make_inputs_table(key), server = TRUE)
    })
  }
  shiny::observeEvent(input$updates_inputs_replace, {
    for (id in unname(upd_inputs_tables)) {
      proxy <- DT::dataTableProxy(id)
      DT::replaceData(proxy, upd_updated, rownames = upd_inputs_names[[id]])
      DT::selectRows(proxy, 2)
    }
  })
  shiny::observeEvent(input$updates_inputs_replace_again, {
    for (id in unname(upd_inputs_tables)) {
      DT::replaceData(DT::dataTableProxy(id), upd_renamed, rownames = upd_inputs_names[[id]], resetPaging = FALSE)
    }
  })
  what <- c(
    "rows_all",
    "rows_current",
    "rows_selected",
    "search",
    "search_columns",
    "columns_selected",
    "cells_selected",
    "cell_clicked",
    "row_last_clicked",
    "state"
  )
  snapshot <- function(id) {
    got <- stats::setNames(lapply(what, function(w) input[[paste0(id, "_", w)]]), what)
    got$state$time <- NULL
    got
  }
  # One line: which inputs differ between the tipped and the plain table, how many columns each reports, the rows on the
  # page and the selected rows of the tipped one.
  compare <- function(with, without) {
    a <- snapshot(with)
    b <- snapshot(without)
    flags <- vapply(what, function(w) paste0(w, "=", identical(a[[w]], b[[w]])), "")
    paste(
      c(
        flags,
        paste0("cols=", length(a$search_columns), "/", length(b$search_columns)),
        paste0("statecols=", length(a$state$columns), "/", length(b$state$columns)),
        paste0("page=", paste(a$rows_current, collapse = ",")),
        paste0("selected=", paste(a$rows_selected, collapse = ","))
      ),
      collapse = " "
    )
  }
  output$updates_inputs_report <- shiny::renderText(compare(upd_inputs_tables[["tips"]], upd_inputs_tables[["plain"]]))
  output$updates_inputs_report_names <- shiny::renderText(
    compare(upd_inputs_tables[["tips_names"]], upd_inputs_tables[["plain_names"]])
  )

  # Escaping.
  escaped <- function(rownames, ..., server = TRUE) {
    DT::renderDT(
      hovercraft::tip_table(
        DT::datatable(upd_firms, rownames = rownames, options = list(dom = "t"), ...),
        rows = "{{ name }} / {{ tag }}"
      ),
      server = server
    )
  }
  output$updates_escape_client <- escaped(FALSE, server = FALSE)
  output$updates_escape_server <- escaped(FALSE)
  output$updates_escape_server_names <- escaped(TRUE)
  output$updates_escape_server_off <- escaped(FALSE, escape = FALSE)
  output$updates_escape_server_name_only <- escaped(FALSE, escape = "name")
  output$updates_escape_client_name_only <- escaped(FALSE, escape = "name", server = FALSE)
}

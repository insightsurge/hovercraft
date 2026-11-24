skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if_not_installed("DT")
skip_if_not_installed("reactable")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# Table updates and proxies in a running Shiny app: DT::replaceData(), reactable::updateReactable(),
# and reactable's own sort/paging/selection events with tip_table().
describe("table proxies and updates in a Shiny app", {
  path <- find.package("hovercraft")
  port <- httpuv::randomPort()
  app <- callr::r_bg(
    function(port, path) {
      if (dir.exists(file.path(path, "inst"))) {
        pkgload::load_all(path, quiet = TRUE)
      } else {
        library(hovercraft)
      }
      scores <- data.frame(
        team = c("Owls", "Foxes", "Hares", "Lynx"),
        wins = c(7, 5, 2, 9)
      )
      updated <- data.frame(
        team = c("Eagles", "Hawks", "Wrens", "Kites"),
        wins = c(12, 8, 4, 15)
      )
      ui <- shiny::fluidPage(
        shiny::actionButton("replace_dt", "Replace DT"),
        shiny::actionButton("update_rt", "Update reactable"),
        DT::DTOutput("dt"),
        reactable::reactableOutput("rt"),
        shiny::verbatimTextOutput("rt_sel")
      )
      server <- function(input, output, session) {
        output$dt <- DT::renderDT(
          tip_table(
            DT::datatable(scores, rownames = FALSE, options = list(pageLength = 2)),
            rows = "**{{ team }}**: {{ wins | number:0 }} wins"
          )
        )
        shiny::observeEvent(input$replace_dt, {
          DT::replaceData(DT::dataTableProxy("dt"), updated, rownames = FALSE)
        })
        output$rt <- reactable::renderReactable(
          tip_table(
            reactable::reactable(scores, defaultPageSize = 2, selection = "single"),
            rows = "**{{ team }}**: {{ wins | number:0 }} wins"
          )
        )
        shiny::observeEvent(input$update_rt, {
          reactable::updateReactable("rt", data = updated)
        })
        output$rt_sel <- shiny::renderText(
          paste("selected:", paste(reactable::getReactableState("rt", "selected"), collapse = ","))
        )
      }
      shiny::runApp(shiny::shinyApp(ui, server), port = port, host = "127.0.0.1", launch.browser = FALSE)
    },
    args = list(port = port, path = path),
    supervise = TRUE
  )
  withr::defer(app$kill())
  url <- paste0("http://127.0.0.1:", port)
  up <- FALSE
  for (i in 1:100) {
    up <- !is.null(tryCatch(suppressWarnings(readLines(url, n = 1L)), error = function(e) NULL))
    if (up || !app$is_alive()) {
      break
    }
    Sys.sleep(0.1)
  }
  page <- local_page(url, environment())
  press_button <- function(id) page$evaluate(sprintf("document.getElementById('%s').click()", id))
  hover_in_view <- function(selector) {
    page$evaluate(sprintf(
      "document.querySelector(%s)?.scrollIntoView({ block: 'center', behavior: 'instant' }); 0",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    ))
    Sys.sleep(0.1)
    page$hover(selector)
  }

  it("starts and connects", {
    expect_true(up, info = paste(app$read_error(), collapse = "\n"))
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
  })

  it("keeps reactable row selection working alongside tip_table()", {
    expect_true(page$wait_until("document.querySelectorAll('#rt .rt-tbody .rt-tr-group [role=row]').length >= 2"))
    expect_identical(hover_in_view("#rt .rt-tbody .rt-tr-group:first-child .rt-td:nth-child(2)"), "Owls: 7 wins")
    page$evaluate("document.querySelector('#rt .rt-tbody .rt-tr-group:first-child input[type=radio]').click()")
    expect_true(page$wait_until("document.getElementById('rt_sel')?.textContent?.trim() === 'selected: 1'"))
  })

  it("updates DT tips after DT::replaceData()", {
    expect_true(page$wait_until("document.querySelectorAll('#dt tbody tr').length === 2"))
    expect_identical(hover_in_view("#dt tbody tr:first-child td:first-child"), "Owls: 7 wins")
    press_button("replace_dt")
    expect_true(page$wait_until("document.querySelector('#dt tbody td')?.textContent?.startsWith('Eagles')"))
    expect_identical(hover_in_view("#dt tbody tr:first-child td:first-child"), "Eagles: 12 wins")
  })

  it("updates reactable tips after reactable::updateReactable()", {
    expect_true(page$wait_until("document.querySelectorAll('#rt .rt-tbody .rt-tr-group [role=row]').length >= 2"))
    press_button("update_rt")
    expect_true(page$wait_until("document.querySelector('#rt .rt-tbody')?.textContent?.includes('Eagles')"))
    expect_identical(hover_in_view("#rt .rt-tbody .rt-tr-group:first-child .rt-td:nth-child(2)"), "Eagles: 12 wins")
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

# A table that its widget updates with new data keeps tips that follow the data: DT in server mode (`replaceData()` only
# works there), reactable (which reuses its row nodes), and DT's client mode for the data row of a tip.
describe("tip_table() follows the data when the table is updated", {
  made <- start_app(function() {
    scores <- data.frame(
      team = c("Owls", "Foxes", "Hares", "Lynx"),
      wins = c(7, 5, 2, 9),
      note = c("a", NA, "c", "d")
    )
    updated <- data.frame(
      team = c("Zürich ✓", "Hawks", "Wrens", "Kites", "Extra"),
      wins = c(12, 8, 4, 15, 3),
      note = c("n1", "n2", NA, "n4", "n5")
    )
    tips <- function(widget, id) {
      tip_table(widget, rows = "{{ team }}: {{ wins | number:0 }} wins", cells = c(note = "Note: {{ note }}"), id = id)
    }
    dt <- function(data, ...) DT::datatable(data, rownames = FALSE, options = list(pageLength = 3), ...)
    ui <- shiny::fluidPage(
      shiny::actionButton("replace", "Replace"),
      DT::DTOutput("dt_server"),
      DT::DTOutput("dt_client"),
      reactable::reactableOutput("rt")
    )
    server <- function(input, output, session) {
      output$dt_server <- DT::renderDT(tips(dt(scores), "dts"), server = TRUE)
      output$dt_client <- DT::renderDT(tips(dt(scores), "dtc"), server = FALSE)
      output$rt <- reactable::renderReactable(tips(reactable::reactable(scores, defaultPageSize = 3), "rt"))
      shiny::observeEvent(input$replace, {
        DT::replaceData(DT::dataTableProxy("dt_server"), updated, rownames = FALSE)
        reactable::updateReactable("rt", data = updated)
      })
    }
    shiny::shinyApp(ui, server)
  })
  page <- local_page(made$url, environment())
  hover_in_view <- function(selector) {
    page$evaluate(sprintf(
      "document.querySelector(%s)?.scrollIntoView({ block: 'center', behavior: 'instant' }); 0",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    ))
    page$hover(selector)
  }
  id_attr <- tip_attribute("id")
  id_of <- function(selector) {
    page$evaluate(sprintf(
      "document.querySelector(%s)?.getAttribute('%s')",
      jsonlite::toJSON(selector, auto_unbox = TRUE),
      id_attr
    ))
  }
  first_cell <- function(table) sprintf("#%s tbody tr:first-child td:first-child", table)
  note_cell <- function(table) sprintf("#%s tbody tr:first-child td:nth-child(3)", table)
  first_starts <- function(table, text) {
    sprintf("document.querySelector('#%s tbody td')?.textContent?.startsWith('%s')", table, text)
  }
  draw <- function(table, call) page$evaluate(sprintf("$('#%s table').DataTable().%s.draw(false); 0", table, call))

  it("starts and connects", {
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    for (table in c("dt_server", "dt_client")) {
      expect_true(
        page$wait_until(sprintf("document.querySelectorAll('#%s tbody tr').length === 3", table)),
        info = table
      )
    }
    expect_true(page$wait_until("document.querySelectorAll('#rt .rt-tbody [role=row]').length === 3"))
  })

  it("shows each DT row's own tip, and its derived id, in server mode while the browser holds one page", {
    draw("dt_server", "order([1, 'desc'])")
    expect_true(page$wait_until(first_starts("dt_server", "Lynx")))
    expect_identical(hover_in_view(first_cell("dt_server")), "Lynx: 9 wins")
    # Lynx is the fourth row of the data, whatever its place on the page.
    expect_identical(id_of("#dt_server tbody tr:first-child"), "dts/row/4")
  })

  it("follows DT::replaceData() with the user's own data frame while a sort is active, in server mode", {
    page$evaluate("document.getElementById('replace').click()")
    # Still sorted by wins, high to low: Kites (15) leads the new data.
    expect_true(page$wait_until(first_starts("dt_server", "Kites")))
    expect_identical(hover_in_view(first_cell("dt_server")), "Kites: 15 wins")
    expect_identical(hover_in_view(note_cell("dt_server")), "Note: n4")
    expect_identical(id_of("#dt_server tbody tr:first-child"), "dts/row/4")
  })

  it("keeps following the new data after a change of page, in server mode", {
    draw("dt_server", "page(1)")
    # By wins, high to low: Kites, Zürich ✓, Hawks | Wrens, Extra.
    expect_true(page$wait_until(first_starts("dt_server", "Wrens")))
    expect_identical(hover_in_view(first_cell("dt_server")), "Wrens: 4 wins")
    expect_identical(id_of("#dt_server tbody tr:first-child"), "dts/row/3")
  })

  it("gives a DT cell that holds NA no tip after the update, in server mode", {
    draw("dt_server", "order([0, 'asc']).page(1)")
    # Sorted by team: Extra, Hawks, Kites | Wrens (note NA), Zürich ✓.
    expect_true(page$wait_until(first_starts("dt_server", "Wrens")))
    template <- "document.querySelector('#dt_server tbody tr:nth-child(%d)').children[2].hasAttribute('%s')"
    expect_false(page$evaluate(sprintf(template, 1L, tip_attribute("template"))))
    expect_true(page$evaluate(sprintf(template, 2L, tip_attribute("template"))))
  })

  it("shows each row's own tip and derived id in client mode, where DT's row index is the data row", {
    draw("dt_client", "order([1, 'desc'])")
    expect_true(page$wait_until(first_starts("dt_client", "Lynx")))
    expect_identical(hover_in_view(first_cell("dt_client")), "Lynx: 9 wins")
    expect_identical(id_of("#dt_client tbody tr:first-child"), "dtc/row/4")
    draw("dt_client", "order([0, 'asc'])")
    # Foxes holds NA in its note: no tip on that cell.
    expect_true(page$wait_until(first_starts("dt_client", "Foxes")))
    expect_false(page$evaluate(sprintf(
      "document.querySelector('#dt_client tbody tr:first-child').children[2].hasAttribute('%s')",
      tip_attribute("template")
    )))
  })

  it("follows reactable::updateReactable(), though reactable reuses its row nodes", {
    expect_true(page$wait_until("document.querySelector('#rt .rt-tbody')?.textContent?.includes('Hawks')"))
    expect_identical(hover_in_view("#rt .rt-tbody .rt-tr-group:first-child .rt-td:nth-child(1)"), "Zürich ✓: 12 wins")
    expect_identical(hover_in_view("#rt .rt-tbody .rt-tr-group:first-child .rt-td:nth-child(3)"), "Note: n1")
    expect_identical(id_of("#rt .rt-tbody .rt-tr-group:first-child [role=row]"), "rt/row/1")
    # The new data has one more row than the old: the next page holds the fourth and the fifth.
    page$evaluate("[...document.querySelectorAll('#rt button')].find((b) => /next/i.test(b.textContent)).click(); 0")
    expect_true(page$wait_until("document.querySelector('#rt .rt-tbody')?.textContent?.includes('Extra')"))
    expect_identical(hover_in_view("#rt .rt-tbody .rt-tr-group:first-child .rt-td:nth-child(1)"), "Kites: 15 wins")
    expect_identical(id_of("#rt .rt-tbody .rt-tr-group:first-child [role=row]"), "rt/row/4")
    expect_identical(hover_in_view("#rt .rt-tbody .rt-tr-group:nth-child(2) .rt-td:nth-child(1)"), "Extra: 3 wins")
    expect_identical(id_of("#rt .rt-tbody .rt-tr-group:nth-child(2) [role=row]"), "rt/row/5")
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

# DT's inputs and the table's own state are the same with and without tip_table(), before and after DT::replaceData()
# (with and without a reset of the page), with a sort active and a row selected, with and without row names. The tables
# hold the same data; a tipped table and a plain one are given the same actions, and the app says which inputs differ.
describe("tip_table() leaves DT's inputs as they are without it", {
  made <- start_app(function() {
    scores <- data.frame(
      team = c("Owls", "Foxes", "Hares", "Lynx"),
      wins = c(7, 5, 2, 9),
      note = c("a", NA, "c", "d")
    )
    updated <- data.frame(
      team = c("Zürich ✓", "Hawks", "Wrens", "Kites", "Extra"),
      wins = c(12, 8, 4, 15, 3),
      note = c("n1", "n2", NA, "n4", "n5")
    )
    renamed <- transform(updated, team = paste0("N-", team))
    tables <- c(tips = "dts", plain = "dtp", tips_names = "dtsn", plain_names = "dtpn")
    has_names <- c(dts = FALSE, dtp = FALSE, dtsn = TRUE, dtpn = TRUE)
    ui <- shiny::fluidPage(
      shiny::actionButton("replace", "Replace"),
      shiny::actionButton("replace_again", "Replace again"),
      lapply(unname(tables), DT::DTOutput),
      shiny::verbatimTextOutput("report"),
      shiny::verbatimTextOutput("report_names")
    )
    server <- function(input, output, session) {
      make <- function(id) {
        widget <- DT::datatable(
          scores,
          rownames = has_names[[id]],
          filter = "top",
          options = list(pageLength = 3, stateSave = TRUE)
        )
        if (id %in% c("dts", "dtsn")) {
          tip_table(widget, rows = "{{ team }}: {{ wins | number:0 }} wins", id = id)
        } else {
          widget
        }
      }
      for (id in unname(tables)) {
        local({
          key <- id
          output[[key]] <- DT::renderDT(make(key), server = TRUE)
        })
      }
      shiny::observeEvent(input$replace, {
        for (id in unname(tables)) {
          proxy <- DT::dataTableProxy(id)
          DT::replaceData(proxy, updated, rownames = has_names[[id]])
          DT::selectRows(proxy, 2)
        }
      })
      shiny::observeEvent(input$replace_again, {
        for (id in unname(tables)) {
          DT::replaceData(DT::dataTableProxy(id), renamed, rownames = has_names[[id]], resetPaging = FALSE)
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
      # One line: which inputs differ between the tipped and the plain table, how many columns each reports, the rows on
      # the page and the selected rows of the tipped one.
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
      output$report <- shiny::renderText(compare("dts", "dtp"))
      output$report_names <- shiny::renderText(compare("dtsn", "dtpn"))
    }
    shiny::shinyApp(ui, server)
  })
  page <- local_page(made$url, environment())
  report <- function(id) page$evaluate(sprintf("document.getElementById('%s')?.textContent?.trim() ?? ''", id))
  # A report line says that every input is the same and what the inputs say; the inputs of the two tables arrive one
  # after the other, so a check waits until they agree rather than reading the line once.
  # `cols` is the number of columns the table says it has: row names count in its state, not in its column searches.
  wait_for <- function(id, cols, page_rows, selected = "") {
    searched <- if (id == "report_names") cols - 1L else cols
    want <- sprintf(
      "cols=%d/%d statecols=%d/%d page=%s selected=%s$",
      searched,
      searched,
      cols,
      cols,
      page_rows,
      selected
    )
    ok <- page$wait_until(sprintf(
      "(() => { const t = (document.getElementById('%s')?.textContent ?? '').trim(); return !t.includes('=false') && !t.includes('=FALSE') && new RegExp(%s).test(t) })()",
      id,
      jsonlite::toJSON(want, auto_unbox = TRUE)
    ))
    expect_true(ok, info = paste(id, report(id)))
    expect_false(grepl("=FALSE", report(id), fixed = TRUE), info = paste(id, report(id)))
  }
  # A call on every table's DataTable; `%s` is the position of the wins column, which row names move by one.
  each <- function(call) {
    for (table in c("dts", "dtp", "dtsn", "dtpn")) {
      wins <- if (table %in% c("dtsn", "dtpn")) 2L else 1L
      page$evaluate(sprintf(
        "$('#%s table').DataTable().%s; 0",
        table,
        if (grepl("%d", call, fixed = TRUE)) sprintf(call, wins) else call
      ))
    }
  }
  first_starts <- function(table, text) {
    sprintf("document.querySelector('#%s tbody td')?.textContent?.startsWith('%s')", table, text)
  }
  hover_in_view <- function(selector) {
    page$evaluate(sprintf("document.querySelector('%s').scrollIntoView({ block: 'center' }); 0", selector))
    page$hover(selector)
  }

  it("starts and connects", {
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    for (table in c("dts", "dtp", "dtsn", "dtpn")) {
      expect_true(
        page$wait_until(sprintf("document.querySelectorAll('#%s tbody tr').length === 3", table)),
        info = table
      )
    }
  })

  it("reports the same inputs and the same columns before any action", {
    wait_for("report", cols = 3, page_rows = "1,2,3")
    wait_for("report_names", cols = 4, page_rows = "1,2,3")
  })

  it("reports the same inputs with a sort active and a row selected", {
    each("order([%d, 'desc']).draw(false)")
    # By wins, high to low: Lynx (9), Owls (7), Foxes (5) | Hares (2).
    wait_for("report", cols = 3, page_rows = "4,1,2")
    wait_for("report_names", cols = 4, page_rows = "4,1,2")
    for (table in c("dts", "dtp", "dtsn", "dtpn")) {
      selector <- sprintf("#%s tbody tr:first-child td:first-child", table)
      page$evaluate(sprintf("document.querySelector('%s').scrollIntoView({ block: 'center' }); 0", selector))
      page$click(selector)
    }
    wait_for("report", cols = 3, page_rows = "4,1,2", selected = "4")
    wait_for("report_names", cols = 4, page_rows = "4,1,2", selected = "4")
  })

  it("reports the same inputs after DT::replaceData() with the user's data frame and a new selection", {
    page$evaluate("document.getElementById('replace').click()")
    # Still sorted by wins: Kites (15), Zürich ✓ (12), Hawks (8) | Wrens (4), Extra (3); row 2 selected by the app.
    wait_for("report", cols = 3, page_rows = "4,1,2", selected = "2")
    wait_for("report_names", cols = 4, page_rows = "4,1,2", selected = "2")
    expect_true(page$wait_until(first_starts("dts", "Kites")))
    expect_identical(hover_in_view("#dts tbody tr:first-child td:first-child"), "Kites: 15 wins")
    expect_identical(hover_in_view("#dtsn tbody tr:first-child td:nth-child(2)"), "Kites: 15 wins")
  })

  it("reports the same inputs after replaceData(resetPaging = FALSE) on the second page", {
    each("page(1).draw(false)")
    wait_for("report", cols = 3, page_rows = "3,5", selected = "2")
    page$evaluate("document.getElementById('replace_again').click()")
    # The page stays the second one, the new names show, and the selection is cleared.
    expect_true(page$wait_until(first_starts("dts", "N-Wrens")))
    wait_for("report", cols = 3, page_rows = "3,5")
    wait_for("report_names", cols = 4, page_rows = "3,5")
    expect_identical(hover_in_view("#dts tbody tr:first-child td:first-child"), "N-Wrens: 4 wins")
    expect_identical(
      page$evaluate(sprintf(
        "document.querySelector('#dtsn tbody tr:first-child').getAttribute('%s')",
        tip_attribute("id")
      )),
      "dtsn/row/3"
    )
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

# DT escapes the HTML in the data it draws (`escape = TRUE` by default) before the browser sees a row, and the tip must
# show the value as it is in the data frame: "AT&T", not "AT&amp;T".
describe("tip_table() shows the values of the data, not DT's escaping of them", {
  made <- start_app(function() {
    firms <- data.frame(
      name = c("AT&T", "<b>x</b>", "Q>5", "say \"hi\" it's", "literal &amp; kept"),
      tag = c("a&b", "<i>", "c", "d", "e"),
      stringsAsFactors = FALSE
    )
    tips <- function(widget) tip_table(widget, rows = "{{ name }} / {{ tag }}")
    dt <- function(rownames, ..., server = TRUE) {
      DT::renderDT(tips(DT::datatable(firms, rownames = rownames, options = list(dom = "t"), ...)), server = server)
    }
    ui <- shiny::fluidPage(
      DT::DTOutput("client"),
      DT::DTOutput("server"),
      DT::DTOutput("server_names"),
      DT::DTOutput("server_off"),
      DT::DTOutput("server_name_only"),
      DT::DTOutput("client_name_only")
    )
    server <- function(input, output, session) {
      output$client <- dt(FALSE, server = FALSE)
      output$server <- dt(FALSE)
      output$server_names <- dt(TRUE)
      output$server_off <- dt(FALSE, escape = FALSE)
      output$server_name_only <- dt(FALSE, escape = "name")
      output$client_name_only <- dt(FALSE, escape = "name", server = FALSE)
    }
    shiny::shinyApp(ui, server)
  })
  page <- local_page(made$url, environment())
  expected <- c("AT&T / a&b", "<b>x</b> / <i>", "Q>5 / c", "say \"hi\" it's / d", "literal &amp; kept / e")
  # The tip of the first cell of row `i`, in the table `id`; a table with row names has one more column first.
  tip_of <- function(id, i, names = FALSE) {
    selector <- sprintf("#%s tbody tr:nth-child(%d) td:nth-child(%d)", id, i, if (names) 2L else 1L)
    page$evaluate(sprintf(
      "document.querySelector(%s)?.scrollIntoView({ block: 'center' }); 0",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    ))
    page$hover(selector)
  }
  check <- function(id, names = FALSE) {
    expect_true(
      page$wait_until(sprintf(
        "document.querySelectorAll('#%s tbody tr[data-hovercraft-tip-template]').length === 5",
        id
      )),
      info = id
    )
    for (i in seq_along(expected)) {
      expect_identical(tip_of(id, i, names), expected[[i]], info = paste(id, i))
    }
  }

  it("starts and connects", {
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
  })

  it("shows the data as it is with DT's default escaping, in client mode and in server mode", {
    check("client")
    check("server")
  })

  it("shows the data as it is when the table has row names", {
    check("server_names", names = TRUE)
  })

  it("shows the data as it is when the table is not escaped, or only one column is", {
    check("server_off")
    check("server_name_only")
    check("client_name_only")
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

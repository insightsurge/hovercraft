skip_if_no_browser(bench_packages)
skip_if_no_bench()

# Table updates and proxies in a running Shiny app: DT::replaceData(), reactable::updateReactable(),
# and reactable's own sort/paging/selection events with tip_table().
describe("table proxies and updates in a Shiny app", {
  # The bench's table updates tab (apps/testbench/R/tab-updates.R), alone.
  bench <- start_bench(environment())
  app <- bench$proc
  url <- bench_url(bench, "updates")
  up <- wait_for_app(app, url)
  page <- local_page(url, environment())

  it("starts and connects", {
    expect_true(up, info = paste(app$read_error(), collapse = "\n"))
    expect_true(page$wait_connected())
  })

  it("keeps reactable row selection working alongside tip_table()", {
    expect_true(page$wait_until(
      "document.querySelectorAll('#updates_proxy_rt .rt-tbody .rt-tr-group [role=row]').length >= 2"
    ))
    expect_identical(
      page$hover_in_view("#updates_proxy_rt .rt-tbody .rt-tr-group:first-child .rt-td:nth-child(2)"),
      "Owls: 7 wins"
    )
    page$evaluate(
      "document.querySelector('#updates_proxy_rt .rt-tbody .rt-tr-group:first-child input[type=radio]').click()"
    )
    expect_true(page$wait_until(
      "document.getElementById('updates_proxy_rt_sel')?.textContent?.trim() === 'selected: 1'"
    ))
  })

  it("updates DT tips after DT::replaceData()", {
    expect_true(page$wait_until("document.querySelectorAll('#updates_proxy_dt tbody tr').length === 2"))
    expect_identical(page$hover_in_view("#updates_proxy_dt tbody tr:first-child td:first-child"), "Owls: 7 wins")
    page$click_id("updates_proxy_replace_dt")
    expect_true(page$wait_until(
      "document.querySelector('#updates_proxy_dt tbody td')?.textContent?.startsWith('Eagles')"
    ))
    expect_identical(page$hover_in_view("#updates_proxy_dt tbody tr:first-child td:first-child"), "Eagles: 12 wins")
  })

  it("updates reactable tips after reactable::updateReactable()", {
    expect_true(page$wait_until(
      "document.querySelectorAll('#updates_proxy_rt .rt-tbody .rt-tr-group [role=row]').length >= 2"
    ))
    page$click_id("updates_proxy_update_rt")
    expect_true(page$wait_until(
      "document.querySelector('#updates_proxy_rt .rt-tbody')?.textContent?.includes('Eagles')"
    ))
    expect_identical(
      page$hover_in_view("#updates_proxy_rt .rt-tbody .rt-tr-group:first-child .rt-td:nth-child(2)"),
      "Eagles: 12 wins"
    )
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

# A table that its widget updates with new data keeps tips that follow the data: DT in server mode (`replaceData()` only
# works there), reactable (which reuses its row nodes), and DT's client mode for the data row of a tip.
describe("tip_table() follows the data when the table is updated", {
  # The bench's table updates tab (apps/testbench/R/tab-updates.R), alone.
  made <- start_bench(environment())
  page <- local_page(bench_url(made, "updates"), environment())
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
    expect_true(page$wait_connected())
    for (table in c("updates_follow_dt_server", "updates_follow_dt_client")) {
      expect_true(
        page$wait_until(sprintf("document.querySelectorAll('#%s tbody tr').length === 3", table)),
        info = table
      )
    }
    expect_true(page$wait_until("document.querySelectorAll('#updates_follow_rt .rt-tbody [role=row]').length === 3"))
  })

  it("shows each DT row's own tip, and its derived id, in server mode while the browser holds one page", {
    draw("updates_follow_dt_server", "order([1, 'desc'])")
    expect_true(page$wait_until(first_starts("updates_follow_dt_server", "Lynx")))
    expect_identical(hover_in_view(first_cell("updates_follow_dt_server")), "Lynx: 9 wins")
    # Lynx is the fourth row of the data, whatever its place on the page.
    expect_identical(id_of("#updates_follow_dt_server tbody tr:first-child"), "updates_follow_dts/row/4")
  })

  it("follows DT::replaceData() with the user's own data frame while a sort is active, in server mode", {
    page$click_now("#updates_follow_replace")
    # Still sorted by wins, high to low: Kites (15) leads the new data.
    expect_true(page$wait_until(first_starts("updates_follow_dt_server", "Kites")))
    expect_identical(hover_in_view(first_cell("updates_follow_dt_server")), "Kites: 15 wins")
    expect_identical(hover_in_view(note_cell("updates_follow_dt_server")), "Note: n4")
    expect_identical(id_of("#updates_follow_dt_server tbody tr:first-child"), "updates_follow_dts/row/4")
  })

  it("keeps following the new data after a change of page, in server mode", {
    draw("updates_follow_dt_server", "page(1)")
    # By wins, high to low: Kites, Zürich ✓, Hawks | Wrens, Extra.
    expect_true(page$wait_until(first_starts("updates_follow_dt_server", "Wrens")))
    expect_identical(hover_in_view(first_cell("updates_follow_dt_server")), "Wrens: 4 wins")
    expect_identical(id_of("#updates_follow_dt_server tbody tr:first-child"), "updates_follow_dts/row/3")
  })

  it("gives a DT cell that holds NA no tip after the update, in server mode", {
    draw("updates_follow_dt_server", "order([0, 'asc']).page(1)")
    # Sorted by team: Extra, Hawks, Kites | Wrens (note NA), Zürich ✓.
    expect_true(page$wait_until(first_starts("updates_follow_dt_server", "Wrens")))
    template <- "document.querySelector('#updates_follow_dt_server tbody tr:nth-child(%d)').children[2].hasAttribute('%s')"
    expect_false(page$evaluate(sprintf(template, 1L, tip_attribute("template"))))
    expect_true(page$evaluate(sprintf(template, 2L, tip_attribute("template"))))
  })

  it("shows each row's own tip and derived id in client mode, where DT's row index is the data row", {
    draw("updates_follow_dt_client", "order([1, 'desc'])")
    expect_true(page$wait_until(first_starts("updates_follow_dt_client", "Lynx")))
    expect_identical(hover_in_view(first_cell("updates_follow_dt_client")), "Lynx: 9 wins")
    expect_identical(id_of("#updates_follow_dt_client tbody tr:first-child"), "updates_follow_dtc/row/4")
    draw("updates_follow_dt_client", "order([0, 'asc'])")
    # Foxes holds NA in its note: no tip on that cell.
    expect_true(page$wait_until(first_starts("updates_follow_dt_client", "Foxes")))
    expect_false(page$evaluate(sprintf(
      "document.querySelector('#updates_follow_dt_client tbody tr:first-child').children[2].hasAttribute('%s')",
      tip_attribute("template")
    )))
  })

  it("follows reactable::updateReactable(), though reactable reuses its row nodes", {
    expect_true(page$wait_until(
      "document.querySelector('#updates_follow_rt .rt-tbody')?.textContent?.includes('Hawks')"
    ))
    expect_identical(
      hover_in_view("#updates_follow_rt .rt-tbody .rt-tr-group:first-child .rt-td:nth-child(1)"),
      "Zürich ✓: 12 wins"
    )
    expect_identical(
      hover_in_view("#updates_follow_rt .rt-tbody .rt-tr-group:first-child .rt-td:nth-child(3)"),
      "Note: n1"
    )
    expect_identical(
      id_of("#updates_follow_rt .rt-tbody .rt-tr-group:first-child [role=row]"),
      "updates_follow_rt/row/1"
    )
    # The new data has one more row than the old: the next page holds the fourth and the fifth.
    page$evaluate(
      "[...document.querySelectorAll('#updates_follow_rt button')].find((b) => /next/i.test(b.textContent)).click(); 0"
    )
    expect_true(page$wait_until(
      "document.querySelector('#updates_follow_rt .rt-tbody')?.textContent?.includes('Extra')"
    ))
    expect_identical(
      hover_in_view("#updates_follow_rt .rt-tbody .rt-tr-group:first-child .rt-td:nth-child(1)"),
      "Kites: 15 wins"
    )
    expect_identical(
      id_of("#updates_follow_rt .rt-tbody .rt-tr-group:first-child [role=row]"),
      "updates_follow_rt/row/4"
    )
    expect_identical(
      hover_in_view("#updates_follow_rt .rt-tbody .rt-tr-group:nth-child(2) .rt-td:nth-child(1)"),
      "Extra: 3 wins"
    )
    expect_identical(
      id_of("#updates_follow_rt .rt-tbody .rt-tr-group:nth-child(2) [role=row]"),
      "updates_follow_rt/row/5"
    )
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

# DT's inputs and the table's own state are the same with and without tip_table(), before and after DT::replaceData()
# (with and without a reset of the page), with a sort active and a row selected, with and without row names. The tables
# hold the same data; a tipped table and a plain one are given the same actions, and the app says which inputs differ.
describe("tip_table() leaves DT's inputs as they are without it", {
  # The bench's table updates tab (apps/testbench/R/tab-updates.R), alone.
  made <- start_bench(environment())
  page <- local_page(bench_url(made, "updates"), environment())
  report <- function(id) page$evaluate(sprintf("document.getElementById('%s')?.textContent?.trim() ?? ''", id))
  # A report line says that every input is the same and what the inputs say; the inputs of the two tables arrive one
  # after the other, so a check waits until they agree rather than reading the line once.
  # `cols` is the number of columns the table says it has: row names count in its state, not in its column searches.
  wait_for <- function(id, cols, page_rows, selected = "") {
    searched <- if (id == "updates_inputs_report_names") cols - 1L else cols
    want <- sprintf(
      "cols=%d/%d statecols=%d/%d page=%s selected=%s$",
      searched,
      searched,
      cols,
      cols,
      page_rows,
      selected
    )
    started <- Sys.time()
    ok <- page$wait_until(sprintf(
      "(() => { const t = (document.getElementById('%s')?.textContent ?? '').trim(); return !t.includes('=false') && !t.includes('=FALSE') && new RegExp(%s).test(t) })()",
      id,
      jsonlite::toJSON(want, auto_unbox = TRUE)
    ))
    waited <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    # A failed wait says how long it waited, the limit it had, what it wanted and what both report lines say now, so a
    # slow page (the wait ran out while the page was still changing) can be told from inputs that really differ.
    expect_true(
      ok,
      info = sprintf(
        "%s: waited %.1f s of %.0f s; wanted /%s/; report: %s; report_names: %s",
        id,
        waited,
        browser_timeout / 1000,
        want,
        report("updates_inputs_report"),
        report("updates_inputs_report_names")
      )
    )
    # The two tables send their inputs one after the other, so the line can agree for a moment before the last one
    # arrives: read it again once the page is quiet.
    page$settle()
    expect_false(grepl("=FALSE", report(id), fixed = TRUE), info = paste(id, report(id)))
  }
  # A call on every table's DataTable; `%s` is the position of the wins column, which row names move by one.
  each <- function(call) {
    for (table in c("updates_inputs_dts", "updates_inputs_dtp", "updates_inputs_dtsn", "updates_inputs_dtpn")) {
      wins <- if (table %in% c("updates_inputs_dtsn", "updates_inputs_dtpn")) 2L else 1L
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
    page$evaluate(sprintf(
      "document.querySelector('%s').scrollIntoView({ block: 'center', behavior: 'instant' }); 0",
      selector
    ))
    page$hover(selector)
  }

  it("starts and connects", {
    expect_true(page$wait_connected())
    for (table in c("updates_inputs_dts", "updates_inputs_dtp", "updates_inputs_dtsn", "updates_inputs_dtpn")) {
      expect_true(
        page$wait_until(sprintf("document.querySelectorAll('#%s tbody tr').length === 3", table)),
        info = table
      )
    }
  })

  it("reports the same inputs and the same columns before any action", {
    wait_for("updates_inputs_report", cols = 3, page_rows = "1,2,3")
    wait_for("updates_inputs_report_names", cols = 4, page_rows = "1,2,3")
  })

  it("reports the same inputs with a sort active and a row selected", {
    each("order([%d, 'desc']).draw(false)")
    # By wins, high to low: Lynx (9), Owls (7), Foxes (5) | Hares (2).
    wait_for("updates_inputs_report", cols = 3, page_rows = "4,1,2")
    wait_for("updates_inputs_report_names", cols = 4, page_rows = "4,1,2")
    for (table in c("updates_inputs_dts", "updates_inputs_dtp", "updates_inputs_dtsn", "updates_inputs_dtpn")) {
      selector <- sprintf("#%s tbody tr:first-child td:first-child", table)
      page$evaluate(sprintf(
        "document.querySelector('%s').scrollIntoView({ block: 'center', behavior: 'instant' }); 0",
        selector
      ))
      page$click(selector, wait = 0)
    }
    wait_for("updates_inputs_report", cols = 3, page_rows = "4,1,2", selected = "4")
    wait_for("updates_inputs_report_names", cols = 4, page_rows = "4,1,2", selected = "4")
  })

  it("reports the same inputs after DT::replaceData() with the user's data frame and a new selection", {
    page$click_now("#updates_inputs_replace")
    # Still sorted by wins: Kites (15), Zürich ✓ (12), Hawks (8) | Wrens (4), Extra (3); row 2 selected by the app.
    wait_for("updates_inputs_report", cols = 3, page_rows = "4,1,2", selected = "2")
    wait_for("updates_inputs_report_names", cols = 4, page_rows = "4,1,2", selected = "2")
    expect_true(page$wait_until(first_starts("updates_inputs_dts", "Kites")))
    expect_identical(hover_in_view("#updates_inputs_dts tbody tr:first-child td:first-child"), "Kites: 15 wins")
    expect_identical(hover_in_view("#updates_inputs_dtsn tbody tr:first-child td:nth-child(2)"), "Kites: 15 wins")
  })

  it("reports the same inputs after replaceData(resetPaging = FALSE) on the second page", {
    each("page(1).draw(false)")
    wait_for("updates_inputs_report", cols = 3, page_rows = "3,5", selected = "2")
    page$click_now("#updates_inputs_replace_again")
    # The page stays the second one, the new names show, and the selection is cleared.
    expect_true(page$wait_until(first_starts("updates_inputs_dts", "N-Wrens")))
    wait_for("updates_inputs_report", cols = 3, page_rows = "3,5")
    wait_for("updates_inputs_report_names", cols = 4, page_rows = "3,5")
    expect_identical(hover_in_view("#updates_inputs_dts tbody tr:first-child td:first-child"), "N-Wrens: 4 wins")
    expect_identical(
      page$evaluate(sprintf(
        "document.querySelector('#updates_inputs_dtsn tbody tr:first-child').getAttribute('%s')",
        tip_attribute("id")
      )),
      "updates_inputs_dtsn/row/3"
    )
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

# DT escapes the HTML in the data it draws (`escape = TRUE` by default) before the browser sees a row, and the tip must
# show the value as it is in the data frame: "AT&T", not "AT&amp;T".
describe("tip_table() shows the values of the data, not DT's escaping of them", {
  # The bench's table updates tab (apps/testbench/R/tab-updates.R), alone.
  made <- start_bench(environment())
  page <- local_page(bench_url(made, "updates"), environment())
  expected <- c("AT&T / a&b", "<b>x</b> / <i>", "Q>5 / c", "say \"hi\" it's / d", "literal &amp; kept / e")
  # The tip of the first cell of row `i`, in the table `id`; a table with row names has one more column first.
  tip_of <- function(id, i, names = FALSE) {
    selector <- sprintf("#%s tbody tr:nth-child(%d) td:nth-child(%d)", id, i, if (names) 2L else 1L)
    page$evaluate(sprintf(
      "document.querySelector(%s)?.scrollIntoView({ block: 'center', behavior: 'instant' }); 0",
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
    expect_true(page$wait_connected())
  })

  it("shows the data as it is with DT's default escaping, in client mode and in server mode", {
    check("updates_escape_client")
    check("updates_escape_server")
  })

  it("shows the data as it is when the table has row names", {
    check("updates_escape_server_names", names = TRUE)
  })

  it("shows the data as it is when the table is not escaped, or only one column is", {
    check("updates_escape_server_off")
    check("updates_escape_server_name_only")
    check("updates_escape_client_name_only")
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

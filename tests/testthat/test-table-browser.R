skip_if_no_browser(c("DT", "reactable"))

# One static page per widget, with five made-up rows. The rows' own tips read the row's record, so a row must keep its
# own words after a sort or a change of page.
scores <- data.frame(
  team = c("Owls", "Foxes", "Hares", "Lynx", "Voles"),
  wins = c(7, 5, 2, 9, 1),
  note = c("Top", NA, "Late", "Best", NA)
)
rows <- "**{{ team }}**: {{ wins | number:0 }} wins"
cells <- c(note = "Note: {{ note }}")

save_page <- function(widget, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  file <- file.path(dir, "page.html")
  htmltools::save_html(widget, file, libdir = "lib")
  file
}

describe("tip_table() in a browser, on a DT table", {
  file <- save_page(
    tip_table(DT::datatable(scores, rownames = FALSE, options = list(pageLength = 2)), rows = rows, cells = cells)
  )
  page <- local_page(file, environment())

  it("shows a row's tip, and a cell's tip over it", {
    expect_true(page$wait_until("document.querySelectorAll('tbody tr[data-hovercraft-tip-template]').length === 2"))
    expect_identical(page$hover("tbody tr:first-child td:first-child"), "Owls: 7 wins")
    expect_identical(page$hover("tbody tr:first-child td:last-child"), "Note: Top")
  })

  it("keeps each row's words after a sort and after a change of page", {
    page$evaluate("$('table').DataTable().order([1, 'desc']).draw()")
    expect_identical(page$hover("tbody tr:first-child td:first-child"), "Lynx: 9 wins")
    page$evaluate("$('table').DataTable().page(1).draw(false)")
    expect_identical(page$hover("tbody tr:first-child td:first-child"), "Foxes: 5 wins")
  })

  it("gives a cell that holds NA no tip", {
    page$evaluate("$('table').DataTable().page(2).draw(false)")
    expect_identical(page$hover("tbody tr:first-child td:last-child"), "Voles: 1 wins")
    expect_identical(page$problems(), character())
  })
})

describe("tip_table() in a browser, on a reactable table", {
  file <- save_page(
    tip_table(reactable::reactable(scores, defaultPageSize = 2), rows = rows, cells = cells)
  )
  page <- local_page(file, environment())

  it("shows a row's tip, and a cell's tip over it", {
    expect_true(page$wait_until("document.querySelectorAll('[role=row][data-hovercraft-tip-template]').length >= 2"))
    expect_identical(page$hover(".rt-tbody [role=cell]:nth-child(1)"), "Owls: 7 wins")
    expect_identical(page$hover(".rt-tbody [role=cell]:nth-child(3)"), "Note: Top")
  })

  it("keeps each row's words after a sort and after a change of page", {
    page$evaluate("document.querySelectorAll('.rt-th')[1].click()")
    expect_true(page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Voles')"))
    expect_identical(page$hover(".rt-tbody [role=cell]:nth-child(1)"), "Voles: 1 wins")
    page$evaluate("[...document.querySelectorAll('button')].find((b) => /next/i.test(b.textContent)).click()")
    expect_true(page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Foxes')"))
    expect_identical(page$hover(".rt-tbody [role=cell]:nth-child(1)"), "Foxes: 5 wins")
    expect_identical(page$problems(), character())
  })
})

# The tip ids of a table with an `id`: one per marked row, cell and header cell, each carrying its own data row, so a
# sort or a change of page moves the ids with the rows. The format is the layer's own; the test only needs each id to
# be unique, to hold the table's id, and to follow the row it marks.
ids <- "JSON.stringify([...document.querySelectorAll('[data-hovercraft-tip-id]')].map((n) => n.getAttribute('data-hovercraft-tip-id')))"
id_of <- function(page, selector) {
  page$evaluate(sprintf("document.querySelector('%s').getAttribute('data-hovercraft-tip-id')", selector))
}
all_ids <- function(page) jsonlite::fromJSON(page$evaluate(ids))
settings <- list(
  rows = rows,
  cells = list(note = tip_spec("Note: {{ note }}", placement = "left")),
  headers = c(wins = "Games won"),
  id = "mod1-league_tips"
)

describe("tip_table(id =) in a browser, on a DT table", {
  file <- save_page(
    do.call(
      tip_table,
      c(list(DT::datatable(scores, rownames = FALSE, options = list(pageLength = 2))), settings)
    )
  )
  page <- local_page(file, environment())

  it("gives every row, cell and header cell a unique id that holds the table's id", {
    expect_true(page$wait_until("document.querySelectorAll('tbody tr[data-hovercraft-tip-id]').length === 2"))
    got <- all_ids(page)
    expect_length(got, 2L + 1L + 1L) # two rows, Owls' note cell (Foxes' note is NA), the wins header
    expect_false(anyDuplicated(got) > 0L)
    expect_true(all(startsWith(got, "mod1-league_tips")))
    expect_true(page$evaluate(
      "document.querySelector('thead th[data-hovercraft-tip-id]').textContent.startsWith('wins')"
    ))
  })

  it("keeps each id with its data row after a sort and a change of page", {
    first <- "tbody tr:first-child"
    id_before <- id_of(page, first)
    page$evaluate("$('table').DataTable().order([1, 'desc']).draw()")
    expect_identical(page$hover(paste(first, "td:first-child")), "Lynx: 9 wins")
    id_lynx <- id_of(page, first)
    expect_false(identical(id_lynx, id_before))
    expect_match(id_lynx, "4$")
    expect_match(id_of(page, paste(first, "td:last-child")), "4/3$")
    page$evaluate("$('table').DataTable().page(1).draw(false)")
    expect_identical(page$hover(paste(first, "td:first-child")), "Foxes: 5 wins")
    expect_match(id_of(page, first), "2$")
    expect_false(anyDuplicated(all_ids(page)) > 0L)
    expect_identical(page$problems(), character())
  })
})

describe("tip_table(id =) in a browser, on a reactable table", {
  file <- save_page(
    do.call(tip_table, c(list(reactable::reactable(scores, defaultPageSize = 2)), settings))
  )
  page <- local_page(file, environment())

  it("gives every row, cell and header cell a unique id that holds the table's id", {
    expect_true(page$wait_until("document.querySelectorAll('[role=row][data-hovercraft-tip-id]').length >= 2"))
    got <- all_ids(page)
    expect_length(got, 2L + 1L + 1L) # two rows, Owls' note cell (Foxes' note is NA), the wins header
    expect_false(anyDuplicated(got) > 0L)
    expect_true(all(startsWith(got, "mod1-league_tips")))
  })

  it("keeps each id with its data row after a sort and a change of page", {
    row <- ".rt-tbody [role=row]:first-child"
    page$evaluate("document.querySelectorAll('.rt-th')[1].click()")
    expect_true(page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Voles')"))
    expect_identical(page$hover(".rt-tbody [role=cell]:nth-child(1)"), "Voles: 1 wins")
    ends <- function(n) {
      page$wait_until(sprintf(
        "document.querySelector('%s')?.getAttribute('data-hovercraft-tip-id')?.endsWith('/%s')",
        row,
        n
      ))
    }
    expect_true(ends("row/5"))
    page$evaluate("[...document.querySelectorAll('button')].find((b) => /next/i.test(b.textContent)).click()")
    expect_true(page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Foxes')"))
    expect_true(ends("row/2"))
    expect_false(anyDuplicated(all_ids(page)) > 0L)
    expect_identical(page$problems(), character())
  })

  it("takes the id away from a cell whose tip is blank, and gives it back", {
    # Sorted by wins, three pages of two: Voles (note NA) and Hares; Foxes (NA) and Owls; Lynx. React reuses the elements.
    cell <- function(n) sprintf(".rt-tbody .rt-tr-group:nth-child(%d) [role=cell]:nth-child(3)", n)
    has <- function(selector) {
      page$evaluate(sprintf("document.querySelector('%s').hasAttribute('data-hovercraft-tip-id')", selector))
    }
    page$evaluate("[...document.querySelectorAll('button')].find((b) => /previous/i.test(b.textContent)).click()")
    expect_true(page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Voles')"))
    expect_true(page$wait_until(
      "document.querySelectorAll('.rt-tbody [role=cell][data-hovercraft-tip-id]').length > 0"
    ))
    expect_false(has(cell(1)))
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s')?.getAttribute('data-hovercraft-tip-id')?.endsWith('cell/3/3')",
      cell(2)
    )))
    page$evaluate("[...document.querySelectorAll('button')].find((b) => /next/i.test(b.textContent)).click()")
    expect_true(page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Foxes')"))
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s')?.getAttribute('data-hovercraft-tip-id')?.endsWith('cell/1/3')",
      cell(2)
    )))
    expect_false(has(cell(1)))
  })
})

# A cell's or a header cell's tip points at its text, not at the whole cell: a right-aligned number in a wide column
# used to get a tip far to its left. The first text node of the element is what the reader sees.
text_and_tip <- function(page, selector) {
  jsonlite::fromJSON(page$evaluate(sprintf(
    paste0(
      "(() => { const n = document.querySelector(%s); const w = document.createTreeWalker(n, NodeFilter.SHOW_TEXT,",
      " { acceptNode: (t) => (t.nodeValue.trim() ? 1 : 3) }); const range = document.createRange();",
      " range.selectNodeContents(w.nextNode()); const text = range.getBoundingClientRect(); const cell = n.getBoundingClientRect();",
      " const tip = [...document.querySelectorAll('.%s')].filter((t) => !t.hidden)[0].getBoundingClientRect();",
      " return JSON.stringify({ text: (text.left + text.right) / 2, cell: (cell.left + cell.right) / 2,",
      " tip: (tip.left + tip.right) / 2, open: n.getAttribute('%s') }); })()"
    ),
    jsonlite::toJSON(selector, auto_unbox = TRUE),
    tip_names()$css,
    tip_attribute("open")
  )))
}

describe("tip_table() anchors a cell's and a header cell's tip on its text, in a browser", {
  wide <- data.frame(team = c("Owls", "Foxes"), wins = c(7, 5))
  right <- function(widget) tip_table(widget, cells = c(wins = "{{ wins }} wins"), headers = c(wins = "Games won"))
  dt_page <- local_page(
    save_page(right(DT::datatable(
      wide,
      rownames = FALSE,
      options = list(columnDefs = list(list(className = "dt-right", targets = 1L)))
    ))),
    environment()
  )
  rt_page <- local_page(
    save_page(right(reactable::reactable(
      wide,
      columns = list(wins = reactable::colDef(align = "right", headerStyle = list(textAlign = "right")))
    ))),
    environment()
  )
  closer_to_text <- function(got) abs(got$tip - got$text) < abs(got$tip - got$cell)

  it("points at the number in a right-aligned DT cell and at the text of its header", {
    expect_true(dt_page$wait_until("document.querySelectorAll('tbody td[data-hovercraft-tip-anchor]').length === 2"))
    dt_page$hover("tbody tr:first-child td:nth-child(2)")
    expect_true(closer_to_text(text_and_tip(dt_page, "tbody tr:first-child td:nth-child(2)")))
    dt_page$hover("thead th:nth-child(2)")
    expect_true(closer_to_text(text_and_tip(dt_page, "thead th:nth-child(2)")))
  })

  it("does the same on a reactable table", {
    expect_true(rt_page$wait_until("document.querySelectorAll('.rt-td[data-hovercraft-tip-anchor]').length >= 2"))
    rt_page$hover(".rt-tbody [role=cell]:nth-child(2)")
    expect_true(closer_to_text(text_and_tip(rt_page, ".rt-tbody [role=cell]:nth-child(2)")))
    rt_page$hover("[role=columnheader]:nth-child(2)")
    expect_true(closer_to_text(text_and_tip(rt_page, "[role=columnheader]:nth-child(2)")))
  })
})

describe("tip_table(marker =) highlights a cell while its tip is open, in a browser", {
  page <- local_page(
    save_page(tip_table(
      DT::datatable(scores, rownames = FALSE),
      cells = c(wins = "{{ wins }} wins"),
      marker = "corner"
    )),
    environment()
  )
  fill <- function(selector) {
    page$evaluate(sprintf("getComputedStyle(document.querySelector('%s')).backgroundColor", selector))
  }

  it("tints the cell while the tip shows and takes the tint away when it closes", {
    cell <- "tbody tr:first-child td:nth-child(2)"
    expect_true(page$wait_until("document.querySelectorAll('tbody td[data-hovercraft-marker]').length === 5"))
    expect_false(grepl("127", fill(cell), fixed = TRUE))
    expect_identical(page$hover(cell), "7 wins")
    expect_identical(
      page$evaluate(sprintf("document.querySelector('%s').getAttribute('%s')", cell, tip_attribute("open"))),
      "hint"
    )
    expect_match(fill(cell), "rgba(127, 127, 127", fixed = TRUE)
    page$hover("tbody tr:first-child td:nth-child(1)", opens = FALSE)
    expect_true(page$wait_until(sprintf(
      "!getComputedStyle(document.querySelector('%s')).backgroundColor.includes('127')",
      cell
    )))
    expect_false(grepl("127", fill(cell), fixed = TRUE))
  })
})

describe("a cell's placement in a browser", {
  file <- save_page(
    tip_table(
      DT::datatable(scores, rownames = FALSE),
      rows = tip_spec(rows, placement = "top"),
      cells = list(wins = "{{ wins }} wins", note = tip_spec("{{ note }}", placement = "left"))
    )
  )
  page <- local_page(file, environment())

  it("follows the table's, not its row's, unless the cell has its own", {
    expect_true(page$wait_until("document.querySelectorAll('tbody tr[data-hovercraft-tip-template]').length === 5"))
    placement <- function(selector) {
      page$evaluate(sprintf("document.querySelector('%s').getAttribute('data-hovercraft-tip-placement')", selector))
    }
    expect_identical(placement("tbody tr:first-child"), "top")
    expect_identical(placement("tbody tr:first-child td:nth-child(2)"), "auto")
    expect_identical(placement("tbody tr:first-child td:nth-child(3)"), "left")
  })
})

# Keyboard reach: the table has one tab stop, and the arrow keys move between its tagged rows, cells and header cells;
# the engine shows a tip for whatever takes focus from the keyboard.
owned_stops <- "document.querySelectorAll('tbody [data-hovercraft-tip-kind][tabindex=\"0\"], .rt-tbody [data-hovercraft-tip-kind][tabindex=\"0\"]').length"
described <- "[...document.querySelectorAll('[data-hovercraft-tip-template]')].every((n) => (n.getAttribute('aria-describedby') ?? '').split(' ').filter(Boolean).some((id) => document.getElementById(id)))"
focus <- function(page, selector) {
  # Let go of the tip a tag held before: it would still be on show when the next one is asked for, and be read as it.
  page$evaluate("document.activeElement?.blur()")
  expect_true(page$wait_closed())
  page$evaluate(sprintf("document.querySelector('%s').focus()", selector))
  expect_true(page$wait_open())
  page$settle(0.1)
  page$shown()
}

describe("tip_table() with the keyboard, on a DT table", {
  file <- save_page(
    tip_table(
      DT::datatable(scores, rownames = FALSE, options = list(pageLength = 2)),
      rows = rows,
      cells = cells,
      headers = c(wins = "Games won")
    )
  )
  page <- local_page(file, environment())

  it("has one tab stop and shows a tip for the row, then each cell, that takes focus", {
    expect_true(page$wait_until("document.querySelectorAll('tbody tr[data-hovercraft-tip-template]').length === 2"))
    expect_identical(page$evaluate(owned_stops), 1L)
    expect_identical(focus(page, "tbody tr:first-child"), "Owls: 7 wins")
    page$press("ArrowRight", wait = 0)
    expect_true(page$wait_shown("Note: Top"))
    page$press("ArrowLeft", wait = 0)
    expect_true(page$wait_shown("Owls: 7 wins"))
    page$press("ArrowDown", wait = 0)
    expect_true(page$wait_shown("Foxes: 5 wins"))
    page$press("ArrowUp", wait = 0)
    expect_true(page$wait_shown("Owls: 7 wins"))
    page$press("End", wait = 0)
    expect_true(page$wait_shown("Note: Top"))
    page$press("Home", wait = 0)
    expect_true(page$wait_shown("Owls: 7 wins"))
    page$press("End", ctrl = TRUE, wait = 0)
    expect_true(page$wait_shown("Foxes: 5 wins"))
    page$press("Home", ctrl = TRUE, wait = 0)
    expect_true(page$wait_shown("Games won"))
  })

  it("leaves the arrow keys alone on an element that is not one of its stops", {
    page$evaluate("document.querySelector('.dataTables_paginate a, .dt-paging button')?.focus()")
    expect_identical(page$evaluate("document.activeElement.matches('[data-hovercraft-tip-kind]')"), FALSE)
    expect_length(page$press("ArrowDown"), 0L) # the key must open nothing: nothing happening has no signal
  })

  it("reaches the header cell above the first row, and keeps one stop after a sort and a page", {
    focus(page, "tbody tr:first-child")
    page$press("ArrowUp", wait = 0)
    expect_true(page$wait_shown("Games won"))
    page$evaluate("$('table').DataTable().order([1, 'desc']).draw()")
    page$evaluate("$('table').DataTable().page(1).draw(false)")
    page$settle()
    expect_true(page$wait_until(sprintf("(%s) === 1", owned_stops)))
    expect_identical(page$evaluate(owned_stops), 1L)
    expect_identical(focus(page, "tbody tr:first-child"), "Foxes: 5 wins")
    expect_identical(page$evaluate(described), TRUE)
    expect_identical(page$problems(), character())
  })
})

describe("tip_table() with the keyboard, on a reactable table", {
  file <- save_page(
    tip_table(
      reactable::reactable(scores, defaultPageSize = 2),
      rows = rows,
      cells = cells,
      headers = c(wins = "Games won")
    )
  )
  page <- local_page(file, environment())

  it("has one tab stop and shows a tip for the row, then each cell, that takes focus", {
    expect_true(page$wait_until("document.querySelectorAll('[role=row][data-hovercraft-tip-template]').length >= 2"))
    expect_identical(page$evaluate(owned_stops), 1L)
    expect_identical(focus(page, ".rt-tbody [role=row]"), "Owls: 7 wins")
    page$press("ArrowRight", wait = 0)
    expect_true(page$wait_shown("Note: Top"))
    page$press("ArrowDown", wait = 0)
    expect_true(page$wait_shown("Foxes: 5 wins"))
    page$press("ArrowUp", wait = 0)
    expect_true(page$wait_shown("Owls: 7 wins"))
    page$press("End", wait = 0)
    expect_true(page$wait_shown("Note: Top"))
    page$press("Home", wait = 0)
    expect_true(page$wait_shown("Owls: 7 wins"))
  })

  it("describes every tagged element to a screen reader, also after a sort and a page", {
    expect_identical(page$evaluate(described), TRUE)
    page$evaluate("document.querySelectorAll('.rt-th')[1].click()")
    expect_true(page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Voles')"))
    page$evaluate("[...document.querySelectorAll('button')].find((b) => /next/i.test(b.textContent)).click()")
    expect_true(page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Foxes')"))
    page$settle()
    expect_identical(page$evaluate(described), TRUE)
    expect_identical(page$evaluate(owned_stops), 1L)
    expect_identical(focus(page, ".rt-tbody [role=row]"), "Foxes: 5 wins")
    expect_identical(page$problems(), character())
  })
})

describe("tip_table() with reactable's row selection, in a browser", {
  file <- save_page(
    tip_table(
      reactable::reactable(scores, selection = "multiple", onClick = "select", defaultPageSize = 3),
      rows = rows
    )
  )
  page <- local_page(file, environment())
  selected <- "[...document.querySelectorAll('.rt-tr-selected')].map((n) => n.querySelector('.rt-td:nth-child(2)').textContent).join()"

  it("keeps the tip and selects the row on the same click", {
    expect_true(page$wait_until("document.querySelectorAll('[role=row][data-hovercraft-tip-template]').length >= 3"))
    page$click(".rt-tbody [role=row] .rt-td:nth-child(2)", until = sprintf("(%s) === 'Owls'", selected))
    page$hold(0.7) # the tip must stay open through the click, past the engine's delay: nothing happening has no signal
    expect_identical(page$shown(), "Owls: 7 wins")
    expect_identical(page$evaluate(selected), "Owls")
    expect_identical(page$evaluate("document.querySelector('.rt-tbody [role=row] input[type=checkbox]').checked"), TRUE)
  })

  it("keeps each row's words and selection after a click on a sorted table", {
    page$evaluate("document.querySelectorAll('.rt-th')[2].click()")
    expect_true(page$wait_until(
      "document.querySelector('.rt-tbody .rt-td:nth-child(2)').textContent.startsWith('Voles')"
    ))
    page$click(".rt-tbody [role=row] .rt-td:nth-child(2)", until = sprintf("(%s) === 'Voles'", selected))
    page$hold(0.7) # the tip must stay open through the click, past the engine's delay: nothing happening has no signal
    expect_identical(page$shown(), "Voles: 1 wins")
    # Owls, selected before the sort, is on another page now, so only the Voles row shows as selected.
    expect_identical(page$evaluate(selected), "Voles")
    expect_identical(page$problems(), character())
  })
})

# DT's extensions and options that move, copy or hide the table's own elements.
wide <- data.frame(
  team = sprintf("Team %02d", 1:30),
  wins = (1:30 * 7) %% 11,
  losses = (1:30 * 5) %% 13,
  note = ifelse(1:30 %% 3 == 0, NA, paste("Note", 1:30)),
  city = paste("City", 1:30),
  region = paste("Region", 1:30),
  coach = paste("Coach", 1:30)
)
wide_tips <- function(widget) {
  tip_table(widget, rows = rows, cells = c(note = "Note: {{ note }}"), headers = c(wins = "Games won", note = "About"))
}
dt_ready <- function(page, extra = "true") {
  expect_true(page$wait_until(sprintf(
    "document.querySelectorAll('tbody tr[data-hovercraft-tip-template]').length > 5 && %s",
    extra
  )))
}

describe("tip_table() with DT's scrollX and scrollY, in a browser", {
  file <- save_page(
    wide_tips(DT::datatable(wide, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "200px", paging = FALSE)))
  )
  page <- local_page(file, environment())

  it("tips the header cell people see, the rows and the cells, also after a sort", {
    dt_ready(page)
    expect_identical(page$hover(".dataTables_scrollHead th:nth-child(2)"), "Games won")
    expect_identical(page$hover(".dataTables_scrollBody tbody tr:first-child td:first-child"), "Team 01: 7 wins")
    expect_identical(page$hover(".dataTables_scrollBody tbody tr:first-child td:nth-child(4)"), "Note: Note 1")
    page$evaluate("$('.dataTables_scrollBody table').DataTable().order([1, 'desc']).draw(); 0")
    expect_identical(page$hover(".dataTables_scrollBody tbody tr:first-child td:first-child"), "Team 03: 10 wins")
    expect_identical(page$problems(), character())
  })
})

describe("tip_table() with DT's FixedHeader, in a browser", {
  file <- save_page(
    wide_tips(DT::datatable(
      wide,
      rownames = FALSE,
      extensions = "FixedHeader",
      options = list(fixedHeader = TRUE, paging = FALSE)
    ))
  )
  page <- local_page(file, environment())

  it("tips the header cell of the floating copy", {
    dt_ready(page)
    page$evaluate("document.body.style.minHeight = '3000px'; window.scrollTo(0, 400); 0")
    expect_true(page$wait_until("document.querySelector('.fixedHeader-floating') !== null"))
    expect_identical(page$hover(".fixedHeader-floating th:nth-child(2)"), "Games won")
    expect_identical(
      page$hover("table.dataTable:not(.fixedHeader-floating) tbody tr:nth-child(12) td:first-child"),
      "Team 12: 7 wins"
    )
    expect_identical(page$problems(), character())
  })
})

describe("tip_table() with DT's ColReorder, in a browser", {
  file <- save_page(
    wide_tips(DT::datatable(wide, rownames = FALSE, extensions = "ColReorder", options = list(colReorder = TRUE)))
  )
  page <- local_page(file, environment())

  it("keeps a column's header and cell tips with the column after it moves", {
    dt_ready(page)
    page$evaluate("$('table').DataTable().colReorder.move(3, 0); 0")
    page$settle()
    expect_identical(page$hover("thead th:first-child"), "About")
    expect_identical(page$hover("tbody tr:first-child td:first-child"), "Note: Note 1")
    expect_identical(page$hover("tbody tr:first-child td:nth-child(2)"), "Team 01: 7 wins")
    expect_identical(page$problems(), character())
  })
})

describe("tip_table() with DT's Responsive, in a browser", {
  file <- save_page(
    wide_tips(DT::datatable(wide, rownames = FALSE, extensions = "Responsive", options = list(responsive = TRUE)))
  )
  page <- local_page(file, environment())

  it("keeps the row tips when columns fold into a details row", {
    dt_ready(page)
    page$evaluate(
      "document.querySelector('.dataTables_wrapper').style.width = '360px'; $('table').DataTable().columns.adjust().responsive.recalc(); 0"
    )
    expect_true(page$wait_until(
      "document.querySelectorAll('tbody tr:first-child td').length > 0 && getComputedStyle(document.querySelector('tbody tr:first-child td:last-child')).display === 'none'"
    ))
    expect_identical(page$hover("tbody tr:first-child td:first-child"), "Team 01: 7 wins")
    page$evaluate("document.querySelector('tbody tr:first-child td').click(); 0")
    expect_true(page$wait_until("document.querySelector('tr.child') !== null"))
    expect_identical(page$hover("tbody tr:first-child td:first-child"), "Team 01: 7 wins")
    expect_identical(page$problems(), character())
  })
})

describe("tip_table() descriptions on a reactable table that reuses its rows", {
  file <- save_page(tip_table(reactable::reactable(scores, defaultPageSize = 5), rows = rows, cells = cells))
  page <- local_page(file, environment())
  # Each row's description must be its own tip, and a cell without a tip must have none (React moves rows' content
  # into the elements it already has, which kept the old row's description when the engine's text was left in place).
  described_by <- "JSON.stringify([...document.querySelectorAll('.rt-tbody [role=row]')].map((row) => {
    const said = (n) => (n.getAttribute('aria-describedby') ?? '').split(' ').filter(Boolean).map((id) => document.getElementById(id)?.textContent);
    const cell = row.querySelector('[role=cell]:nth-child(3)');
    return [row.querySelector('[role=cell]').childNodes[0].textContent, said(row), said(cell)];
  }))"

  it("keeps a description with the row and the cell it describes after a sort", {
    expect_true(page$wait_until("document.querySelectorAll('[role=row][aria-describedby]').length >= 5"))
    page$evaluate("document.querySelectorAll('.rt-th')[1].click()")
    expect_true(page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Voles')"))
    page$settle()
    got <- jsonlite::fromJSON(page$evaluate(described_by), simplifyVector = FALSE)
    expect_length(got, 5L)
    for (row in got) {
      expect_match(row[[2]][[1]], paste0("^", row[[1]], ": "))
    }
    # Voles (first) holds NA in note: no tip on that cell, so nothing described; Hares (second) says its own note.
    expect_identical(got[[1]][[3]], list())
    expect_identical(got[[2]][[3]], list("Note: Late"))
  })
})

describe("tip_table() with reactable's groupBy and row details, in a browser", {
  leagues <- data.frame(
    team = c("Owls", "Foxes", "Hares", "Lynx", "Voles", "Stoats"),
    league = c("A", "B", "A", "B", "A", "B"),
    wins = c(7, 5, 2, 9, 1, 4)
  )
  grouped <- save_page(
    tip_table(
      reactable::reactable(
        leagues,
        groupBy = "league",
        columns = list(wins = reactable::colDef(aggregate = "sum"))
      ),
      rows = rows,
      headers = c(wins = "Games won")
    )
  )
  details <- save_page(
    tip_table(
      reactable::reactable(
        leagues,
        onClick = "expand",
        details = function(index) paste("Details of", leagues$team[[index]])
      ),
      rows = rows
    )
  )

  it("tips the rows inside an open group by their own data row, and not the group's own row", {
    page <- local_page(grouped, environment())
    expect_true(page$wait_until(
      "document.querySelector('.rt-expander-button, .rt-td-expandable [role=button]') !== null"
    ))
    page$evaluate("document.querySelector('.rt-expander-button, .rt-td-expandable [role=button]').click(); 0")
    expect_true(page$wait_until("document.querySelectorAll('[role=row][data-hovercraft-tip-template]').length === 3"))
    # reactable draws the group's own row and the rows inside it as sibling groups: the first and the last are the
    # two groups' own rows (League A, League B), the three between are Owls, Hares and Voles.
    inside <- ".rt-tbody .rt-tr-group:nth-child(%d) [role=row] .rt-td:nth-child(2)"
    expect_identical(page$hover(sprintf(inside, 2)), "Owls: 7 wins")
    expect_identical(page$hover(sprintf(inside, 4)), "Voles: 1 wins")
    expect_length(page$hover(".rt-tbody .rt-tr-group:nth-child(1) [role=row] .rt-td:nth-child(2)", opens = FALSE), 0L)
    expect_length(page$hover(".rt-tbody .rt-tr-group:nth-child(5) [role=row] .rt-td:nth-child(2)", opens = FALSE), 0L)
    expect_identical(page$problems(), character())
  })

  it("keeps the row's tip when its details open, and shows none over the details", {
    page <- local_page(details, environment())
    expect_true(page$wait_until("document.querySelectorAll('[role=row][data-hovercraft-tip-template]').length === 6"))
    # The tip must stay open through the click, past the engine's delay: nothing happening has no signal.
    expect_identical(page$click(".rt-tbody .rt-tr-group:nth-child(3) [role=row] .rt-td:nth-child(2)"), "Hares: 2 wins")
    expect_true(page$wait_until("document.querySelector('.rt-tr-details') !== null"))
    expect_length(page$hover(".rt-tr-details", opens = FALSE), 0L)
    expect_identical(page$hover(".rt-tbody .rt-tr-group:nth-child(4) [role=row] .rt-td:nth-child(2)"), "Lynx: 9 wins")
    expect_identical(
      page$evaluate("document.querySelectorAll('.rt-tr-details [data-hovercraft-tip-template]').length"),
      0L
    )
    expect_identical(page$problems(), character())
  })
})

describe("tip_table(id =) and the keyboard with DT's copies of the header", {
  with_id <- function(widget) {
    tip_table(widget, rows = rows, cells = c(note = "Note: {{ note }}"), headers = c(wins = "Games won"), id = "t")
  }
  duplicated_ids <- "JSON.stringify((() => {
    const ids = [...document.querySelectorAll('[data-hovercraft-tip-id]')].map((n) => n.getAttribute('data-hovercraft-tip-id'));
    return [...new Set(ids.filter((x, i) => ids.indexOf(x) !== i))];
  })())"

  it("skips the collapsed header copy of scrollX, and gives only a header's copy the header's id", {
    file <- save_page(
      with_id(DT::datatable(wide, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "200px", paging = FALSE)))
    )
    page <- local_page(file, environment())
    dt_ready(page)
    page$evaluate("$('.dataTables_scrollBody table').DataTable().order([1, 'desc']).draw(); 0")
    expect_true(page$wait_until(
      "document.querySelector('.dataTables_scrollBody tbody tr:first-child')?.hasAttribute('data-hovercraft-tip-id')"
    ))
    page$settle()
    expect_identical(jsonlite::fromJSON(page$evaluate(duplicated_ids)), "t/header/2")
    focus(page, ".dataTables_scrollBody tbody tr:first-child")
    page$press("ArrowUp", wait = 0)
    expect_true(page$wait_shown("Games won"))
    expect_true(page$evaluate("!!document.activeElement.closest('.dataTables_scrollHead')"))
  })

  it("gives the floating header of fixedHeader the id of the header it copies", {
    file <- save_page(
      with_id(
        DT::datatable(
          wide,
          rownames = FALSE,
          extensions = "FixedHeader",
          options = list(fixedHeader = TRUE, paging = FALSE)
        )
      )
    )
    page <- local_page(file, environment())
    dt_ready(page)
    page$evaluate("document.body.style.minHeight = '3000px'; window.scrollTo(0, 400); 0")
    expect_true(page$wait_until("document.querySelector('.fixedHeader-floating') !== null"))
    expect_identical(jsonlite::fromJSON(page$evaluate(duplicated_ids)), "t/header/2")
  })
})

# A table's corner is a background triangle on the cell, so it takes no room and leaves DT's sort arrows (the header
# cell's ::before and ::after) alone. It must show on cells and header cells of both widgets, and follow the text colour.
corner <- function(page, selector) {
  jsonlite::fromJSON(page$evaluate(sprintf(
    paste0(
      "(() => { const n = document.querySelector('%s'); const s = getComputedStyle(n);",
      " return JSON.stringify({ image: s.backgroundImage, color: s.color, size: s.backgroundSize }); })()"
    ),
    selector
  )))
}
marked_settings <- list(cells = c(wins = "{{ wins }} wins"), headers = c(wins = "Games won"), marker = "corner")

describe("tip_table(marker =) in a browser", {
  dt_file <- save_page(
    do.call(
      tip_table,
      c(list(DT::datatable(scores, rownames = FALSE, options = list(pageLength = 2))), marked_settings)
    )
  )
  dt_page <- local_page(dt_file, environment())
  rt_file <- save_page(do.call(tip_table, c(list(reactable::reactable(scores, defaultPageSize = 2)), marked_settings)))
  rt_page <- local_page(rt_file, environment())

  it("draws a triangle on a DT cell and header cell, and leaves the sort arrows", {
    expect_true(dt_page$wait_until("document.querySelectorAll('tbody td[data-hovercraft-marker]').length === 2"))
    for (selector in c("tbody tr:first-child td:nth-child(2)", "thead th:nth-child(2)")) {
      got <- corner(dt_page, selector)
      expect_match(got$image, "linear-gradient", fixed = TRUE)
      expect_match(got$image, got$color, fixed = TRUE)
    }
    expect_identical(corner(dt_page, "tbody tr:first-child td:nth-child(1)")$image, "none")
    sort_arrow <- "getComputedStyle(document.querySelector('thead th:nth-child(2)'), '::after').content"
    expect_false(dt_page$evaluate(sprintf("%s === 'none' || %s === 'normal'", sort_arrow, sort_arrow)))
    expect_identical(dt_page$problems(), character())
  })

  it("keeps the triangle on the cells after a sort", {
    dt_page$evaluate("$('table').DataTable().order([1, 'desc']).draw()")
    expect_true(dt_page$wait_until(
      "document.querySelector('tbody tr:first-child td:nth-child(2)')?.hasAttribute('data-hovercraft-marker')"
    ))
    expect_match(corner(dt_page, "tbody tr:first-child td:nth-child(2)")$image, "linear-gradient", fixed = TRUE)
  })

  it("draws a triangle on a reactable cell and header cell, and keeps it after a change of page", {
    expect_true(rt_page$wait_until("document.querySelectorAll('.rt-td[data-hovercraft-marker]').length >= 2"))
    expect_match(corner(rt_page, ".rt-tbody [role=cell]:nth-child(2)")$image, "linear-gradient", fixed = TRUE)
    expect_match(corner(rt_page, "[role=columnheader][data-hovercraft-marker]")$image, "linear-gradient", fixed = TRUE)
    rt_page$evaluate("[...document.querySelectorAll('button')].find((b) => /next/i.test(b.textContent)).click()")
    expect_true(rt_page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Hares')"))
    expect_match(corner(rt_page, ".rt-tbody [role=cell]:nth-child(2)")$image, "linear-gradient", fixed = TRUE)
    expect_identical(rt_page$problems(), character())
  })
})

# A table's style reaches the tip: the table's class on a cell's tip, and a column's own class over it.
shown_look <- function(page) {
  jsonlite::fromJSON(page$evaluate(sprintf(
    paste0(
      "(() => { const t = [...document.querySelectorAll('.%s')].find((n) => !n.hidden);",
      " return JSON.stringify({ classes: [...t.classList], preset: t.dataset.preset ?? null }); })()"
    ),
    tip_names()$css
  )))
}

describe("tip_table(style =) in a browser", {
  file <- save_page(
    tip_table(
      DT::datatable(scores, rownames = FALSE, options = list(pageLength = 2)),
      cells = list(wins = tip_spec("{{ wins }} wins", style = tip_style(class = "own-column")), note = "{{ note }}"),
      style = tip_style(class = "league-tip")
    )
  )
  page <- local_page(file, environment())

  it("puts the table's class on a tip, and a column's own class over it", {
    expect_true(page$wait_until("document.querySelectorAll('tbody td[data-hovercraft-tip-template]').length >= 2"))
    expect_identical(page$hover("tbody tr:first-child td:last-child"), "Top")
    expect_true("league-tip" %in% shown_look(page)$classes)
    expect_identical(page$hover("tbody tr:first-child td:nth-child(2)"), "7 wins")
    expect_true("own-column" %in% shown_look(page)$classes)
    expect_identical(page$problems(), character())
  })
})

# tip_on(placement =): the tip opens on the side it was asked for. A button in the middle of a wide page has room on
# every side, so the engine keeps the side it is given.
side_of <- function(page) {
  page$evaluate(sprintf(
    "[...document.querySelectorAll('.%s')].find((n) => !n.hidden).dataset.side",
    tip_names()$css
  ))
}

describe("tip_on(placement =) in a browser", {
  button <- function(id, ...) {
    tip_on(htmltools::tags$button(id = id, style = "margin: 160px 300px", id), paste("Tip", id), ...)
  }
  dir <- withr::local_tempdir()
  path <- file.path(dir, "page.html")
  htmltools::save_html(
    htmltools::tagList(button("right", placement = "right"), button("below", placement = "bottom")),
    path
  )
  page <- local_page(path, environment())

  it("opens on the side that was asked for", {
    expect_true(page$wait_until(sprintf("typeof %s === 'object'", tip_names()$global)))
    expect_identical(page$hover("#right"), "Tip right")
    expect_identical(side_of(page), "right")
    expect_identical(page$hover("#below"), "Tip below")
    expect_identical(side_of(page), "below")
    expect_identical(page$problems(), character())
  })
})

describe("tip_table() on a DT table with a header of more than one row, in a browser", {
  two <- DT::datatable(
    scores,
    rownames = FALSE,
    container = htmltools::tags$table(
      htmltools::tags$thead(
        htmltools::tags$tr(htmltools::tags$th(colspan = 2, "Team"), htmltools::tags$th(colspan = 1, "Notes")),
        htmltools::tags$tr(lapply(names(scores), htmltools::tags$th))
      )
    )
  )
  page <- local_page(
    save_page(tip_table(two, rows = rows, cells = cells, headers = c(wins = "Games won"))),
    environment()
  )

  it("shows the header tip on the cell of the column, and the row's and cell's tips", {
    expect_true(page$wait_until("document.querySelectorAll('tbody tr[data-hovercraft-tip-template]').length === 5"))
    expect_identical(page$hover("thead tr:nth-child(2) th:nth-child(2)"), "Games won")
    expect_identical(page$hover("tbody tr:first-child td:first-child"), "Owls: 7 wins")
    expect_identical(page$hover("tbody tr:first-child td:last-child"), "Note: Top")
    expect_identical(page$problems(), character())
  })
})

# A filter box under a header (#86): the header cell has the tip, the filter cell under it has none. reactable puts a
# column's `headerClassName` on both, and the layer used to mark both, so the header's tip opened over the filter box.
describe("tip_table() with a filter row, in a browser", {
  tipped <- "[data-hovercraft-tip-template], [data-hovercraft-tip-kind], [aria-describedby]"
  # How many of the elements the selector names hold a tip, a tip kind or a description.
  count <- function(page, selector) {
    page$evaluate(sprintf(
      "[...document.querySelectorAll(%s)].filter((n) => n.matches(%s)).length",
      jsonlite::toJSON(selector, auto_unbox = TRUE),
      jsonlite::toJSON(tipped, auto_unbox = TRUE)
    ))
  }

  it("gives a reactable header cell the tip and its filter cell none", {
    file <- save_page(
      tip_table(
        reactable::reactable(scores, filterable = TRUE, defaultPageSize = 5),
        rows = rows,
        cells = cells,
        headers = c(wins = "Wins so far")
      )
    )
    page <- local_page(file, environment())
    expect_true(page$wait_until("document.querySelectorAll('.rt-tbody [data-hovercraft-tip-template]').length >= 5"))
    expect_identical(page$evaluate("document.querySelectorAll('.rt-td-filter').length"), 3L)
    expect_identical(page$evaluate("document.querySelectorAll('.rt-th[data-hovercraft-tip-template]').length"), 1L)
    expect_identical(count(page, ".rt-tr-filters, .rt-td-filter"), 0L)
    expect_identical(page$hover(".rt-th:nth-child(2)"), "Wins so far")
    # The filter box under the tipped header is free: no tip opens over it.
    expect_length(page$hover(".rt-td-filter:nth-child(2)", opens = FALSE), 0L)
    # A sort re-marks the table; the filter cell stays free, and the header and the rows keep their tips.
    page$evaluate("document.querySelectorAll('.rt-th')[1].click()")
    expect_true(page$wait_until(
      "['ascending', 'descending'].includes(document.querySelectorAll('.rt-th')[1].getAttribute('aria-sort'))"
    ))
    page$settle()
    expect_identical(count(page, ".rt-tr-filters, .rt-td-filter"), 0L)
    expect_identical(page$hover(".rt-th:nth-child(2)"), "Wins so far")
    expect_match(page$hover(".rt-tbody .rt-tr:first-child .rt-td:first-child"), "wins$")
    expect_identical(page$problems(), character())
  })

  it("leaves DT's filter row as it was: no tip on it", {
    file <- save_page(
      tip_table(
        DT::datatable(scores, rownames = FALSE, filter = "top", options = list(pageLength = 5)),
        rows = rows,
        headers = c(wins = "Wins so far")
      )
    )
    page <- local_page(file, environment())
    expect_true(page$wait_until("document.querySelectorAll('tbody tr[data-hovercraft-tip-template]').length === 5"))
    expect_identical(page$evaluate("document.querySelectorAll('thead th[data-hovercraft-tip-template]').length"), 1L)
    expect_identical(count(page, "thead tr:nth-child(2) td, thead tr:nth-child(2) th"), 0L)
    expect_identical(page$hover("thead tr:first-child th:nth-child(2)"), "Wins so far")
  })
})

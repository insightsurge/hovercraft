skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("DT")
skip_if_not_installed("reactable")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

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
    page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Voles')")
    expect_identical(page$hover(".rt-tbody [role=cell]:nth-child(1)"), "Voles: 1 wins")
    page$evaluate("[...document.querySelectorAll('button')].find((b) => /next/i.test(b.textContent)).click()")
    page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Foxes')")
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
    page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Voles')")
    expect_identical(page$hover(".rt-tbody [role=cell]:nth-child(1)"), "Voles: 1 wins")
    expect_match(id_of(page, row), "5$")
    page$evaluate("[...document.querySelectorAll('button')].find((b) => /next/i.test(b.textContent)).click()")
    page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Foxes')")
    expect_match(id_of(page, row), "2$")
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
    page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Voles')")
    page$wait_until("document.querySelectorAll('.rt-tbody [role=cell][data-hovercraft-tip-id]').length > 0")
    expect_false(has(cell(1)))
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s')?.getAttribute('data-hovercraft-tip-id')?.endsWith('cell/3/3')",
      cell(2)
    )))
    page$evaluate("[...document.querySelectorAll('button')].find((b) => /next/i.test(b.textContent)).click()")
    page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Foxes')")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('%s')?.getAttribute('data-hovercraft-tip-id')?.endsWith('cell/1/3')",
      cell(2)
    )))
    expect_false(has(cell(1)))
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
  page$evaluate(sprintf("document.querySelector('%s').focus()", selector))
  Sys.sleep(0.8)
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
    expect_identical(page$press("ArrowRight"), "Note: Top")
    expect_identical(page$press("ArrowLeft"), "Owls: 7 wins")
    expect_identical(page$press("ArrowDown"), "Foxes: 5 wins")
    expect_identical(page$press("ArrowUp"), "Owls: 7 wins")
    expect_identical(page$press("End"), "Note: Top")
    expect_identical(page$press("Home"), "Owls: 7 wins")
  })

  it("leaves the arrow keys alone on an element that is not one of its stops", {
    page$evaluate("document.querySelector('.dataTables_paginate a, .dt-paging button')?.focus()")
    expect_identical(page$evaluate("document.activeElement.matches('[data-hovercraft-tip-kind]')"), FALSE)
    expect_length(page$press("ArrowDown"), 0L)
  })

  it("reaches the header cell above the first row, and keeps one stop after a sort and a page", {
    focus(page, "tbody tr:first-child")
    expect_identical(page$press("ArrowUp"), "Games won")
    page$evaluate("$('table').DataTable().order([1, 'desc']).draw()")
    page$evaluate("$('table').DataTable().page(1).draw(false)")
    Sys.sleep(0.5)
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
    expect_identical(page$press("ArrowRight"), "Note: Top")
    expect_identical(page$press("ArrowDown"), "Foxes: 5 wins")
    expect_identical(page$press("ArrowUp"), "Owls: 7 wins")
    expect_identical(page$press("End"), "Note: Top")
    expect_identical(page$press("Home"), "Owls: 7 wins")
  })

  it("describes every tagged element to a screen reader, also after a sort and a page", {
    expect_identical(page$evaluate(described), TRUE)
    page$evaluate("document.querySelectorAll('.rt-th')[1].click()")
    page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Voles')")
    page$evaluate("[...document.querySelectorAll('button')].find((b) => /next/i.test(b.textContent)).click()")
    page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Foxes')")
    Sys.sleep(0.5)
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
    expect_identical(page$click(".rt-tbody [role=row] .rt-td:nth-child(2)"), "Owls: 7 wins")
    expect_identical(page$evaluate(selected), "Owls")
    expect_identical(page$evaluate("document.querySelector('.rt-tbody [role=row] input[type=checkbox]').checked"), TRUE)
  })

  it("keeps each row's words and selection after a click on a sorted table", {
    page$evaluate("document.querySelectorAll('.rt-th')[2].click()")
    page$wait_until("document.querySelector('.rt-tbody .rt-td:nth-child(2)').textContent.startsWith('Voles')")
    expect_identical(page$click(".rt-tbody [role=row] .rt-td:nth-child(2)"), "Voles: 1 wins")
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
    Sys.sleep(0.5)
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
    page$wait_until("document.querySelector('.rt-tbody .rt-td').textContent.startsWith('Voles')")
    Sys.sleep(0.5)
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
    expect_length(page$hover(".rt-tbody .rt-tr-group:nth-child(1) [role=row] .rt-td:nth-child(2)"), 0L)
    expect_length(page$hover(".rt-tbody .rt-tr-group:nth-child(5) [role=row] .rt-td:nth-child(2)"), 0L)
    expect_identical(page$problems(), character())
  })

  it("keeps the row's tip when its details open, and shows none over the details", {
    page <- local_page(details, environment())
    expect_true(page$wait_until("document.querySelectorAll('[role=row][data-hovercraft-tip-template]').length === 6"))
    expect_identical(page$click(".rt-tbody .rt-tr-group:nth-child(3) [role=row] .rt-td:nth-child(2)"), "Hares: 2 wins")
    expect_true(page$wait_until("document.querySelector('.rt-tr-details') !== null"))
    expect_length(page$hover(".rt-tr-details"), 0L)
    expect_identical(page$hover(".rt-tbody .rt-tr-group:nth-child(4) [role=row] .rt-td:nth-child(2)"), "Lynx: 9 wins")
    expect_identical(
      page$evaluate("document.querySelectorAll('.rt-tr-details [data-hovercraft-tip-template]').length"),
      0L
    )
    expect_identical(page$problems(), character())
  })
})

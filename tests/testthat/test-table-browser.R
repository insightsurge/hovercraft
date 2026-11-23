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

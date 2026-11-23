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

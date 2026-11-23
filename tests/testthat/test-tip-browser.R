skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("bslib")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# One bslib page with a plain text tip, a template tip on a table row, a button inside bslib::tooltip() that already
# has a tip of its own, and the shared placeholder cases as buttons, each with a template made of one text block.
page_file <- function(dir) {
  cases <- jsonlite::read_json(tip_path("placeholders.json"), simplifyVector = FALSE)$cases
  base <- tip_names()$attribute
  case_buttons <- lapply(seq_along(cases), function(i) {
    record <- lapply(cases[[i]]$record, function(value) if (is.null(value)) NA else value)
    attrs <- stats::setNames(
      list(
        paste0("case", i),
        as.character(jsonlite::toJSON(
          structure(record, names = names(record) %||% character()),
          auto_unbox = TRUE,
          na = "null",
          digits = NA
        ))
      ),
      c(paste0(base, "-template"), paste0(base, "-record"))
    )
    do.call(htmltools::tagAppendAttributes, c(list(htmltools::tags$button(id = paste0("case", i), "case")), attrs))
  })
  define_cases <- htmltools::tags$script(htmltools::HTML(paste(
    vapply(
      seq_along(cases),
      function(i) {
        sprintf(
          "%s.define(%s, [{type: 'text', text: %s}]);",
          tip_names()$global,
          jsonlite::toJSON(paste0("case", i), auto_unbox = TRUE),
          jsonlite::toJSON(cases[[i]]$text, auto_unbox = TRUE)
        )
      },
      ""
    ),
    collapse = "\n"
  )))
  rows <- data.frame(label = c("ADAE", "ADSL"), differences = c(3, 0))
  page <- bslib::page_fluid(
    tip_define(diff = tip_template(tip_title("{{ label }}"), tip_text("{{ differences | number:0 }} differences"))),
    tip_on(htmltools::tags$button(id = "plain", "Run"), "Runs the **check**"),
    htmltools::tags$table(
      lapply(seq_len(nrow(rows)), function(i) {
        tip_on(
          htmltools::tags$tr(htmltools::tags$td(rows$label[[i]], class = "cell"), tabindex = "0"),
          template = "diff",
          record = rows[i, ]
        )
      })
    ),
    bslib::tooltip(
      tip_on(htmltools::tags$button(id = "inside", "Inside bslib"), "From hovercraft"),
      "From bslib"
    ),
    htmltools::tags$div(id = "dynamic-host"),
    htmltools::tagList(define_cases, case_buttons)
  )
  path <- file.path(dir, "page.html")
  htmltools::save_html(page, path)
  path
}

describe("a bslib page", {
  dir <- withr::local_tempdir()
  path <- page_file(dir)
  env <- environment()
  page <- local_page(path, env)
  it("loads the engine without a console error", {
    expect_true(page$wait_until(sprintf("typeof %s === 'object'", tip_names()$global)))
    expect_identical(page$problems(), character())
  })

  it("shows a plain text tip after the pointer rests, as markdown", {
    expect_identical(page$hover("#plain"), "Runs the check")
    expect_identical(
      page$evaluate(sprintf("document.querySelector('.%s strong').textContent", tip_names()$css)),
      "check"
    )
  })

  it("shows a template tip filled from the row's record", {
    shown <- page$hover("tr:nth-child(1) .cell")
    expect_match(shown, "ADAE")
    expect_match(shown, "3 differences")
    expect_match(page$hover("tr:nth-child(2) .cell"), "0 differences")
  })

  it("hides the tip when the pointer leaves", {
    page$hover("#plain")
    expect_length(page$hover("tr:nth-child(1) .cell"), 1L)
    expect_length(page$shown(), 1L)
  })

  it("leaves a tag inside bslib::tooltip() to bslib", {
    shown <- page$hover("#inside")
    expect_false(any(grepl("From hovercraft", shown, fixed = TRUE)))
  })

  it("gives a tip to a tag added to the page later, as renderUI does", {
    html <- as.character(htmltools::as.tags(tip_on(htmltools::tags$button(id = "later", "Later"), "Added later")))
    page$evaluate(sprintf(
      "document.getElementById('dynamic-host').insertAdjacentHTML('beforeend', %s)",
      jsonlite::toJSON(html, auto_unbox = TRUE)
    ))
    expect_identical(page$hover("#later"), "Added later")
  })

  it("shows every shared placeholder case as the cases say", {
    cases <- jsonlite::read_json(tip_path("placeholders.json"), simplifyVector = FALSE)$cases
    for (i in seq_along(cases)) {
      words <- page$evaluate(sprintf(
        "(() => { const b = document.getElementById('case%d'); const id = b.getAttribute('aria-describedby'); return id ? document.getElementById(id).textContent : null; })()",
        i
      ))
      expect_identical(trimws(words), cases[[i]]$shown, info = cases[[i]]$name)
    }
  })

  it("ends without a console error", {
    expect_identical(page$problems(), character())
  })
})

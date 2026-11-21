skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("bslib")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# One page with a theme: a tip's fill and words in light mode, then after the page's own data-bs-theme says dark.
theme_page <- function(dir, theme) {
  page <- bslib::page_fluid(
    tip_dependency(theme = theme),
    tip_on(htmltools::tags$button(id = "go", "Run"), "Runs the check")
  )
  path <- file.path(dir, "page.html")
  htmltools::save_html(page, path)
  path
}

describe("a themed page", {
  dir <- withr::local_tempdir()
  theme <- tip_theme(
    light = list(bg = "#1b3a6b", ink = "#ffffff", radius = 9),
    dark = list(bg = "#9ec5fe", ink = "#0b1f3a")
  )
  page <- local_page(theme_page(dir, theme), environment())
  look <- function() {
    jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const t = document.querySelector('.%s:not([hidden])'); const s = getComputedStyle(t); return JSON.stringify({ bg: s.backgroundColor, ink: s.color, radius: s.borderTopLeftRadius }); })()",
      tip_names()$css
    )))
  }

  it("draws the tip in the theme's light colours and corner", {
    expect_true(page$wait_until(sprintf("typeof %s === 'object'", tip_names()$global)))
    expect_identical(page$hover("#go"), "Runs the check")
    got <- look()
    expect_identical(got$bg, "rgb(27, 58, 107)")
    expect_identical(got$ink, "rgb(255, 255, 255)")
    expect_identical(got$radius, "9px")
  })

  it("draws it in the dark colours once the page says dark, as bslib's input_dark_mode() does", {
    page$evaluate("document.documentElement.setAttribute('data-bs-theme', 'dark')")
    page$hover("#go")
    got <- look()
    expect_identical(got$bg, "rgb(158, 197, 254)")
    expect_identical(got$ink, "rgb(11, 31, 58)")
    expect_identical(got$radius, "9px")
  })

  it("has no console error", {
    expect_identical(page$problems(), character())
  })
})

describe("a page with no theme", {
  dir <- withr::local_tempdir()
  page <- local_page(theme_page(dir, NULL), environment())

  it("keeps the engine's own colours: the page's text colour as the fill", {
    expect_true(page$wait_until(sprintf("typeof %s === 'object'", tip_names()$global)))
    page$hover("#go")
    bg <- page$evaluate(sprintf(
      "getComputedStyle(document.querySelector('.%s:not([hidden])')).backgroundColor",
      tip_names()$css
    ))
    expect_match(bg, "^rgb\\(")
    expect_false(identical(bg, "rgb(27, 58, 107)"))
  })
})

skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("bslib")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# The look of a style in a real browser: the page's preset and theme together (a page theme once sat on :root, where the
# engine took it for inherited and a page preset overwrote it), a tag's own style against the page's, and the class.
rgb_of <- function(hex) paste0("rgb(", paste(grDevices::col2rgb(hex), collapse = ", "), ")")
preset_light <- function(name) jsonlite::read_json(tip_path("presets", paste0(name, ".json")))$light$bg

style_page <- function(dir, style, ...) {
  page <- bslib::page_fluid(
    tip_dependency(style = style),
    htmltools::tags$button(id = "plain", "Plain") |> tip_on("Plain tip"),
    ...
  )
  path <- file.path(dir, "page.html")
  htmltools::save_html(page, path)
  path
}

tag <- function(id, style) tip_on(htmltools::tags$button(id = id, id), paste("Tip", id), style = style)

# What the tip on show looks like: fill, words, corner, and its classes.
look <- function(page) {
  jsonlite::fromJSON(page$evaluate(sprintf(
    paste0(
      "(() => { const t = document.querySelector('.%s:not([hidden])'); const s = getComputedStyle(t);",
      " return JSON.stringify({ bg: s.backgroundColor, ink: s.color, radius: s.borderTopLeftRadius,",
      " classes: [...t.classList] }); })()"
    ),
    tip_names()$css
  )))
}
wait_for_engine <- function(page) page$wait_until(sprintf("typeof %s === 'object'", tip_names()$global))

describe("a page with a preset and a theme", {
  dir <- withr::local_tempdir()
  navy <- tip_theme(
    light = list(bg = "#1b3a6b", ink = "#ffffff"),
    dark = list(bg = "#9ec5fe", ink = "#0b1f3a")
  )
  maroon <- tip_theme(light = list(bg = "#7a1f1f", ink = "#ffffff"))
  path <- style_page(
    dir,
    tip_style("sober", theme = navy, class = "page-wide"),
    tag("dense", "dense"),
    tag("none", "none"),
    tag("maroon", maroon),
    tag("own", tip_style(class = "own")),
    tag("cleared", tip_style(class = ""))
  )
  page <- local_page(path, environment())

  it("writes the page preset and class on the page as the engine's attributes", {
    expect_true(wait_for_engine(page))
    read <- function(name) page$evaluate(sprintf("document.documentElement.getAttribute('%s')", name))
    expect_identical(read(tip_attribute("preset")), "sober")
    expect_identical(read(tip_attribute("class")), "page-wide")
  })

  it("shows the theme's colours with the preset's corner: the theme beats the preset it sits with", {
    expect_identical(page$hover("#plain"), "Plain tip")
    got <- look(page)
    expect_identical(got$bg, "rgb(27, 58, 107)")
    expect_identical(got$ink, "rgb(255, 255, 255)")
    expect_identical(got$radius, "2px")
    expect_true("page-wide" %in% got$classes)
  })

  it("shows the theme's dark colours in dark mode, still under the preset", {
    page$evaluate("document.documentElement.setAttribute('data-bs-theme', 'dark')")
    page$hover("#plain")
    got <- look(page)
    expect_identical(got$bg, "rgb(158, 197, 254)")
    expect_identical(got$radius, "2px")
    page$evaluate("document.documentElement.setAttribute('data-bs-theme', 'light')")
  })

  it("lets a tag's preset beat the page's theme", {
    page$hover("#dense")
    got <- look(page)
    expect_identical(got$bg, rgb_of(preset_light("dense")))
    expect_false(identical(got$bg, "rgb(27, 58, 107)"))
  })

  it("lets a tag's none drop the preset and keep the page's theme", {
    page$hover("#none")
    got <- look(page)
    expect_identical(got$bg, "rgb(27, 58, 107)")
    expect_false(identical(got$radius, "2px"))
  })

  it("lets a tag's theme beat the page's, keeping the page preset's corner", {
    page$hover("#maroon")
    got <- look(page)
    expect_identical(got$bg, "rgb(122, 31, 31)")
    expect_identical(got$radius, "2px")
  })

  it("lets a tag's class replace the page's, and an empty class clear it", {
    page$hover("#own")
    own <- look(page)
    expect_true("own" %in% own$classes)
    expect_false("page-wide" %in% own$classes)
    page$hover("#cleared")
    expect_false("page-wide" %in% look(page)$classes)
  })

  it("has no console error", {
    expect_identical(page$problems(), character())
  })
})

describe("a page with a preset alone", {
  dir <- withr::local_tempdir()
  page <- local_page(style_page(dir, "sober"), environment())

  it("shows the preset's colours and corner", {
    expect_true(wait_for_engine(page))
    page$hover("#plain")
    got <- look(page)
    expect_identical(got$bg, rgb_of(preset_light("sober")))
    expect_identical(got$radius, "2px")
  })
})

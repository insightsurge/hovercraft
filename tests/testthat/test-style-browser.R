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
    use_hovercraft(style = style),
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
    light = list(bg = "#1b3a6b", text = "#ffffff"),
    dark = list(bg = "#9ec5fe", text = "#0b1f3a")
  )
  maroon <- tip_theme(light = list(bg = "#7a1f1f", text = "#ffffff"))
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

# Size and motion: each setting changes what the page draws. The gap is how far the tip sits from its tag, the widths are
# the boxes' own, and the motion is the animation the engine starts when a tip opens (a fade, or a slide of four px).
long_text <- paste(rep("Revenue is the gross amount billed in the period, before refunds.", 6), collapse = " ")
size_tag <- function(id, ..., text = "A short tip", pin = FALSE) {
  tag <- tip_on(htmltools::tags$button(id = id, id), text, placement = "bottom", id = if (pin) id, ...)
  if (pin) {
    tag <- htmltools::tagAppendAttributes(tag, !!tip_attribute("pin") := "true")
  }
  htmltools::tags$div(style = "height: 260px; padding-top: 40px", tag)
}

# Records the animations the engine starts, as their keyframes.
watch_animations <- function(page) {
  page$evaluate(
    paste0(
      "window.__animations = []; const start = Element.prototype.animate;",
      " Element.prototype.animate = function (keyframes, options) {",
      " window.__animations.push(JSON.stringify(keyframes)); return start.call(this, keyframes, options); }; 0"
    )
  )
}
open_tip <- function(page, id, click = FALSE) {
  page$evaluate(sprintf("document.getElementById('%s').scrollIntoView({block: 'center', behavior: 'instant'}); 0", id))
  # The page moved under the pointer, which may open another tag's tip: let it settle before the one under test.
  Sys.sleep(0.8)
  page$evaluate("window.__animations = []; 0")
  shown <- if (click) page$click(paste0("#", id)) else page$hover(paste0("#", id))
  info <- jsonlite::fromJSON(
    page$evaluate(sprintf(
      paste0(
        "(() => { const a = document.getElementById('%s').getBoundingClientRect();",
        " const t = [...document.querySelectorAll('.%s')].filter((b) => !b.hidden)[0];",
        " const r = t.getBoundingClientRect();",
        " return JSON.stringify({ gap: r.top - a.bottom, width: r.width, animations: window.__animations }); })()"
      ),
      id,
      tip_names()$css
    )),
    simplifyVector = FALSE
  )
  info$shown <- shown
  info$animations <- unlist(info$animations)
  info
}

describe("size and motion on one tag", {
  dir <- withr::local_tempdir()
  page <- local_page(
    style_page(
      dir,
      "sober",
      size_tag("gap0", style = tip_style(gap = 0)),
      size_tag("gap24", style = tip_style(gap = 24)),
      size_tag("narrow", style = tip_style(max_width = 200), text = long_text),
      size_tag("wide", style = tip_style(max_width = 500), text = long_text),
      size_tag("pin-narrow", style = tip_style(pin_width = 260), text = long_text, pin = TRUE),
      size_tag("pin-wide", style = tip_style(pin_width = 600), text = long_text, pin = TRUE),
      size_tag("fade", style = tip_style(motion = "fade")),
      size_tag("slide", style = tip_style(motion = "slide")),
      size_tag("none", style = tip_style(motion = "none")),
      size_tag("unset", style = tip_style(class = "plain"))
    ),
    environment()
  )

  it("moves the tip away from its tag by the gap", {
    expect_true(wait_for_engine(page))
    watch_animations(page)
    near <- open_tip(page, "gap0")
    far <- open_tip(page, "gap24")
    expect_equal(far$gap - near$gap, 24, tolerance = 0.5)
    expect_gte(near$gap, 0)
  })

  it("caps the width of a hint at max_width", {
    narrow <- open_tip(page, "narrow")
    wide <- open_tip(page, "wide")
    expect_equal(narrow$width, 200, tolerance = 0.5)
    expect_equal(wide$width, 500, tolerance = 0.5)
  })

  it("sets the width of a pinned tip with pin_width", {
    narrow <- open_tip(page, "pin-narrow", click = TRUE)
    expect_equal(narrow$width, 260, tolerance = 0.5)
    wide <- open_tip(page, "pin-wide", click = TRUE)
    expect_equal(wide$width, 600, tolerance = 0.5)
  })

  it("fades, slides or stands still as the motion says", {
    fade <- open_tip(page, "fade")
    expect_length(fade$animations, 1L)
    expect_match(fade$animations, "\"opacity\":0", fixed = TRUE)
    expect_match(fade$animations, "\"transform\":\"none\"", fixed = TRUE)
    expect_no_match(fade$animations, "translate", fixed = TRUE)
    slide <- open_tip(page, "slide")
    expect_length(slide$animations, 1L)
    expect_match(slide$animations, "translateY(-4px)", fixed = TRUE)
    expect_length(open_tip(page, "none")$animations, 0L)
    expect_length(open_tip(page, "unset")$animations, 0L)
  })

  it("lets a reader's reduced-motion setting win over fade and slide", {
    page$emulate("prefers-reduced-motion", "reduce")
    expect_length(open_tip(page, "slide")$animations, 0L)
    expect_length(open_tip(page, "fade")$animations, 0L)
    page$emulate("prefers-reduced-motion", "no-preference")
    expect_length(open_tip(page, "slide")$animations, 1L)
  })

  it("has no console error", {
    expect_identical(page$problems(), character())
  })
})

describe("size and motion on the page", {
  dir <- withr::local_tempdir()
  page <- local_page(
    style_page(
      dir,
      tip_style("sober", motion = "slide", gap = 18, max_width = 220, pin_width = 300),
      size_tag("base", text = long_text),
      size_tag("pinned", text = long_text, pin = TRUE),
      size_tag("own-gap", style = tip_style(gap = 2), text = long_text),
      size_tag("own-width", style = tip_style(max_width = 400), text = long_text),
      size_tag("quiet", style = tip_style(motion = "none"), text = long_text)
    ),
    environment()
  )

  it("applies to a tag with no style of its own", {
    expect_true(wait_for_engine(page))
    watch_animations(page)
    got <- open_tip(page, "base")
    expect_equal(got$width, 220, tolerance = 0.5)
    expect_length(got$animations, 1L)
    expect_match(got$animations, "translateY(-4px)", fixed = TRUE)
    expect_equal(open_tip(page, "pinned", click = TRUE)$width, 300, tolerance = 0.5)
  })

  it("keeps the page's motion and width on a tag that sets only the gap, value by value", {
    plain <- open_tip(page, "base")
    own <- open_tip(page, "own-gap")
    expect_equal(plain$gap - own$gap, 16, tolerance = 0.5)
    expect_equal(own$width, 220, tolerance = 0.5)
    expect_match(own$animations, "translateY(-4px)", fixed = TRUE)
  })

  it("keeps the page's gap and motion on a tag that sets only the width", {
    plain <- open_tip(page, "base")
    own <- open_tip(page, "own-width")
    expect_equal(own$gap, plain$gap, tolerance = 0.5)
    expect_equal(own$width, 400, tolerance = 0.5)
    expect_match(own$animations, "translateY(-4px)", fixed = TRUE)
  })

  it("lets a tag's motion replace the page's, keeping the page's width", {
    quiet <- open_tip(page, "quiet")
    expect_length(quiet$animations, 0L)
    expect_equal(quiet$width, 220, tolerance = 0.5)
  })

  it("has no console error", {
    expect_identical(page$problems(), character())
  })
})

describe("size and motion without a preset", {
  dir <- withr::local_tempdir()
  page <- local_page(
    style_page(
      dir,
      tip_style(gap = 14, max_width = 240),
      size_tag("base", text = long_text),
      size_tag("own", style = tip_style(gap = 0), text = long_text)
    ),
    environment()
  )

  it("reaches a tip on a page with no preset, from the page and from the tag", {
    expect_true(wait_for_engine(page))
    watch_animations(page)
    plain <- open_tip(page, "base")
    own <- open_tip(page, "own")
    expect_equal(plain$width, 240, tolerance = 0.5)
    expect_equal(plain$gap - own$gap, 14, tolerance = 0.5)
    expect_equal(own$width, 240, tolerance = 0.5)
  })
})

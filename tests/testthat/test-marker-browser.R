skip_if_no_browser()

# Markers in a real browser: the line each preset draws for an underline (and the nearest preset winning), the wave in
# the text's own colour, an explicit line beating the preset, no shift in the tag's box, the pointer, the contrast in
# light and dark, and an icon that stays on the line of the last word.
tags <- htmltools::tags

# One tipped word. `preset` is the tag's own tip preset; `css` is the tag's own inline style.
word <- function(id, marker = "underline", preset = NULL, css = NULL, text = "Baseline", ...) {
  tag <- tags$span(id = id, text, tabindex = "0", style = css, ...)
  suppressWarnings(tip_on(tag, paste("Tip", id), marker = marker, style = preset))
}

marker_page <- function(dir, ..., preset = NULL, dark = FALSE, name = "page.html") {
  page <- bslib::page_fluid(
    use_hovercraft(style = preset),
    if (dark) tags$script("document.documentElement.setAttribute('data-bs-theme', 'dark');"),
    ...
  )
  path <- file.path(dir, name)
  htmltools::save_html(page, path)
  path
}

# What an element draws: the line it asks of text-decoration, the drawn wave, and the text colour.
drawn <- function(page, id) {
  jsonlite::fromJSON(page$evaluate(sprintf(
    paste0(
      "(() => { const s = getComputedStyle(document.getElementById('%s'));",
      " return JSON.stringify({ line: s.textDecorationLine, style: s.textDecorationStyle, color: s.color,",
      " image: s.backgroundImage }); })()"
    ),
    id
  )))
}
is_wave <- function(x) grepl("radial-gradient", x$image, fixed = TRUE)
looks <- function(x) if (is_wave(x)) "wavy" else x$style
# The size of the tag's own box and of the container it sits in: a drawn line may reach below the tag (a background
# does), but must not move a line of text.
box_of <- function(page, id) {
  jsonlite::fromJSON(page$evaluate(sprintf(
    paste0(
      "(() => { const e = document.getElementById('%s'); const r = e.getBoundingClientRect();",
      " const c = e.parentElement.getBoundingClientRect(); return JSON.stringify([r.width, c.height, r.top - c.top]); })()"
    ),
    id
  )))
}

describe("an underline's line follows the tip preset", {
  dir <- withr::local_tempdir()
  path <- do.call(
    marker_page,
    c(
      list(dir),
      lapply(c(tip_preset_names(), "none"), function(name) word(paste0("p-", name), preset = name)),
      list(
        word("plain"),
        word("dotted", tip_marker("underline", line = "dotted"), preset = "lively"),
        word("dashed", tip_marker("underline", line = "dashed"), preset = "lively"),
        word("wavy", tip_marker("underline", line = "wavy"), preset = "sober")
      )
    )
  )
  page <- local_page(path, environment())
  expect_true(page$wait_until("document.getElementById('plain') !== null"))

  it("draws what the design says for each preset", {
    want <- c(
      none = "dotted",
      sober = "dotted",
      dense = "dotted",
      modern = "dashed",
      editorial = "dashed",
      lively = "wavy"
    )
    expect_setequal(c(tip_preset_names(), "none"), names(want))
    for (name in names(want)) {
      expect_identical(looks(drawn(page, paste0("p-", name))), unname(want[[name]]), info = name)
    }
  })

  it("draws dotted where no preset is set", {
    expect_identical(looks(drawn(page, "plain")), "dotted")
  })

  it("lets a line chosen on the tag beat the preset, both ways", {
    expect_identical(looks(drawn(page, "dotted")), "dotted") # lively would be a wave
    expect_identical(looks(drawn(page, "dashed")), "dashed")
    expect_identical(looks(drawn(page, "wavy")), "wavy") # sober would be dotted
  })

  it("draws the wave as a background, not the browser's zigzag", {
    got <- drawn(page, "p-lively")
    expect_identical(got$line, "none")
    expect_true(is_wave(got))
    expect_false(identical(got$style, "wavy"))
    dots <- drawn(page, "p-sober")
    expect_identical(dots$line, "underline")
    expect_identical(dots$image, "none")
  })
})

describe("the preset is read from the page, and the nearest one wins", {
  dir <- withr::local_tempdir()
  path <- marker_page(
    dir,
    preset = "lively",
    word("page-wide"),
    tags$div(id = "inner", `data-hovercraft-tip-preset` = "sober", word("nested")),
    word("own", preset = "modern"),
    name = "lively.html"
  )
  page <- local_page(path, environment())
  expect_true(page$wait_until("document.getElementById('own') !== null"))

  it("follows the page preset the style layer writes on the page", {
    expect_identical(page$evaluate("document.documentElement.getAttribute('data-hovercraft-tip-preset')"), "lively")
    expect_identical(looks(drawn(page, "page-wide")), "wavy")
  })

  it("lets a preset nearer the tag beat the page's, on an ancestor or on the tag", {
    expect_identical(looks(drawn(page, "nested")), "dotted")
    expect_identical(looks(drawn(page, "own")), "dashed")
  })
})

describe("the wave is drawn in the text's own colour", {
  dir <- withr::local_tempdir()
  path <- marker_page(
    dir,
    word("red", tip_marker("underline", line = "wavy"), css = "color: rgb(180, 35, 24);"),
    word("blue", tip_marker("underline", line = "wavy"), css = "color: rgb(10, 80, 200);"),
    word("plain-red", "none", css = "color: rgb(180, 35, 24);")
  )
  page <- local_page(path, environment())
  expect_true(page$wait_until("document.getElementById('red') !== null"))

  it("takes the colour of the element, whatever it is", {
    expect_match(drawn(page, "red")$image, "rgb\\(180, 35, 24\\)")
    expect_match(drawn(page, "blue")$image, "rgb\\(10, 80, 200\\)")
    expect_false(grepl("rgb\\(0, 0, 0\\)", drawn(page, "red")$image)) # not the browser's black
  })

  it("follows the text when its colour changes, which a drawn picture file cannot", {
    page$evaluate("document.getElementById('red').style.color = 'rgb(250, 240, 230)';")
    expect_match(drawn(page, "red")$image, "rgb\\(250, 240, 230\\)")
    expect_false(grepl("rgb\\(180, 35, 24\\)", drawn(page, "red")$image))
  })

  it("draws pixels, not only a style: the wave's red is on the screen, more than the same word has without it", {
    page$evaluate(paste0(
      "for (const id of ['red', 'plain-red']) { const e = document.getElementById(id);",
      " e.style.color = 'rgb(180, 35, 24)'; e.style.fontSize = '40px'; }"
    ))
    # The strip under the word. The browser decodes its own screenshot and counts the pixels that are the text's red.
    red_pixels <- function(id) {
      box <- jsonlite::fromJSON(page$evaluate(sprintf(
        paste0(
          "(() => { const r = document.getElementById('%s').getBoundingClientRect();",
          " return JSON.stringify({ bottom: r.bottom, left: r.left, right: r.right }); })()"
        ),
        id
      )))
      data <- page$screenshot(c(box$left, box$bottom - 7, box$right, box$bottom + 4))
      page$evaluate(sprintf(
        paste0(
          "window.__red = null; (() => { const img = new Image(); img.onload = () => {",
          " const c = document.createElement('canvas'); c.width = img.width; c.height = img.height;",
          " const g = c.getContext('2d'); g.drawImage(img, 0, 0);",
          " const d = g.getImageData(0, 0, c.width, c.height).data;",
          " let n = 0; for (let i = 0; i < d.length; i += 4) { if (d[i] > 140 && d[i + 1] < 80 && d[i + 2] < 80) n++; }",
          " window.__red = n; }; img.src = 'data:image/png;base64,%s'; })(); true"
        ),
        data
      ))
      expect_true(page$wait_until("window.__red !== null"))
      page$evaluate("window.__red")
    }
    with_wave <- red_pixels("red")
    without <- red_pixels("plain-red")
    expect_lt(without, 10) # below the text, a word with no marker has nothing of that colour
    expect_gt(with_wave, 200)
  })
})

describe("a marker moves nothing", {
  dir <- withr::local_tempdir()
  path <- marker_page(
    dir,
    tags$p(word("w-none", "none")),
    tags$p(word("w-underline")),
    tags$p(word("w-wavy", tip_marker("underline", line = "wavy"))),
    tags$p(word("w-corner", "corner")),
    tags$p(word("w-icon", "icon")),
    tags$p(
      suppressWarnings(tip_on(
        tags$label(id = "l-none", "Site", tabindex = "0", class = "form-label"),
        "Tip",
        marker = "none"
      ))
    ),
    tags$p(
      suppressWarnings(tip_on(
        tags$label(id = "l-wavy", "Site", tabindex = "0", class = "form-label"),
        "Tip",
        marker = tip_marker("underline", line = "wavy")
      ))
    ),
    tags$table(tags$tbody(tags$tr(
      suppressWarnings(tip_on(tags$td(id = "c-none", "14.8%", tabindex = "0"), "Cell", marker = "none"))
    ))),
    tags$table(tags$tbody(tags$tr(
      suppressWarnings(tip_on(
        tags$td(id = "c-under", "14.8%", tabindex = "0"),
        "Cell",
        marker = tip_marker("underline", line = "wavy")
      ))
    ))),
    tags$table(tags$tbody(tags$tr(
      suppressWarnings(tip_on(tags$td(id = "c-corner", "14.8%", tabindex = "0"), "Cell", marker = "corner"))
    )))
  )
  page <- local_page(path, environment())
  expect_true(page$wait_until("document.getElementById('w-icon') !== null"))

  it("leaves the box of a tag with an underline or a corner exactly as an unmarked one", {
    base <- box_of(page, "w-none")
    for (id in c("w-underline", "w-wavy", "w-corner")) {
      expect_equal(box_of(page, id), base, info = id)
    }
    cell <- box_of(page, "c-none")
    for (id in c("c-under", "c-corner")) {
      expect_equal(box_of(page, id), cell, info = id)
    }
    # a Bootstrap label keeps its own margin: the container is as tall as one with no marker
    expect_equal(box_of(page, "l-wavy"), box_of(page, "l-none"))
  })

  it("lets an icon take its own small width and no height", {
    got <- box_of(page, "w-icon")
    base <- box_of(page, "w-none")
    expect_gt(got[[1]], base[[1]])
    expect_equal(got[[2]], base[[2]])
  })

  it("does not change when the pointer rests on it or keyboard focus arrives", {
    before <- box_of(page, "w-wavy")
    page$hover("#w-wavy")
    expect_equal(box_of(page, "w-wavy"), before)
    page$evaluate("document.getElementById('w-wavy').focus()")
    page$hold(0.3) # the focus must change nothing: nothing happening has no signal
    expect_equal(box_of(page, "w-wavy"), before)
  })
})

describe("the pointer", {
  dir <- withr::local_tempdir()
  pinned <- suppressWarnings(tip_on(tags$span(id = "pinned", "Pin", tabindex = "0"), "Tip", marker = "underline"))
  pinned <- htmltools::tagAppendAttributes(pinned, `data-hovercraft-tip-pin` = "")
  path <- marker_page(
    dir,
    word("hint"),
    word("none-marker", "none"),
    pinned,
    suppressWarnings(tip_on(tags$button(id = "btn", "Run"), "Tip", marker = "underline")),
    suppressWarnings(tip_on(tags$a(id = "link", href = "#", "Link"), "Tip", marker = "icon"))
  )
  page <- local_page(path, environment())
  expect_true(page$wait_until("document.getElementById('link') !== null"))
  cursor <- function(id) page$evaluate(sprintf("getComputedStyle(document.getElementById('%s')).cursor", id))

  it("is help on a marked tag that does nothing on a click, never pointer", {
    expect_identical(cursor("hint"), "help")
    expect_identical(cursor("none-marker"), "auto")
  })

  it("is a hand when the tip can be pinned", {
    expect_identical(cursor("pinned"), "pointer")
  })

  it("is left to a button and a link, which keep their own", {
    expect_false(identical(cursor("btn"), "help"))
    expect_false(identical(cursor("link"), "help"))
  })
})

describe("the contrast of a marker against the page, in light and dark", {
  dir <- withr::local_tempdir()
  items <- list(
    word("u"),
    word("w", tip_marker("underline", line = "wavy")),
    word("i", "icon"),
    word("c", "corner")
  )
  luminance <- paste0(
    "const lum = (c) => { const v = c.match(/[\\d.]+/g).slice(0, 3).map((x) => { x = x / 255;",
    " return x <= 0.03928 ? x / 12.92 : Math.pow((x + 0.055) / 1.055, 2.4); });",
    " return 0.2126 * v[0] + 0.7152 * v[1] + 0.0722 * v[2]; };",
    " const ratio = (a, b) => { const x = lum(a), y = lum(b); return (Math.max(x, y) + 0.05) / (Math.min(x, y) + 0.05); };"
  )
  for (dark in c(FALSE, TRUE)) {
    mode <- if (dark) "dark" else "light"
    path <- do.call(marker_page, c(list(dir), items, list(dark = dark, name = paste0(mode, ".html"))))
    page <- local_page(path, environment())
    expect_true(page$wait_until("document.getElementById('c') !== null"))
    ratios <- jsonlite::fromJSON(page$evaluate(paste0(
      "(() => { ",
      luminance,
      " const page = getComputedStyle(document.body).backgroundColor;",
      " const out = {}; for (const id of ['u', 'w', 'i', 'c']) {",
      "   const e = document.getElementById(id); const s = getComputedStyle(e);",
      "   const a = getComputedStyle(e, '::after');",
      # the line is the text's colour, and the icon and corner are drawn with background-color: currentColor
      "   out[id] = ratio(id === 'i' || id === 'c' ? a.backgroundColor : s.color, page); }",
      " return JSON.stringify(out); })()"
    )))
    it(paste("keeps at least 3:1 for every marker in", mode), {
      expect_equal(page$evaluate("document.documentElement.getAttribute('data-bs-theme') === 'dark'"), dark)
      for (id in names(ratios)) {
        expect_gte(ratios[[id]], 3, label = paste(id, mode))
      }
    })
  }
})

describe("an icon stays with the last word", {
  dir <- withr::local_tempdir()
  long <- paste(rep("review", 14), collapse = " ")
  path <- marker_page(
    dir,
    tags$div(style = "width: 220px", word("built-in", "icon", text = long)),
    tags$div(
      style = "width: 220px",
      word("custom", tip_marker("icon", icon = tags$b("?")), text = long)
    )
  )
  page <- local_page(path, environment())
  expect_true(page$wait_until("document.getElementById('custom') !== null"))

  it("never sits alone on a line, built-in or custom", {
    # The icon's line is the line of the last word: the icon's top is not below the top of the last word's line.
    lines <- jsonlite::fromJSON(page$evaluate(paste0(
      "(() => { const out = {}; for (const id of ['built-in', 'custom']) {",
      " const e = document.getElementById(id); const range = document.createRange();",
      " const text = e.firstChild; range.setStart(text, text.length - 3); range.setEnd(text, text.length);",
      " const word = range.getClientRects(); const last = word[word.length - 1];",
      " const mark = id === 'custom' ? e.querySelector('.hc-marker-icon').getBoundingClientRect() : null;",
      " const e2 = e.getBoundingClientRect();",
      " out[id] = { wordBottom: last.bottom, markTop: mark ? mark.top : e2.bottom - 2, box: e2.height }; }",
      " return JSON.stringify(out); })()"
    )))
    # The icon begins above the bottom of the last word's rectangle, so it shares the line.
    expect_lt(lines[["custom"]]$markTop, lines[["custom"]]$wordBottom)
    # The built-in icon is a pseudo-element: the box does not grow by a whole line when the icon is added.
    single <- jsonlite::fromJSON(page$evaluate(
      "(() => { const e = document.getElementById('built-in'); return JSON.stringify(e.getClientRects().length); })()"
    ))
    expect_gte(single, 1)
  })

  it("puts the gap between the icon and the words when the page reads right to left", {
    position <- function() {
      page$evaluate("getComputedStyle(document.getElementById('built-in'), '::after').maskPosition")
    }
    expect_match(position(), "^0\\.28em|^4\\.48px|^4\\.[0-9]+px") # left to right: room before the icon
    page$evaluate("document.documentElement.setAttribute('dir', 'rtl')")
    expect_match(position(), "^0(%|px)") # right to left: the icon is at the box's far edge
    page$evaluate("document.documentElement.removeAttribute('dir')")
  })

  it("has no console error", {
    expect_identical(page$problems(), character())
  })
})

describe("a marked tag is highlighted while its tip is open", {
  dir <- withr::local_tempdir()
  box <- tags$div(
    tags$span(id = "label", tabindex = "0", "Total"),
    id = "vbox",
    class = "bslib-value-box",
    tabindex = "0"
  )
  path <- marker_page(
    dir,
    word("w-underline"),
    word("w-icon", "icon"),
    word("w-none", "none"),
    suppressWarnings(tip_on(box, "Box tip", marker = "icon")),
    tags$p("Elsewhere", id = "elsewhere")
  )
  page <- local_page(path, environment())
  expect_true(page$wait_until("document.getElementById('w-none') !== null"))
  open_attr <- tip_attribute("open")
  state <- function(id) {
    jsonlite::fromJSON(page$evaluate(sprintf(
      paste0(
        "(() => { const e = document.getElementById('%s'); const s = getComputedStyle(e);",
        " return JSON.stringify({ open: e.getAttribute('%s'), fill: s.backgroundColor, shadow: s.boxShadow }); })()"
      ),
      id,
      open_attr
    )))
  }
  transparent <- "rgba(0, 0, 0, 0)"

  it("tints an underlined word and an icon's tag while the tip shows, and not before or after", {
    for (id in c("w-underline", "w-icon")) {
      expect_identical(state(id)$fill, transparent, info = id)
      page$hover(paste0("#", id))
      got <- state(id)
      expect_identical(got$open, "hint", info = id)
      expect_match(got$fill, "^rgba\\(127, 127, 127", info = id)
      page$hover("#elsewhere", opens = FALSE)
      expect_true(
        page$wait_until(sprintf(
          "(() => { const e = document.getElementById('%s'); return e.getAttribute('%s') === null && getComputedStyle(e).backgroundColor === '%s'; })()",
          id,
          open_attr,
          transparent
        )),
        info = id
      )
      expect_identical(state(id)$fill, transparent, info = id)
      expect_null(state(id)$open, info = id)
    }
  })

  it("does not tint a tag with no marker, though its tip is open", {
    page$hover("#w-none")
    expect_identical(state("w-none")$open, "hint")
    expect_identical(state("w-none")$fill, transparent)
  })

  it("glows round a value box and leaves its own background", {
    expect_identical(state("vbox")$shadow, "none")
    page$hover("#vbox")
    got <- state("vbox")
    expect_identical(got$open, "hint")
    expect_false(identical(got$shadow, "none"))
    expect_identical(got$fill, transparent)
  })
})

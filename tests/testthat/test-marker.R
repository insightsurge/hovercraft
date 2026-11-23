skip_if_not_installed("withr")
quiet <- function(code) suppressWarnings(code)
attr_of <- function(tag, name) htmltools::tagGetAttribute(tag, name)
marked <- function(tag, marker, ...) quiet(tip_on(tag, "A tip", marker = marker, ...))
span <- function(...) htmltools::tags$span("Word", tabindex = "0", ...)
marker_attribute <- function(key = NULL) {
  paste0("data-", tip_names()$prefix, "-marker", if (!is.null(key)) paste0("-", key))
}
has_marker_dep <- function(tag) {
  "hovercraft-marker" %in% vapply(htmltools::htmlDependencies(tag), function(d) d$name, "")
}

describe("tip_marker()", {
  it("holds the type, with a line only for an underline and an icon only for an icon", {
    expect_s3_class(tip_marker("underline"), "hovercraft_marker")
    expect_identical(tip_marker("underline", line = "wavy")$line, "wavy")
    expect_null(tip_marker("underline")$line)
    expect_identical(tip_marker("icon", icon = htmltools::tags$b("?"))$icon, htmltools::tags$b("?"))
  })

  it("takes the four types and no others, naming the close one", {
    for (type in c("none", "underline", "icon", "corner")) {
      expect_identical(tip_marker(type)$type, type)
    }
    expect_error(tip_marker("undrline"), class = "hovercraft_error_marker_type")
    expect_error(tip_marker("undrline"), "underline")
    expect_error(tip_marker(), class = "hovercraft_error_marker_type")
    expect_error(tip_marker(c("icon", "corner")), class = "hovercraft_error_marker_type")
    expect_error(tip_marker(NA_character_), class = "hovercraft_error_marker_type")
  })

  it("stops on a line for any type but underline, and on a line that is not one", {
    expect_error(tip_marker("corner", line = "wavy"), class = "hovercraft_error_marker_line")
    expect_error(tip_marker("icon", line = "dotted"), class = "hovercraft_error_marker_line")
    expect_error(tip_marker("underline", line = "wavey"), "wavy")
    expect_error(tip_marker("underline", line = c("dotted", "dashed")), class = "hovercraft_error_marker_line")
    for (line in c("dotted", "dashed", "wavy")) {
      expect_identical(tip_marker("underline", line = line)$line, line)
    }
  })

  it("stops on an icon for any type but icon, and on an icon that is a name", {
    expect_error(tip_marker("underline", icon = htmltools::tags$b("?")), class = "hovercraft_error_marker_icon")
    expect_error(tip_marker("icon", icon = "info"), class = "hovercraft_error_marker_icon")
    expect_error(tip_marker("icon", icon = "info"), "bs_icon")
    expect_error(tip_marker("icon", icon = "info"), "fa\\(")
    expect_error(tip_marker("icon", icon = 3), class = "hovercraft_error_marker_icon")
  })

  it("takes markup as an icon, as an icon package such as fontawesome gives", {
    expect_s3_class(tip_marker("icon", icon = htmltools::HTML("<svg></svg>")), "hovercraft_marker")
    expect_identical(tip_as_marker(htmltools::HTML("<svg></svg>"))$type, "icon")
  })

  it("makes a misspelt argument an error", {
    expect_error(tip_marker("underline", lines = "wavy"), class = "hovercraft_error_marker")
    expect_error(tip_marker("underline", "wavy"), class = "hovercraft_error_marker")
  })

  it("prints as the call that made it", {
    expect_identical(format(tip_marker("underline")), 'tip_marker("underline")')
    expect_identical(format(tip_marker("underline", line = "wavy")), 'tip_marker("underline", line = "wavy")')
    expect_identical(format(tip_marker("icon", icon = htmltools::tags$b("?"))), 'tip_marker("icon", icon = <tag>)')
    expect_output(print(tip_marker("corner")), 'tip_marker("corner")', fixed = TRUE)
    expect_identical(print(tip_marker("corner")) |> utils::capture.output() |> length(), 1L)
  })
})

describe("tip_as_marker()", {
  it("reads every accepted form as one marker", {
    expect_identical(tip_as_marker(NULL)$type, "none")
    expect_identical(tip_as_marker("none")$type, "none")
    expect_identical(tip_as_marker("corner")$type, "corner")
    marker <- tip_marker("underline", line = "dashed")
    expect_identical(tip_as_marker(marker), marker)
  })

  it("reads a bare tag as a custom icon, the same as tip_marker('icon', icon = tag)", {
    tag <- htmltools::tags$b("?")
    expect_identical(tip_as_marker(tag), tip_marker("icon", icon = tag))
  })

  it("stops on a list, a number or a word that is not a type", {
    expect_error(tip_as_marker(list("underline")), class = "hovercraft_error_marker_type")
    expect_error(tip_as_marker(3), class = "hovercraft_error_marker_type")
    expect_error(tip_as_marker("wavy"), class = "hovercraft_error_marker_type")
    expect_error(tip_as_marker(I("underline")), class = "hovercraft_error_marker_type")
  })
})

describe("tip_on(marker =)", {
  it("draws nothing by default and attaches no marker stylesheet", {
    tag <- quiet(tip_on(span(), "A tip"))
    expect_null(attr_of(tag, marker_attribute()))
    expect_false(has_marker_dep(tag))
    expect_false(has_marker_dep(marked(span(), "none")))
    expect_false(has_marker_dep(marked(span(), NULL)))
  })

  it("writes the type as one attribute and attaches the stylesheet once a marker is used", {
    for (type in c("underline", "icon", "corner")) {
      tag <- marked(span(), type)
      expect_identical(attr_of(tag, marker_attribute()), type)
      expect_true(has_marker_dep(tag))
    }
  })

  it("keeps the tag's own attributes and the tip", {
    tag <- marked(span(id = "w"), "corner")
    expect_identical(attr_of(tag, "id"), "w")
    expect_identical(attr_of(tag, tip_attributes()[["text"]]), "A tip")
  })

  it("writes the line only when one is chosen, so the preset decides otherwise", {
    expect_null(attr_of(marked(span(), "underline"), marker_attribute("line")))
    expect_null(attr_of(marked(span(), tip_marker("underline")), marker_attribute("line")))
    for (line in c("dotted", "dashed", "wavy")) {
      expect_identical(attr_of(marked(span(), tip_marker("underline", line = line)), marker_attribute("line")), line)
    }
  })

  it("adds a custom icon as a hidden child and says so, so no CSS :has() is needed", {
    tag <- marked(span(), tip_marker("icon", icon = htmltools::tags$b("?")))
    expect_identical(attr_of(tag, marker_attribute("icon")), "custom")
    html <- as.character(tag)
    expect_match(html, "<span class=\"hc-marker-icon\" aria-hidden=\"true\">", fixed = TRUE)
    expect_match(html, "<b>?</b>", fixed = TRUE)
    # the word joiner comes first, with no space between it and the icon
    expect_match(html, "&#8288;<span class=\"hc-marker-icon\"", fixed = TRUE)
  })

  it("takes a bare tag as the icon, and the built-in icon draws no child", {
    expect_identical(
      as.character(marked(span(), htmltools::tags$b("?"))),
      as.character(marked(span(), tip_marker("icon", icon = htmltools::tags$b("?"))))
    )
    builtin <- marked(span(), "icon")
    expect_null(attr_of(builtin, marker_attribute("icon")))
    expect_false(grepl("hc-marker-icon", as.character(builtin), fixed = TRUE))
  })

  it("keeps the dependencies of the icon", {
    skip_if_not_installed("shiny")
    tag <- marked(span(), shiny::icon("flag"))
    names <- vapply(htmltools::htmlDependencies(tag), function(d) d$name, "")
    expect_true("font-awesome" %in% names)
    expect_true("hovercraft-marker" %in% names)
  })

  it("puts the words of a block tag in a span for an underline, and leaves an inline tag as it is", {
    cell <- marked(htmltools::tags$td("14.8%", tabindex = "0"), "underline")
    expect_match(as.character(cell), "<span class=\"hc-marker-text\">14.8%</span>", fixed = TRUE)
    expect_identical(attr_of(cell, marker_attribute("text")), "words")
    word <- marked(span(), "underline")
    expect_false(grepl("hc-marker-text", as.character(word), fixed = TRUE))
    expect_null(attr_of(word, marker_attribute("text")))
    # a button is not a line of text either: its words are wrapped
    expect_match(as.character(marked(htmltools::tags$button("Run"), "underline")), "hc-marker-text", fixed = TRUE)
  })

  it("wraps each run of words in a block tag and leaves a block child where it is", {
    box <- marked(
      htmltools::tags$div(
        "Intro ",
        htmltools::tags$strong("bold"),
        htmltools::tags$p("A block"),
        "after",
        htmltools::tagList("in a list"),
        tabindex = "0"
      ),
      "underline"
    )
    html <- as.character(box)
    expect_equal(lengths(regmatches(html, gregexpr("hc-marker-text", html, fixed = TRUE))), 2L)
    expect_match(html, "<p>A block</p>", fixed = TRUE)
    expect_match(html, "in a list", fixed = TRUE)
  })

  it("stops on an underline for a block tag that holds no words, and says where the marker goes", {
    block <- htmltools::tags$div(htmltools::tags$p("Only a block"), tabindex = "0")
    expect_error(quiet(tip_on(block, "A tip", marker = "underline")), class = "hovercraft_error_marker_tag")
    expect_error(quiet(tip_on(block, "A tip", marker = "underline")), "label")
    # an icon or a corner needs no words
    expect_no_error(marked(block, "corner"))
    expect_no_error(marked(block, "icon"))
    skip_if_not_installed("bslib")
    no_title <- bslib::card(bslib::card_body("Body"))
    expect_error(quiet(tip_on(no_title, "A tip", marker = "underline")), class = "hovercraft_error_marker_tag")
  })

  it("marks the title of a value box, and its icon goes in the title too", {
    skip_if_not_installed("bslib")
    box <- function() bslib::value_box("Flagged", "14.8%")
    under <- marked(box(), "underline")
    html <- as.character(under)
    expect_match(html, "<p class=\"value-box-title\">\\s*<span class=\"hc-marker-text\">Flagged</span>")
    expect_false(grepl("hc-marker-text\">14.8%", html, fixed = TRUE))
    expect_identical(attr_of(under, marker_attribute("text")), "words")
    icon <- as.character(marked(box(), htmltools::tags$b("?")))
    expect_match(icon, "Flagged\\s*&#8288;<span class=\"hc-marker-icon\"")
    expect_match(icon, "</span>\\s*</p>")
    builtin <- marked(box(), "icon")
    expect_false(grepl("hc-marker-icon", as.character(builtin), fixed = TRUE))
  })

  it("stops on a tag that holds nothing, naming the fix", {
    expect_error(
      quiet(tip_on(htmltools::tags$input(), "A tip", marker = "icon")),
      class = "hovercraft_error_marker_tag"
    )
    expect_error(quiet(tip_on(htmltools::tags$img(), "A tip", marker = "corner")), "around it")
    expect_no_error(marked(htmltools::tags$input(), "none"))
  })

  it("stops on a marker that is not one, before anything else is done", {
    expect_error(quiet(tip_on(span(), "A tip", marker = "under")), class = "hovercraft_error_marker_type")
    expect_error(quiet(tip_on(span(), "A tip", marker = list())), class = "hovercraft_error_marker_type")
    expect_error(quiet(tip_on(span(), "A tip", marker = TRUE)), class = "hovercraft_error_marker_type")
  })

  it("is replaced by a later call: its attributes, its icon and its span go with it", {
    first <- marked(htmltools::tags$td("14.8%", tabindex = "0"), "underline")
    again <- quiet(tip_on(first, "Another tip"))
    expect_null(attr_of(again, marker_attribute()))
    expect_null(attr_of(again, marker_attribute("text")))
    expect_false(grepl("hc-marker", as.character(again), fixed = TRUE))
    expect_match(as.character(again), "<td[^>]*>14.8%</td>")

    iconed <- marked(span(), tip_marker("icon", icon = htmltools::tags$b("?")))
    cleared <- quiet(tip_on(iconed, "Another tip"))
    expect_false(grepl("hc-marker|8288", as.character(cleared)))

    swapped <- quiet(tip_on(first, "Another tip", marker = "icon"))
    expect_identical(attr_of(swapped, marker_attribute()), "icon")
    expect_false(grepl("hc-marker-text", as.character(swapped), fixed = TRUE))
  })

  it("marks a tag once even when it is called twice with the same marker", {
    once <- marked(htmltools::tags$td("14.8%", tabindex = "0"), "underline")
    twice <- quiet(tip_on(once, "A tip", marker = "underline"))
    expect_equal(
      lengths(regmatches(as.character(twice), gregexpr("hc-marker-text", as.character(twice), fixed = TRUE))),
      1L
    )
  })

  it("does not draw a marker where there is no tip", {
    tag <- quiet(tip_on(span(), NA_character_, marker = "underline"))
    expect_null(attr_of(tag, marker_attribute()))
  })

  it("leaves the tip's other features alone", {
    tag <- marked(span(), "corner", style = "sober")
    expect_identical(attr_of(tag, tip_attribute("preset")), "sober")
    expect_identical(attr_of(tag, marker_attribute()), "corner")
  })

  it("does not pass marker through ... : a misspelt argument is still an error", {
    expect_error(quiet(tip_on(span(), "A tip", markers = "icon")), class = "hovercraft_error_argument")
  })
})

describe("the marker stylesheet", {
  css <- paste(readLines(system.file("marker", "marker.css", package = "hovercraft")), collapse = "\n")
  code <- gsub("(?s)/\\*.*?\\*/", "", css, perl = TRUE) # the comments say what the code avoids

  it("spells the attributes and the classes R writes", {
    expect_match(css, paste0("[", marker_attribute(), "=\"underline\"]"), fixed = TRUE)
    expect_match(css, paste0("[", marker_attribute(), "=\"icon\"]"), fixed = TRUE)
    expect_match(css, paste0("[", marker_attribute(), "=\"corner\"]"), fixed = TRUE)
    for (key in c("line", "text", "icon")) {
      expect_match(css, paste0("[", marker_attribute(key)), fixed = TRUE)
    }
    for (key in c("text", "icon")) {
      expect_match(css, paste0(".", tip_marker_class(key)), fixed = TRUE)
    }
  })

  it("reads the engine's own preset and pin attributes, and every preset the engine ships", {
    expect_match(css, paste0("[", tip_attribute("preset"), "=\"lively\"]"), fixed = TRUE)
    expect_match(css, paste0("[", tip_attribute("pin"), "]"), fixed = TRUE)
    for (name in tip_preset_names()) {
      expect_match(css, paste0("[", tip_attribute("preset"), "=\"", name, "\"]"), fixed = TRUE)
    }
  })

  it("holds no :has(), no color-mix() and no hard-coded colour, so it works from the browser floor", {
    expect_false(grepl(":has(", code, fixed = TRUE))
    expect_false(grepl("color-mix(", code, fixed = TRUE))
    expect_false(grepl("#[0-9a-fA-F]{3,6}\\b", gsub("%23", "", code)))
  })
})

describe("tip_marker_dependency()", {
  it("is its own dependency, named for the marker layer and served from inst/marker", {
    dep <- tip_marker_dependency()
    expect_identical(dep$name, "hovercraft-marker")
    expect_identical(dep$stylesheet, "marker.css")
    expect_true(file.exists(file.path(system.file("marker", package = "hovercraft"), "marker.css")))
  })
})

skip_if_not_installed("withr")

disabled_button <- function() htmltools::tags$button("Run", disabled = NA)
attrs <- function(tag) unlist(tag$attribs)

describe("tip_on(when_disabled = )", {
  it("puts the control in a wrapper that holds the reason, and attaches the layer", {
    got <- tip_on(disabled_button(), "Runs the **check**", when_disabled = "Pick a dataset first")
    expect_identical(got$name, "span")
    expect_identical(attrs(got)[["class"]], tip_disabled_class())
    expect_identical(attrs(got)[[tip_disabled_attribute()]], "Pick a dataset first")
    inner <- got$children[[1]]
    expect_identical(inner$name, "button")
    expect_identical(attrs(inner)[[tip_names()$attribute]], "Runs the **check**")
    names <- vapply(htmltools::findDependencies(got), function(x) x$name, "")
    expect_true(all(c("hovercraft", "hovercraft-disabled") %in% names))
  })

  it("does not write the engine's reason attribute itself: the layer does, while the control is disabled", {
    got <- tip_on(disabled_button(), "Runs", when_disabled = "Why")
    expect_false(tip_attribute("reason") %in% names(got$attribs))
    expect_false(tip_attribute("reason") %in% names(got$children[[1]]$attribs))
  })

  it("leaves a tag without when_disabled as it was, with no wrapper and no layer", {
    got <- tip_on(disabled_button(), "Runs")
    expect_identical(got$name, "button")
    expect_false("hovercraft-disabled" %in% vapply(htmltools::findDependencies(got), function(x) x$name, ""))
  })

  it("takes a tag that has only a reason, and still asks for text or a reason when neither is given", {
    got <- tip_on(disabled_button(), when_disabled = "Why")
    expect_identical(got$children[[1]]$name, "button")
    expect_false(tip_names()$attribute %in% names(got$children[[1]]$attribs))
    expect_error(tip_on(disabled_button()), class = "hovercraft_error_tip")
  })

  it("keeps the id and the events on the control and finds the control in a Shiny input's own div", {
    got <- tip_on(disabled_button(), "Runs", id = "run_tip", events = "hint", when_disabled = "Why")
    inner <- got$children[[1]]
    expect_identical(attrs(inner)[[tip_attribute("id")]], "run_tip")
    expect_identical(attrs(inner)[[tip_shiny_attribute()]], "hint")
    skip_if_not_installed("shiny")
    expect_identical(tip_on(shiny::actionButton("go", "Go"), when_disabled = "Why")$name, "span")
  })

  it("gives the wrapper the width the control asked for, so a layout does not move", {
    skip_if_not_installed("shiny")
    wide <- tip_on(shiny::actionButton("go", "Go", width = "100%"), when_disabled = "Why")
    expect_match(attrs(wide)[[tip_disabled_attribute("style")]], "width:100%", fixed = TRUE)
    expect_false("style" %in% names(wide$attribs))
    full <- tip_on(htmltools::tags$button("Go", class = "btn w-100 d-block"), when_disabled = "Why")
    expect_identical(attrs(full)[["class"]], paste(tip_disabled_class(), "w-100 d-block"))
    plain <- tip_on(htmltools::tags$button("Go", style = "color: red"), when_disabled = "Why")
    expect_false("style" %in% names(plain$attribs))
    expect_false(tip_disabled_attribute("style") %in% names(plain$attribs))
    expect_identical(attrs(plain$children[[1]])[["style"]], "color: red;")
  })

  it("moves the width and the place in a layout to the wrapper, and the control fills the wrapper", {
    half <- tip_on(htmltools::tags$button("Go", style = "width: 50%; color: red"), when_disabled = "Why")
    # The wrapper's layout is data, which the layer applies from script; the control fills it, by a mark and not a style.
    expect_identical(attrs(half)[[tip_disabled_attribute("style")]], "width: 50%;")
    expect_false("style" %in% names(half$attribs))
    expect_identical(attrs(half$children[[1]])[["style"]], "color: red;")
    expect_true(tip_disabled_attribute("fill") %in% names(half$children[[1]]$attribs))
    classes <- tip_on(htmltools::tags$button("Go", class = "btn w-50 flex-fill ms-auto btn-sm"), when_disabled = "Why")
    expect_identical(attrs(classes)[["class"]], paste(tip_disabled_class(), "w-50 flex-fill ms-auto"))
    expect_identical(attrs(classes$children[[1]])[["class"]], "btn btn-sm")
    expect_true(tip_disabled_attribute("fill") %in% names(classes$children[[1]]$attribs))
    block <- tip_on(htmltools::tags$button("Go", class = "d-block"), when_disabled = "Why")
    expect_identical(attrs(block)[["class"]], paste(tip_disabled_class(), "d-block"))
    expect_identical(attrs(block$children[[1]])[["class"]], "d-block")
    flex <- tip_on(htmltools::tags$button("Go", style = "flex: 1 1 0; order: 2"), when_disabled = "Why")
    expect_identical(attrs(flex)[[tip_disabled_attribute("style")]], "flex: 1 1 0; order: 2;")
    expect_false("style" %in% names(flex$children[[1]]$attribs))
    expect_true(tip_disabled_attribute("fill") %in% names(flex$children[[1]]$attribs))
  })

  it("wraps a block tag, such as the div Shiny puts around an input, in a block", {
    skip_if_not_installed("shiny")
    got <- tip_on(shiny::textInput("name", "Name"), when_disabled = "Sign in first")
    expect_identical(got$name, "div")
    expect_match(attrs(got)[["class"]], paste0(tip_disabled_class(), "-block"), fixed = TRUE)
  })

  it("writes the look and the placement on the wrapper as well, which the engine reads from the item upwards", {
    got <- tip_on(
      disabled_button(),
      "Runs",
      style = tip_style("sober", theme = list("--hovercraft-tip-radius" = "13px")),
      placement = "right",
      when_disabled = "Why"
    )
    inner <- got$children[[1]]
    for (key in c("preset", "placement")) {
      expect_identical(attrs(got)[[tip_attribute(key)]], attrs(inner)[[tip_attribute(key)]])
    }
    expect_identical(attrs(got)[[tip_attribute("placement")]], "right")
    expect_identical(attrs(got)[[tip_theme_attribute()]], attrs(inner)[[tip_theme_attribute()]])
  })

  it("wraps once: a second call changes the reason and the tip of the same wrapper", {
    first <- tip_on(disabled_button(), "One", style = "sober", when_disabled = "First reason")
    again <- tip_on(first, "Two", when_disabled = "Second reason")
    expect_identical(again$name, "span")
    expect_identical(again$children[[1]]$name, "button")
    expect_identical(attrs(again)[[tip_disabled_attribute()]], "Second reason")
    expect_identical(attrs(again$children[[1]])[[tip_names()$attribute]], "Two")
    # The look of the first call went with it, as a tip made before is replaced.
    expect_false(tip_attribute("preset") %in% names(again$attribs))
    kept <- tip_on(first, "Two")
    expect_identical(attrs(kept)[[tip_disabled_attribute()]], "First reason")
    expect_identical(kept$children[[1]]$name, "button")
  })

  it("leaves a marker off a tag that has a reason and no tip of its own, and says so", {
    expect_warning(
      got <- tip_on(disabled_button(), marker = "corner", when_disabled = "Why"),
      class = "hovercraft_warning_marker"
    )
    expect_false(any(grepl(tip_marker_attribute(), names(got$children[[1]]$attribs), fixed = TRUE)))
    expect_no_warning(marked <- tip_on(disabled_button(), "Runs", marker = "corner", when_disabled = "Why"))
    expect_true(any(grepl(tip_marker_attribute(), names(marked$children[[1]]$attribs), fixed = TRUE)))
  })

  it("refuses a reason that is only blanks, since the tip would have nothing to say", {
    expect_error(tip_on(disabled_button(), "x", when_disabled = "   "), class = "hovercraft_error_argument")
    expect_error(tip_on(disabled_button(), "x", when_disabled = "\t "), "blanks")
  })

  it("refuses a reason that is not one non-empty string, with the fix", {
    expect_error(tip_on(disabled_button(), "x", when_disabled = c("a", "b")), class = "hovercraft_error_argument")
    expect_error(tip_on(disabled_button(), "x", when_disabled = ""), class = "hovercraft_error_argument")
    expect_error(tip_on(disabled_button(), "x", when_disabled = NA_character_), class = "hovercraft_error_argument")
    expect_error(tip_on(disabled_button(), "x", when_disabled = 1), "string")
  })

  it("refuses a tag with no control that can be disabled inside it, and says which controls do", {
    expect_error(
      tip_on(htmltools::tags$div("Run"), "x", when_disabled = "Why"),
      class = "hovercraft_error_when_disabled"
    )
    expect_error(tip_on(htmltools::tags$a("Run", href = "#"), "x", when_disabled = "Why"), "button")
    expect_no_error(tip_on(htmltools::tags$div(htmltools::tags$input(type = "text")), "x", when_disabled = "Why"))
  })
})

describe("the layer's files", {
  css <- paste(readLines(system.file("disabled", "disabled.css", package = "hovercraft")), collapse = "\n")
  js <- paste(readLines(system.file("disabled", "disabled.js", package = "hovercraft")), collapse = "\n")

  it("spell the class R writes and the class the engine draws the reason line with", {
    expect_true(grepl(paste0(".", tip_disabled_class()), css, fixed = TRUE))
    expect_true(grepl(paste0(".", tip_names()$css, "-reason"), css, fixed = TRUE))
  })

  it("get both attribute names from R, and write none of the engine's own", {
    expect_false(grepl(tip_names()$attribute, js, fixed = TRUE))
    expect_false(grepl(tip_disabled_attribute(), js, fixed = TRUE))
    dependency <- tip_disabled_dependency()
    expect_null(dependency$head)
    config <- dep_config(dependency, "hovercraft-disabled-config")
    expect_identical(config$reason, tip_attribute("reason"))
    expect_identical(config$own, tip_disabled_attribute())
    expect_identical(config$style, tip_disabled_attribute("style"))
  })
})

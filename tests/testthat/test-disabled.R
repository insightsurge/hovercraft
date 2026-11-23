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
    expect_match(attrs(wide)[["style"]], "width:100%", fixed = TRUE)
    full <- tip_on(htmltools::tags$button("Go", class = "btn w-100 d-block"), when_disabled = "Why")
    expect_identical(attrs(full)[["class"]], paste(tip_disabled_class(), "w-100 d-block"))
    plain <- tip_on(htmltools::tags$button("Go", style = "min-width: 4em"), when_disabled = "Why")
    expect_false("style" %in% names(plain$attribs))
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
    config <- jsonlite::fromJSON(sub("^.*Disabled\\.start\\((.*)\\);</script>$", "\\1", dependency$head))
    expect_identical(config$reason, tip_attribute("reason"))
    expect_identical(config$own, tip_disabled_attribute())
  })
})

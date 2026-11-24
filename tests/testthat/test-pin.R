skip_if_not_installed("withr")

button <- function() htmltools::tags$button("Run", id = "run")
attrs <- function(tag) unlist(tag$attribs)

describe("tip_on(pin = )", {
  withr::defer(tip_reset())
  tip_reset()

  it("writes the engine's pin value manual for TRUE, and the classic pin for \"click\"", {
    manual <- tip_on(button(), "Runs", id = "run_tip", pin = TRUE)
    expect_identical(attrs(manual)[[tip_attribute("pin")]], "manual")
    classic <- tip_on(button(), "Runs", id = "run_tip", pin = "click")
    expect_identical(attrs(classic)[[tip_attribute("pin")]], "true")
    expect_false(tip_attribute("pin") %in% names(tip_on(button(), "Runs", id = "run_tip")$attribs))
    expect_false(tip_attribute("pin") %in% names(tip_on(button(), "Runs", id = "run_tip", pin = FALSE)$attribs))
  })

  it("takes the id of the tag when there is no id argument, and needs one", {
    expect_identical(attrs(tip_on(button(), "Runs", pin = TRUE))[[tip_attribute("pin")]], "manual")
    expect_error(tip_on(htmltools::tags$div("Row"), "Runs", pin = TRUE), class = "hovercraft_error_pin")
    expect_error(tip_on(htmltools::tags$div("Row"), "Runs", pin = TRUE), "id")
  })

  it("refuses a value that is not NULL, FALSE, TRUE or \"click\"", {
    for (bad in list("manual", "yes", 1, NA, c(TRUE, FALSE))) {
      expect_error(tip_on(button(), "Runs", id = "r", pin = bad), class = "hovercraft_error_argument")
    }
  })

  it("refuses a tag the engine does not pin, because its own click does something else", {
    expect_error(tip_on(htmltools::tags$a("Go", href = "#", id = "a"), "x", pin = TRUE), class = "hovercraft_error_pin")
    expect_error(tip_on(htmltools::tags$button("Go", id = "a", type = "submit"), "x", pin = TRUE), "cannot pin")
    expect_error(tip_on(htmltools::tags$input(id = "a"), "x", pin = "click"), class = "hovercraft_error_pin")
    expect_error(tip_on(htmltools::tags$div(id = "a", contenteditable = "true"), "x", pin = TRUE), "cannot pin")
    expect_no_error(suppressWarnings(tip_on(htmltools::tags$a("Go", id = "a"), "x", pin = TRUE)))
    expect_no_error(tip_on(htmltools::tags$div(id = "a", contenteditable = "false"), "x", pin = TRUE))
    expect_no_error(tip_on(htmltools::tags$button("Go", id = "a", type = "button"), "x", pin = TRUE))
  })

  it("attaches the pin layer only for a tag that can pin, and replaces an earlier pin", {
    names_of <- function(tag) vapply(htmltools::findDependencies(tag), function(x) x$name, "")
    expect_true("hovercraft-pin" %in% names_of(tip_on(button(), "Runs", pin = TRUE)))
    expect_false("hovercraft-pin" %in% names_of(tip_on(button(), "Runs")))
    again <- tip_on(tip_on(button(), "One", pin = TRUE), "Two", pin = "click")
    expect_identical(sum(names(again$attribs) == tip_attribute("pin")), 1L)
    expect_identical(attrs(again)[[tip_attribute("pin")]], "true")
  })

  it("makes the tagged item announce the pin key, and takes it away with the pin", {
    got <- tip_on(button(), "Runs", id = "run_tip", pin = TRUE)
    expect_identical(attrs(got)[["aria-keyshortcuts"]], toupper(tip_pin_key()))
    expect_identical(attrs(tip_on(button(), "Runs", id = "run_tip", pin = "click"))[["aria-keyshortcuts"]], "P")
    expect_false("aria-keyshortcuts" %in% names(tip_on(button(), "Runs")$attribs))
    off <- tip_on(got, "Runs", pin = FALSE)
    expect_false("aria-keyshortcuts" %in% names(off$attribs))
    # A key shortcut the page wrote itself stays.
    own <- tip_on(htmltools::tags$button("Run", id = "r", `aria-keyshortcuts` = "Alt+R"), "Runs", pin = FALSE)
    expect_identical(attrs(own)[["aria-keyshortcuts"]], "Alt+R")
  })

  it("takes NULL, the default, as no pin, and FALSE as never", {
    expect_identical(tip_check_pin(NULL), NULL)
    expect_false(tip_attribute("pin") %in% names(tip_on(button(), "Runs", pin = NULL)$attribs))
    expect_false(tip_attribute("pin") %in% names(tip_on(button(), "Runs", pin = FALSE)$attribs))
    expect_error(tip_on(button(), "Runs", pin = NA), class = "hovercraft_error_argument")
    expect_identical(formals(tip_on)$pin, NULL)
  })

  it("keeps a pin and its id when the tag is tipped again without a pin argument, and drops both with pin = FALSE", {
    first <- tip_on(button(), "One", id = "run_tip", pin = TRUE)
    again <- tip_on(first, "Two")
    expect_identical(attrs(again)[[tip_attribute("pin")]], "manual")
    expect_identical(attrs(again)[[tip_attribute("id")]], "run_tip")
    expect_identical(attrs(again)[[tip_names()$attribute]], "Two")
    off <- tip_on(first, "Two", pin = FALSE)
    expect_false(tip_attribute("pin") %in% names(off$attribs))
    # With a new id and no pin argument, the new id is the pin's.
    renamed <- tip_on(first, "Two", id = "other_tip")
    expect_identical(attrs(renamed)[[tip_attribute("id")]], "other_tip")
    expect_identical(attrs(renamed)[[tip_attribute("pin")]], "manual")
  })

  it("lets more_on = \"pin\" follow the pin argument", {
    expect_no_error(tip_on(button(), "Short", more = "Long", pin = TRUE))
    expect_error(tip_on(button(), "Short", more = "Long"), class = "hovercraft_error_pin")
  })

  it("warns nothing and writes no pin for a tag with no pin", {
    expect_no_warning(tip_on(button(), "Runs"))
  })
})

describe("the pin layer's files", {
  css <- paste(readLines(system.file("pin", "pin.css", package = "hovercraft")), collapse = "\n")
  js <- paste(readLines(system.file("pin", "pin.js", package = "hovercraft")), collapse = "\n")
  engine <- paste(readLines(system.file("engine", "www", "tip.js", package = "hovercraft")), collapse = "\n")

  it("spell the strip's class and the engine's hint box, which the room for the strip is reserved on", {
    expect_true(grepl(".hovercraft-pin-strip", css, fixed = TRUE))
    expect_true(grepl(paste0(".", tip_names()$css, "[data-hovercraft-pin-room]"), css, fixed = TRUE))
    expect_true(grepl("hovercraft-pin-strip", js, fixed = TRUE))
  })

  it("get every name of the engine from R, and write none of the engine's own", {
    expect_false(grepl(tip_names()$attribute, js, fixed = TRUE))
    dependency <- tip_pin_dependency()
    expect_null(dependency$head)
    config <- jsonlite::fromJSON(
      rawToChar(jsonlite::base64_dec(dependency$meta[["hovercraft-pin-config"]])),
      simplifyVector = TRUE
    )
    expect_identical(config$attributes$pin, tip_attribute("pin"))
    expect_identical(config$attributes$part, tip_contract()$attributes$part)
    expect_identical(config$events$hint, tip_contract()$events$hint$name)
    expect_identical(config$css, tip_names()$css)
    expect_identical(config$key, "p")
  })

  it("rely on the hint box, the pinned box, the Copy and close buttons and the door the engine still has", {
    # After an engine copy: the layer finds the hint by the engine's class and draws over it; these names must remain.
    expect_true(grepl("const pin = el(doc, 'div', `${CSS} ${CSS}-pin`)", engine, fixed = TRUE))
    expect_true(grepl("hint.setAttribute('role', 'tooltip')", engine, fixed = TRUE))
    expect_true(grepl("`${CSS}-copy`", engine, fixed = TRUE))
    expect_true(grepl("`${CSS}-close`", engine, fixed = TRUE))
    expect_true(grepl("pin: (el, opts) =>", engine, fixed = TRUE) || grepl("pin: (el", engine, fixed = TRUE))
  })
})

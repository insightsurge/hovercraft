skip_if_not_installed("withr")
button <- function() htmltools::tags$button("Run")
attrs <- function(tag) unlist(tag$attribs)

describe("tip_on()", {
  withr::defer(tip_reset())
  tip_reset()

  it("writes the text under the engine's attribute and attaches the engine", {
    got <- tip_on(button(), "Runs the **check**")
    expect_identical(attrs(got)[[tip_names()$attribute]], "Runs the **check**")
    expect_identical(htmltools::findDependencies(got)[[1]]$name, "hovercraft")
    expect_match(as.character(got), "Runs the **check**", fixed = TRUE)
  })

  it("writes a template and a record as JSON, one value each", {
    got <- tip_on(button(), template = "diff", record = list(label = "ADAE", n = 3, missing = NA))
    base <- tip_names()$attribute
    expect_identical(attrs(got)[[paste0(base, "-template")]], "diff")
    expect_equal(
      jsonlite::fromJSON(attrs(got)[[paste0(base, "-record")]]),
      list(label = "ADAE", n = 3, missing = NULL)
    )
  })

  it("takes a record as a data frame of one row", {
    got <- tip_on(button(), template = "diff", record = data.frame(label = "ADAE", n = 3))
    expect_match(attrs(got)[[paste0(tip_names()$attribute, "-record")]], "\"label\":\"ADAE\"", fixed = TRUE)
  })

  it("keeps a value from leaving the attribute or the script", {
    got <- tip_on(button(), template = "diff", record = list(label = "\"><script>alert(1)</script>"))
    expect_false(grepl("<script>alert", as.character(got), fixed = TRUE))
  })

  it("writes all three attributes when both a text and a template are given", {
    got <- tip_on(button(), "Text", template = "diff", record = list(label = "x"))
    expect_length(grep(tip_names()$attribute, names(got$attribs), fixed = TRUE), 3L)
  })

  it("replaces the tip of a tag that has one", {
    got <- tip_on(tip_on(button(), "first"), "second")
    expect_identical(attrs(got)[[tip_names()$attribute]], "second")
    expect_length(got$attribs[names(got$attribs) == tip_names()$attribute], 1L)
  })

  it("treats NA and an empty string as no text", {
    expect_error(tip_on(button(), NA_character_), class = "hovercraft_error_tip")
    expect_error(tip_on(button(), ""), class = "hovercraft_error_tip")
    expect_error(tip_on(button()), class = "hovercraft_error_tip")
  })

  it("keeps a tag's other attributes", {
    got <- tip_on(htmltools::tags$button("Run", id = "go", class = "btn"), "Go")
    expect_identical(htmltools::tagGetAttribute(got, "id"), "go")
    expect_identical(htmltools::tagGetAttribute(got, "class"), "btn")
  })

  it("needs one tag", {
    expect_error(tip_on(htmltools::tagList(button(), button()), "x"), class = "hovercraft_error_tag")
    expect_error(tip_on("not a tag", "x"), class = "hovercraft_error_tag")
  })

  it("checks its arguments and takes nothing else", {
    expect_error(tip_on(button(), 1), "text")
    expect_error(tip_on(button(), template = 1), "template")
    expect_error(tip_on(button(), "x", extra = 1), "must be empty")
  })

  it("warns when a record has no template to fill", {
    expect_warning(tip_on(button(), "x", record = list(a = 1)), class = "hovercraft_warning_record")
  })

  it("refuses a record with a value a tip cannot show", {
    expect_error(tip_on(button(), template = "diff", record = list(a = list(1))), class = "hovercraft_error_record")
  })
})

describe("tip_on() against the templates defined in this session", {
  withr::defer(tip_reset())
  tip_reset()
  tip_define(diff = tip_template(tip_title("{{ label }}"), tip_text("{{ n }} of {{ tip_id }}")))

  it("stops when the record lacks a field the template names", {
    expect_error(
      tip_on(button(), template = "diff", record = list(label = "x")),
      class = "hovercraft_error_field"
    )
    expect_error(tip_on(button(), template = "diff"), class = "hovercraft_error_field")
  })

  it("knows tip_id when the tag has an id, and says to give it one when not", {
    with_id <- htmltools::tags$button("Run", id = "go")
    expect_no_error(tip_on(with_id, template = "diff", record = list(label = "x", n = 1)))
    expect_error(
      tip_on(button(), template = "diff", record = list(label = "x", n = 1)),
      "id",
      class = "hovercraft_error_field"
    )
    expect_no_error(tip_on(button(), template = "diff", record = list(label = "x", n = 1, tip_id = "mine")))
  })

  it("refuses tip_index: there is no list here to count in", {
    tip_define(counted = tip_template(tip_text("{{ tip_index }}")))
    expect_error(tip_on(button(), template = "counted", record = list()), "later", class = "hovercraft_error_field")
    expect_no_error(tip_on(button(), template = "counted", record = list(tip_index = 3)))
  })

  it("warns when a column is named like an engine field", {
    expect_warning(
      tip_on(button(), template = "diff", record = list(label = "x", n = 1, tip_id = "9")),
      class = "hovercraft_warning_engine_name"
    )
  })

  it("keeps only the columns the template names, and ignores a bad value in another", {
    got <- tip_on(
      htmltools::tags$button("Run", id = "go"),
      template = "diff",
      record = list(label = "x", n = 1, secret = "hidden", other = list(1, 2))
    )
    record <- jsonlite::fromJSON(attrs(got)[[paste0(tip_names()$attribute, "-record")]])
    expect_named(record, c("label", "n"))
    expect_no_match(as.character(got), "hidden")
  })

  it("keeps every column for a template it has not seen, and refuses a bad value in any of them", {
    got <- tip_on(button(), template = "elsewhere", record = list(a = 1, secret = "kept"))
    expect_match(as.character(got), "secret", fixed = TRUE)
    expect_error(
      tip_on(button(), template = "elsewhere", record = list(a = list(1))),
      class = "hovercraft_error_record"
    )
  })

  it("warns once about a name not defined yet, and does not stop: the UI may define it later", {
    rlang::reset_warning_verbosity("hovercraft-template-dif")
    expect_warning(
      tip_on(button(), template = "dif", record = list(label = "x")),
      class = "hovercraft_warning_template"
    )
    expect_no_warning(tip_on(button(), template = "dif", record = list(label = "x")))
  })
})

describe("tip_on() when no template has been defined", {
  withr::defer(tip_reset())
  tip_reset()

  it("leaves a template name to the page", {
    expect_no_warning(tip_on(button(), template = "later", record = list(a = 1)))
  })
})

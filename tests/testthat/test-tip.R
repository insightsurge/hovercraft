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

  it("returns the tag as it was for an NA or empty text, which is how a table row with no note should look", {
    expect_identical(tip_on(button(), NA_character_), button())
    expect_identical(tip_on(button(), ""), button())
    expect_identical(tip_on(button(), NA_character_, record = list(a = 1)), button())
  })

  it("uses the template when the text is missing", {
    got <- tip_on(button(), NA_character_, template = "diff", record = list(a = 1))
    expect_identical(attrs(got)[[paste0(tip_names()$attribute, "-template")]], "diff")
    expect_false(tip_names()$attribute %in% names(got$attribs))
  })

  it("stops only when neither a text nor a template is given", {
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

  it("checks its arguments with errors of its own class, and takes nothing else", {
    expect_error(tip_on(button(), 1), "text", class = "hovercraft_error_argument")
    expect_error(tip_on(button(), template = 1), "template", class = "hovercraft_error_argument")
    expect_error(tip_on(button(), "x", extra = 1), "must be empty", class = "hovercraft_error_argument")
  })

  it("warns, once, about a tag that cannot take focus: keyboard users would never reach its tip", {
    rlang::reset_warning_verbosity("hovercraft-focus")
    expect_warning(tip_on(htmltools::tags$tr("x"), "Row"), class = "hovercraft_warning_focus")
    expect_no_warning(tip_on(htmltools::tags$tr("x"), "Row"))
  })

  it("does not warn about a tag that can take focus, and never adds a tabindex itself", {
    rlang::reset_warning_verbosity("hovercraft-focus")
    focusable <- list(
      htmltools::tags$button("b"),
      htmltools::tags$a("l", href = "#"),
      htmltools::tags$input(type = "text"),
      htmltools::tags$select(),
      htmltools::tags$textarea(),
      htmltools::tags$summary("s"),
      htmltools::tags$tr("r", tabindex = "0"),
      htmltools::tags$div("d", tabindex = "-1")
    )
    for (tag in focusable) {
      expect_no_warning(got <- tip_on(tag, "Tip"))
      expect_identical(htmltools::tagGetAttribute(got, "tabindex"), htmltools::tagGetAttribute(tag, "tabindex"))
    }
  })

  it("warns about an anchor with no href and a hidden input, which cannot take focus", {
    rlang::reset_warning_verbosity("hovercraft-focus")
    expect_warning(tip_on(htmltools::tags$a("l"), "Tip"), class = "hovercraft_warning_focus")
    rlang::reset_warning_verbosity("hovercraft-focus")
    expect_warning(tip_on(htmltools::tags$input(type = "hidden"), "Tip"), class = "hovercraft_warning_focus")
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
    expect_no_error(suppressWarnings(
      tip_on(button(), template = "diff", record = list(label = "x", n = 1, tip_id = "mine"))
    ))
  })

  it("refuses tip_index: there is no list here to count in", {
    tip_define(counted = tip_template(tip_text("{{ tip_index }}")))
    expect_error(tip_on(button(), template = "counted", record = list()), "later", class = "hovercraft_error_field")
    expect_no_error(suppressWarnings(tip_on(button(), template = "counted", record = list(tip_index = 3))))
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
    got <- suppressWarnings(tip_on(button(), template = "elsewhere", record = list(a = 1, secret = "kept")))
    expect_match(as.character(got), "secret", fixed = TRUE)
    expect_error(
      suppressWarnings(tip_on(button(), template = "elsewhere", record = list(a = list(1)))),
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

describe("tip_define() again", {
  withr::defer(tip_reset())
  tip_reset()

  it("is quiet when the template is the same, and warns when the same name now holds a different one", {
    tip_define(demo = tip_template(tip_text("one")))
    expect_no_warning(tip_define(demo = tip_template(tip_text("one"))))
    expect_warning(tip_define(demo = tip_template(tip_text("two"))), class = "hovercraft_warning_template")
    expect_identical(tip_known()$demo$fields, character())
  })

  it("says to put a module's id in its template names", {
    tip_define(modal = tip_template(tip_text("a")))
    expect_warning(tip_define(modal = tip_template(tip_text("b"))), "module")
  })
})

describe("tip_on() when no template has been defined", {
  withr::defer(tip_reset())
  tip_reset()

  it("leaves a template name to the page", {
    expect_no_warning(tip_on(button(), template = "later", record = list(a = 1)))
  })
})

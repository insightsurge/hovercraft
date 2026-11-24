skip_if_not_installed("withr")
button <- function() htmltools::tags$button("Run")
attrs <- function(tag) unlist(tag$attribs)

describe("tip_on()", {
  withr::defer(tip_reset())
  tip_reset()

  it("has no `kind`, and takes NULL for `marker` and `pin` by default", {
    expect_false("kind" %in% names(formals(tip_on)))
    expect_error(tip_on(button(), "Runs", kind = "review_item"), class = "hovercraft_error_argument")
    expect_null(formals(tip_on)$marker)
    expect_null(formals(tip_on)$pin)
    expect_false(tip_attribute("kind") %in% names(tip_on(button(), "Runs")$attribs))
    expect_identical(
      names(tip_on(button(), "Runs", marker = NULL)$attribs),
      names(tip_on(button(), "Runs", marker = "none")$attribs)
    )
  })

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

  it("looks inside the tag for something focusable: a Shiny input's outer div is fine", {
    rlang::reset_warning_verbosity("hovercraft-focus")
    wrapped <- htmltools::tags$div(
      class = "form-group",
      htmltools::tags$label("Name"),
      htmltools::tagList(htmltools::tags$span("x"), list(htmltools::tags$input(type = "text")))
    )
    expect_no_warning(tip_on(wrapped, "Tip"))
    expect_no_warning(tip_on(htmltools::tags$div(htmltools::tags$span(htmltools::tags$a(href = "#", "go"))), "Tip"))
    expect_no_warning(tip_on(htmltools::tags$tr(htmltools::tags$td(htmltools::tags$button("b"))), "Tip"))
    expect_null(htmltools::tagGetAttribute(tip_on(wrapped, "Tip"), "tabindex"))
  })

  it("still warns when nothing inside the tag can take focus", {
    rlang::reset_warning_verbosity("hovercraft-focus")
    nothing <- htmltools::tags$div(
      "text",
      htmltools::HTML("<b>raw</b>"),
      htmltools::tags$span(htmltools::tags$input(type = "hidden"), htmltools::tags$a("no href"))
    )
    expect_warning(tip_on(nothing, "Tip"), class = "hovercraft_warning_focus")
  })

  it("warns about an anchor with no href and a hidden input, which cannot take focus", {
    rlang::reset_warning_verbosity("hovercraft-focus")
    expect_warning(tip_on(htmltools::tags$a("l"), "Tip"), class = "hovercraft_warning_focus")
    rlang::reset_warning_verbosity("hovercraft-focus")
    expect_warning(tip_on(htmltools::tags$input(type = "hidden"), "Tip"), class = "hovercraft_warning_focus")
  })

  it("labels an icon-only button from a plain-text tip, with the markdown marks taken out", {
    icon <- htmltools::tags$i(class = "icon-gear", `aria-hidden` = "true")
    got <- tip_on(htmltools::tags$button(icon), "Open the **settings**, see [help](https://example.com)")
    expect_identical(htmltools::tagGetAttribute(got, "aria-label"), "Open the settings, see help")
    expect_identical(
      htmltools::tagGetAttribute(got, tip_names()$attribute),
      "Open the **settings**, see [help](https://example.com)"
    )
    expect_identical(htmltools::tagGetAttribute(tip_on(htmltools::tags$button(), "Empty"), "aria-label"), "Empty")
    expect_identical(
      htmltools::tagGetAttribute(
        tip_on(htmltools::tags$span(role = "button", tabindex = "0", icon), "A\n\n- `one`\n- _two_ x"),
        "aria-label"
      ),
      "A one two x"
    )
    expect_identical(
      htmltools::tagGetAttribute(tip_on(htmltools::tags$button(htmltools::HTML("<svg></svg>")), "Svg"), "aria-label"),
      "Svg"
    )
  })

  it("leaves a button that has a name, or a tag that is not a button, as it is", {
    icon <- htmltools::tags$i(class = "icon-gear")
    named <- list(
      htmltools::tags$button("Run"),
      htmltools::tags$button(icon, " ", htmltools::tags$span("Settings")),
      htmltools::tags$button(htmltools::HTML("<i></i> Words")),
      htmltools::tags$button(htmltools::tags$img(src = "x.png", alt = "Settings")),
      htmltools::tags$button(icon, `aria-label` = "Own label"),
      htmltools::tags$button(icon, `aria-labelledby` = "other")
    )
    for (tag in named) {
      expect_identical(
        htmltools::tagGetAttribute(tip_on(tag, "Tip"), "aria-label"),
        htmltools::tagGetAttribute(tag, "aria-label")
      )
    }
    expect_identical(htmltools::tagGetAttribute(tip_on(named[[5]], "Tip"), "aria-label"), "Own label")
    expect_null(htmltools::tagGetAttribute(tip_on(htmltools::tags$a(icon, href = "#"), "Tip"), "aria-label"))
  })

  it("takes the real Shiny markup: no focus warning on an input, a label for an icon-only action button", {
    skip_if_not_installed("shiny")
    rlang::reset_warning_verbosity("hovercraft-focus")
    expect_no_warning(tip_on(shiny::textInput("name", "Name"), "Tip"))
    expect_no_warning(tip_on(shiny::checkboxInput("flag", "Flag"), "Tip"))
    expect_no_warning(tip_on(shiny::selectInput("pick", "Pick", c("a", "b")), "Tip"))
    got <- tip_on(shiny::actionButton("go", NULL, icon = shiny::icon("gear")), "Settings")
    expect_identical(htmltools::tagGetAttribute(got, "aria-label"), "Settings")
    expect_null(htmltools::tagGetAttribute(tip_on(shiny::actionButton("go", "Go"), "Settings"), "aria-label"))
  })

  it("replaces the label of an earlier tip, but keeps one the page wrote", {
    icon <- htmltools::tags$i(class = "icon-gear")
    once <- tip_on(htmltools::tags$button(icon), "First")
    twice <- tip_on(once, "Second **one**")
    expect_identical(htmltools::tagGetAttribute(twice, "aria-label"), "Second one")
    own <- tip_on(htmltools::tags$button(icon, `aria-label` = "Mine"), "First")
    expect_identical(htmltools::tagGetAttribute(tip_on(own, "Second"), "aria-label"), "Mine")
  })

  it("counts a number in a button as visible words, and nothing else as none", {
    expect_null(htmltools::tagGetAttribute(tip_on(htmltools::tags$button(5), "Tip"), "aria-label"))
    expect_false(tip_has_words(NULL))
    expect_false(tip_has_words(list()))
  })

  it("gives a template tip no label, because its words depend on the record", {
    tip_define(lab = tip_template(tip_text("{{ x }}")))
    got <- tip_on(htmltools::tags$button(htmltools::tags$i()), template = "lab", record = list(x = 1))
    expect_null(htmltools::tagGetAttribute(got, "aria-label"))
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

describe("tip_on(placement =, id =)", {
  withr::defer(tip_reset())
  tip_reset()
  attribute <- function(key) tip_attribute(key)

  it("writes a placement and an id as the engine's attributes, and nothing when they are NULL", {
    got <- tip_on(button(), "Runs", placement = "left", id = "mod1-run_tip")
    expect_identical(htmltools::tagGetAttribute(got, attribute("placement")), "left")
    expect_identical(htmltools::tagGetAttribute(got, attribute("id")), "mod1-run_tip")
    plain <- tip_on(button(), "Runs")
    expect_null(htmltools::tagGetAttribute(plain, attribute("placement")))
    expect_null(htmltools::tagGetAttribute(plain, attribute("id")))
  })

  it("lets \"auto\" be written, so a tag can break from a side set above it", {
    got <- tip_on(button(), "Runs", placement = "auto")
    expect_identical(htmltools::tagGetAttribute(got, attribute("placement")), "auto")
  })

  it("keeps the tag's own HTML id apart from the tip's id", {
    got <- tip_on(htmltools::tags$button("Run", id = "go"), "Runs", id = "run_tip")
    expect_identical(htmltools::tagGetAttribute(got, "id"), "go")
    expect_identical(htmltools::tagGetAttribute(got, attribute("id")), "run_tip")
  })

  it("replaces them when tip_on() is called again on the same tag", {
    once <- tip_on(button(), "Runs", placement = "left", id = "first")
    rlang::reset_warning_verbosity("hovercraft-retip")
    expect_warning(twice <- tip_on(once, "Runs again"), class = "hovercraft_warning_retip")
    expect_null(htmltools::tagGetAttribute(twice, attribute("placement")))
    expect_null(htmltools::tagGetAttribute(twice, attribute("id")))
  })

  it("stops on a side the engine does not know and on an id that is not one", {
    expect_error(tip_on(button(), "Runs", placement = "above"), class = "hovercraft_error_argument")
    expect_error(tip_on(button(), "Runs", placement = c("left", "right")), class = "hovercraft_error_argument")
    expect_error(tip_on(button(), "Runs", id = "two words"), class = "hovercraft_error_argument")
    expect_error(tip_on(button(), "Runs", id = ""), class = "hovercraft_error_argument")
    expect_error(tip_on(button(), "Runs", id = 1), class = "hovercraft_error_argument")
  })
})

describe("tip_on(more =)", {
  withr::defer(tip_reset())
  tip_reset()
  pinnable <- function(id = NULL) htmltools::tags$button("Run", id = id)
  held <- function() tip_more_attribute("held")
  held_template <- function() tip_more_attribute("held-template")
  deps_of <- function(tag) vapply(htmltools::findDependencies(tag), function(d) d$name, "")

  it("keeps the long form behind a button by default: attributes of its own, and none of the engine's", {
    got <- tip_on(button(), "Short", more = "Extra **words**")
    expect_identical(htmltools::tagGetAttribute(got, held()), "Extra **words**")
    expect_null(htmltools::tagGetAttribute(got, tip_attribute("more")))
    expect_null(htmltools::tagGetAttribute(got, tip_attribute("more_on")))
    expect_null(htmltools::tagGetAttribute(got, tip_attribute("more_template")))
    expect_identical(htmltools::tagGetAttribute(got, "aria-keyshortcuts"), "M")
    expect_true("hovercraft-more" %in% deps_of(got))
    # No pin and no id are needed, and the same with tip_more() and its default.
    same <- tip_on(button(), "Short", more = tip_more("Extra **words**"))
    expect_identical(as.character(same), as.character(got))
    expect_identical(
      as.character(tip_on(button(), "Short", more = tip_more("Extra **words**", on = "button"))),
      as.character(got)
    )
  })

  it("writes the engine's attributes for on = \"hover\", with no button and no key", {
    got <- tip_on(button(), "Short", more = tip_more("Extra **words**", on = "hover"))
    expect_identical(htmltools::tagGetAttribute(got, tip_attribute("more")), "Extra **words**")
    expect_identical(htmltools::tagGetAttribute(got, tip_attribute("more_on")), "hover")
    expect_null(htmltools::tagGetAttribute(got, held()))
    expect_null(htmltools::tagGetAttribute(got, "aria-keyshortcuts"))
    expect_false("hovercraft-more" %in% deps_of(got))
  })

  it("writes the engine's attributes for on = \"pin\", and announces both keys when it can also pin", {
    got <- tip_on(pinnable("run"), "Short", id = "run", pin = TRUE, more = tip_more("Long", on = "pin"))
    expect_identical(htmltools::tagGetAttribute(got, tip_attribute("more")), "Long")
    expect_identical(htmltools::tagGetAttribute(got, tip_attribute("more_on")), "pin")
    expect_identical(htmltools::tagGetAttribute(got, "aria-keyshortcuts"), "P")
    both <- tip_on(pinnable("run"), "Short", id = "run", pin = TRUE, more = "Long")
    expect_identical(htmltools::tagGetAttribute(both, "aria-keyshortcuts"), "P M")
  })

  it("holds the words of the buttons as data, one string or both", {
    one <- tip_on(button(), "Short", more = tip_more("Long", label = "Details"))
    expect_identical(htmltools::tagGetAttribute(one, tip_more_attribute("label")), "{\"more\":\"Details\"}")
    two <- tip_on(button(), "Short", more = tip_more("Long", label = c(more = "Details", less = "Fewer")))
    expect_identical(
      htmltools::tagGetAttribute(two, tip_more_attribute("label")),
      "{\"more\":\"Details\",\"less\":\"Fewer\"}"
    )
    expect_null(htmltools::tagGetAttribute(tip_on(button(), "Short", more = "Long"), tip_more_attribute("label")))
  })

  it("accepts a tip_template() or placeholder text for more and attaches a deterministic template dependency", {
    got_a <- tip_on(
      pinnable("run_a"),
      "Short",
      pin = TRUE,
      more = "Details for {{ label }} ({{ n | number:0 }})",
      record = list(label = "ADAE", n = 12, unused = "drop")
    )
    got_b <- tip_on(
      pinnable("run_a"),
      "Short",
      pin = TRUE,
      more = "Details for {{ label }} ({{ n | number:0 }})",
      record = list(label = "ADAE", n = 12, unused = "drop")
    )
    expect_identical(as.character(got_a), as.character(got_b))
    expect_match(htmltools::tagGetAttribute(got_a, held_template()), "^more_[a-f0-9]{12}$")
    rec <- jsonlite::fromJSON(htmltools::tagGetAttribute(got_a, tip_attribute("record")))
    expect_named(rec, c("label", "n"))

    tpl_tag <- tip_on(
      pinnable(),
      "Short",
      pin = TRUE,
      more = tip_template(tip_fields(Owner = "{{ owner }}")),
      record = list(owner = "Biostats"),
      id = "run_tpl"
    )
    expect_match(htmltools::tagGetAttribute(tpl_tag, held_template()), "^more_[a-f0-9]{12}$")
    expect_true(any(startsWith(deps_of(tpl_tag), "hovercraft-more-")))
    # On the engine's own moments the template is the engine's attribute.
    hover <- tip_on(
      pinnable(),
      "Short",
      more = tip_more(tip_template(tip_fields(Owner = "{{ owner }}")), on = "hover"),
      record = list(owner = "Biostats")
    )
    expect_match(htmltools::tagGetAttribute(hover, tip_attribute("more_template")), "^more_[a-f0-9]{12}$")
  })

  it("parks a Shiny output that a more template holds, with the park markup in the template's dependency", {
    skip_if_not_installed("shiny")
    tag <- tip_on(
      pinnable(),
      "Short",
      pin = TRUE,
      more = tip_template(tip_output(shiny::plotOutput("mod1-more_plot"))),
      id = "run_out"
    )
    dep <- Filter(function(d) startsWith(d$name, "hovercraft-more-"), htmltools::findDependencies(tag))[[1]]
    # The runtime script reads the operations from a <meta>, as data: the output is parked, and its tag goes to the script as
    # a string that it puts in the body, since a `<div>` written into a head would end the head.
    expect_null(dep$head)
    ops <- dep_ops(dep)
    expect_match(ops, "{\"op\":\"park\",\"id\":\"mod1-more_plot\"}", fixed = TRUE)
    expect_match(ops, "\"op\":\"parkHtml\"", fixed = TRUE)
    expect_match(ops, "id=\\\"mod1-more_plot\\\"", fixed = TRUE)
    # Nothing in it can end the block that holds it.
    expect_no_match(ops, "</", fixed = TRUE)
  })

  it("treats NA and empty more as no long form and clears earlier more attributes on replacement", {
    for (none in list("", NA_character_, tip_more(""), tip_more(NA))) {
      got <- tip_on(button(), "Short", more = none)
      expect_null(htmltools::tagGetAttribute(got, held()))
      expect_null(htmltools::tagGetAttribute(got, "aria-keyshortcuts"))
      expect_false("hovercraft-more" %in% deps_of(got))
    }
    once <- tip_on(button(), "Short", more = tip_more("Extra", on = "hover"))
    twice <- tip_on(once, "Only short")
    expect_null(htmltools::tagGetAttribute(twice, tip_attribute("more")))
    expect_null(htmltools::tagGetAttribute(twice, tip_attribute("more_on")))
    behind <- tip_on(button(), "Short", more = tip_more("Extra", label = "Details"))
    again <- tip_on(behind, "Only short")
    expect_null(htmltools::tagGetAttribute(again, held()))
    expect_null(htmltools::tagGetAttribute(again, tip_more_attribute("label")))
    expect_null(htmltools::tagGetAttribute(again, "aria-keyshortcuts"))
    expect_false("hovercraft-more" %in% deps_of(again))
  })

  it("keeps a key shortcut the page wrote itself when the tip has no pin and no button", {
    own <- htmltools::tags$button("Run", `aria-keyshortcuts` = "Alt+R")
    expect_identical(htmltools::tagGetAttribute(tip_on(own, "Short"), "aria-keyshortcuts"), "Alt+R")
    expect_identical(
      htmltools::tagGetAttribute(tip_on(own, "Short", more = tip_more("Long", on = "hover")), "aria-keyshortcuts"),
      "Alt+R"
    )
  })

  it("stops with a classed error when on is pin without a pinnable tag or id", {
    expect_error(tip_on(button(), "Short", more = tip_more("Long", on = "pin")), class = "hovercraft_error_pin")
    expect_error(
      tip_on(button(), "Short", id = "x", more = tip_more("Long", on = "pin")),
      class = "hovercraft_error_pin"
    )
    expect_error(
      tip_on(pinnable(), "Short", pin = TRUE, more = tip_more("Long", on = "pin")),
      class = "hovercraft_error_pin"
    )
    expect_no_error(tip_on(pinnable(), "Short", id = "x", pin = TRUE, more = tip_more("Long", on = "pin")))
    expect_no_error(tip_on(pinnable("x"), "Short", pin = TRUE, more = tip_more("Long", on = "pin")))
    # The default needs neither a pin nor an id.
    expect_no_error(tip_on(button(), "Short", more = "Long"))
  })

  it("validates more, on, label and placeholders in more", {
    expect_error(tip_on(button(), "Short", more = 1), class = "hovercraft_error_argument")
    expect_error(tip_on(button(), "Short", more = "{{ bad"), class = "hovercraft_error_placeholder")
    expect_error(
      tip_on(button(), "Short", more = "{{ missing }}", record = list(other = 1)),
      class = "hovercraft_error_field"
    )
    expect_error(
      tip_on(button(), "Short", more = "{{ tip_index }}", record = list()),
      class = "hovercraft_error_field"
    )
    expect_error(tip_on(button(), "Short", more = "{{ tip_id }}", record = list()), class = "hovercraft_error_field")
    # more_on is gone, with no alias.
    expect_error(tip_on(button(), "Short", more = "Long", more_on = "hover"), class = "hovercraft_error_argument")
  })
})

describe("tip_more()", {
  it("holds the content, the moment and the words of the buttons", {
    got <- tip_more("Long", on = "hover")
    expect_s3_class(got, "hovercraft_more")
    expect_identical(got$content, "Long")
    expect_identical(got$on, "hover")
    expect_null(got$label)
    expect_null(tip_more("Long")$on)
    expect_identical(tip_more("Long", label = "Details")$label, c(more = "Details"))
    expect_identical(tip_more("Long", on = "button", label = c(less = "Fewer"))$label, c(less = "Fewer"))
    expect_s3_class(tip_more(tip_template(tip_text("Long"))), "hovercraft_more")
    expect_s3_class(tip_more(NA), "hovercraft_more")
  })

  it("stops on a content, a moment or a label that is not one", {
    expect_error(tip_more(1), class = "hovercraft_error_argument")
    expect_error(tip_more(c("a", "b")), class = "hovercraft_error_argument")
    expect_error(tip_more("Long", on = "click"), class = "hovercraft_error_argument")
    expect_error(tip_more("Long", on = c("hover", "pin")), class = "hovercraft_error_argument")
    expect_error(tip_more("Long", label = 1), class = "hovercraft_error_argument")
    expect_error(tip_more("Long", label = ""), class = "hovercraft_error_argument")
    expect_error(tip_more("Long", label = c(more = "a", other = "b")), class = "hovercraft_error_argument")
    expect_error(tip_more("Long", label = c(more = "a", more = "b")), class = "hovercraft_error_argument")
    expect_error(tip_more("Long", label = c("a", "b")), class = "hovercraft_error_argument")
    expect_error(tip_more("Long", labell = "a"), class = "hovercraft_error_argument")
  })

  it("takes a label only with the button", {
    expect_error(tip_more("Long", on = "hover", label = "Details"), "button", class = "hovercraft_error_argument")
    expect_error(tip_more("Long", on = "pin", label = c(more = "a")), class = "hovercraft_error_argument")
    expect_no_error(tip_more("Long", on = "button", label = "Details"))
  })
})

describe("tip_on() rules", {
  withr::defer(tip_reset())
  tip_reset()

  it("reports its errors from tip_on()", {
    call_of <- function(code) tryCatch(code, error = function(e) rlang::call_name(e$call))
    expect_identical(call_of(tip_on("x", "y")), "tip_on")
    expect_identical(
      call_of(tip_on(button(), "x", style = tip_style(theme = list("--hovercraft-tip-radius" = "9px")))),
      "tip_on"
    )
    expect_identical(call_of(tip_on(button(), "x", id = "a", pin = "click")), "tip_on")
    expect_identical(call_of(tip_on(button(), "x", events = "pin")), "tip_on")
  })

  it("takes one tag, and says how to tip a list, text or a tab's title", {
    for (bad in list(htmltools::tagList(button(), button()), list(button()), "Data", NULL)) {
      expect_error(tip_on(bad, "x"), class = "hovercraft_error_tag")
    }
    expect_error(tip_on("Data", "x"), "nav_panel")
    expect_error(tip_on("Data", "x"), "value")
    expect_error(tip_on(htmltools::tagList(button()), "x"), "list of tags")
  })

  it("keeps a title attribute and warns, once, with a class of its own", {
    rlang::reset_warning_verbosity("hovercraft-title")
    titled <- htmltools::tags$button("Run", title = "Native")
    expect_warning(got <- tip_on(titled, "Runs"), class = "hovercraft_warning_title")
    expect_identical(htmltools::tagGetAttribute(got, "title"), "Native")
    expect_warning(tip_on(titled, "Runs"), regexp = NA)
    rlang::reset_warning_verbosity("hovercraft-title")
    expect_warning(tip_on(titled, "Runs"), "title")
    expect_no_warning(tip_on(button(), "Runs"))
  })

  it("lets a wrapper silence the warning for a tip it made on purpose", {
    first <- tip_on(button(), "One", id = "run_tip")
    rlang::reset_warning_verbosity("hovercraft-retip")
    expect_no_warning(suppressWarnings(tip_on(first, "Two"), classes = "hovercraft_warning_retip"))
  })

  it("stops pin = \"click\" on a control or a tag that holds one, and says which pin to use", {
    expect_error(tip_on(button(), "x", id = "a", pin = "click"), class = "hovercraft_error_pin")
    expect_error(tip_on(button(), "x", id = "a", pin = "click"), "pin = TRUE")
    around <- htmltools::tags$div(id = "w", htmltools::tags$input(id = "i"))
    expect_error(tip_on(around, "x", pin = "click"), "holds a control")
    expect_error(tip_on(around, "x", pin = "click"), "pin = TRUE")
    expect_no_error(suppressWarnings(tip_on(around, "x", pin = TRUE)))
    link <- htmltools::tags$div(id = "l", htmltools::tags$a("Go", href = "#"))
    expect_error(tip_on(link, "x", pin = "click"), "holds a control")
    expect_no_error(suppressWarnings(tip_on(link, "x", pin = TRUE)))
    # Where no pin works at all, the message says that and says what to wrap it in.
    expect_error(tip_on(htmltools::tags$input(id = "a"), "x", pin = "click"), "cannot pin")
    expect_error(tip_on(htmltools::tags$input(id = "a"), "x", pin = "click"), "pin = TRUE")
    expect_error(tip_on(htmltools::tags$a("Go", href = "#", id = "a"), "x", pin = "click"), "cannot pin")
    # A tag with no control in it takes the click pin.
    expect_no_error(tip_on(htmltools::tags$span("Go", id = "s", tabindex = "0"), "x", pin = "click"))
  })

  it("stops a list of CSS variables in the style of one tag, and takes it for the page", {
    css <- tip_style(theme = list("--hovercraft-tip-radius" = "9px"))
    expect_error(tip_on(button(), "x", style = css), class = "hovercraft_error_style")
    expect_error(tip_on(button(), "x", style = css), "use_hovercraft")
    expect_no_error(use_hovercraft(style = css))
    expect_no_error(tip_on(button(), "x", style = tip_style(theme = list(radius = 9))))
  })

  it("takes a tip_template() as the template, and checks its fields against the record", {
    tpl <- tip_template(tip_title("{{ label }}"), tip_text("n is {{ n }}"))
    got <- tip_on(button(), template = tpl, record = list(label = "ADAE", n = 3, extra = "dropped"))
    name <- htmltools::tagGetAttribute(got, tip_attribute("template"))
    expect_match(name, "^inline_[a-f0-9]{12}$")
    expect_null(htmltools::tagGetAttribute(got, tip_names()$attribute))
    expect_named(jsonlite::fromJSON(htmltools::tagGetAttribute(got, tip_attribute("record"))), c("label", "n"))
    expect_true(any(vapply(htmltools::findDependencies(got), function(d) grepl("^hovercraft-inline-", d$name), NA)))
    same <- tip_on(button(), template = tpl, record = list(label = "ADAE", n = 3))
    expect_identical(htmltools::tagGetAttribute(same, tip_attribute("template")), name)
    expect_error(
      tip_on(button(), template = tpl, record = list(label = "ADAE")),
      class = "hovercraft_error_field"
    )
    expect_error(tip_on(button(), template = tpl, record = list(label = "ADAE")), "template")
    expect_error(tip_on(button(), template = 1), class = "hovercraft_error_argument")
  })

  it("fills {{ field }} in the text when a record is given, and leaves braces alone without one", {
    filled <- tip_on(button(), "Hello {{ name }}", record = list(name = "Ada", other = "x"))
    expect_match(htmltools::tagGetAttribute(filled, tip_attribute("template")), "^inline_[a-f0-9]{12}$")
    expect_null(htmltools::tagGetAttribute(filled, tip_names()$attribute))
    expect_named(jsonlite::fromJSON(htmltools::tagGetAttribute(filled, tip_attribute("record"))), "name")
    expect_null(htmltools::tagGetAttribute(filled, "aria-label"))
    plain <- tip_on(button(), "Hello {{ name }}")
    expect_identical(htmltools::tagGetAttribute(plain, tip_names()$attribute), "Hello {{ name }}")
    expect_null(htmltools::tagGetAttribute(plain, tip_attribute("template")))
    expect_error(tip_on(button(), "Hello {{ name }}", record = list(other = 1)), class = "hovercraft_error_field")
    expect_error(tip_on(button(), "Hello {{ name", record = list(name = "Ada")), class = "hovercraft_error_placeholder")
    expect_error(tip_on(button(), "Hello {{ name", record = list(name = "Ada")), "The words of `text`", fixed = TRUE)
    # With a named template and a record, the text is the tip and its placeholders are filled too.
    both <- tip_on(button(), "Hi {{ name }}", template = "card", record = list(name = "Ada"))
    expect_match(htmltools::tagGetAttribute(both, tip_attribute("template")), "^inline_")
    # Text with no placeholder has nothing to fill: the record is ignored, as before.
    expect_warning(tip_on(button(), "Hello", record = list(name = "Ada")), class = "hovercraft_warning_record")
  })
})

describe("tip_on() replacing a tip", {
  withr::defer(tip_reset())
  tip_reset()
  dep_names <- function(tag) vapply(htmltools::findDependencies(tag), function(d) d$name, "")

  it("takes the old tip off when the new text is empty", {
    tipped <- tip_on(button(), "One", id = "a", pin = TRUE, events = "pin", marker = "underline")
    for (empty in list(NA_character_, "")) {
      got <- tip_on(tipped, empty)
      expect_false(any(startsWith(names(got$attribs), tip_names()$attribute)))
      expect_false(tip_shiny_attribute() %in% names(got$attribs))
      expect_false("aria-keyshortcuts" %in% names(got$attribs))
      expect_false(any(startsWith(dep_names(got), "hovercraft")))
    }
    wrapped <- tip_on(htmltools::tags$button("Run"), "One", when_disabled = "why")
    got <- tip_on(wrapped, NA)
    expect_identical(got$name, "button")
    expect_null(htmltools::tagGetAttribute(got, tip_names()$attribute))
    expect_null(htmltools::tagGetAttribute(got, tip_disabled_attribute()))
    # A tag that had no tip comes back as it was, and no text at all is still an error.
    expect_identical(tip_on(button(), NA), button())
    expect_error(tip_on(button(), NULL), class = "hovercraft_error_tip")
  })

  it("takes the old tip's dependencies off with it, and keeps the page's own", {
    other <- htmltools::htmlDependency("other", "1.0", src = tempdir())
    first <- htmltools::attachDependencies(
      tip_on(button(), "One", id = "a", pin = TRUE, events = "pin"),
      other,
      append = TRUE
    )
    expect_true("hovercraft-pin" %in% dep_names(first))
    again <- suppressWarnings(tip_on(first, "Two"))
    expect_false("hovercraft-pin" %in% dep_names(again))
    expect_identical(sum(dep_names(again) == "hovercraft"), 1L)
    expect_identical(sum(dep_names(again) == "hovercraft-runtime"), 1L)
    expect_true("other" %in% dep_names(again))
    styled <- tip_on(button(), "One", style = tip_style(theme = list(radius = 9)))
    expect_true(any(startsWith(dep_names(styled), "hovercraft-theme")))
    expect_false(any(startsWith(dep_names(tip_on(styled, "Two")), "hovercraft-theme")))
  })

  it("gives back the control as it was when a wrapper goes", {
    control <- htmltools::tags$button("Go", class = "btn w-50 d-block", style = "width: 50%; color: red", disabled = NA)
    wrapped <- tip_on(control, "One", when_disabled = "why")
    expect_true(tip_disabled_attribute("fill") %in% names(wrapped$children[[1]]$attribs))
    again <- tip_on(wrapped, "Two")
    expect_identical(again$name, "button")
    expect_false(tip_disabled_attribute("fill") %in% names(again$attribs))
    expect_match(htmltools::tagGetAttribute(again, "style"), "width: 50%;", fixed = TRUE)
    expect_match(htmltools::tagGetAttribute(again, "style"), "color: red;", fixed = TRUE)
    expect_setequal(strsplit(htmltools::tagGetAttribute(again, "class"), " ")[[1]], c("btn", "w-50", "d-block"))
    expect_false("hovercraft-disabled" %in% dep_names(again))
    # With a new reason, one wrapper stays and carries the new tip.
    rewrapped <- tip_on(wrapped, "Two", when_disabled = "new why")
    expect_identical(rewrapped$name, "span")
    expect_identical(htmltools::tagGetAttribute(rewrapped, tip_disabled_attribute()), "new why")
    expect_identical(sum(dep_names(rewrapped) == "hovercraft-disabled"), 1L)
  })

  it("refuses CSS variables in a style given to a spec or a table, from the call that was written", {
    css <- tip_style(theme = list("--hovercraft-tip-radius" = "9px"))
    call_of <- function(code) tryCatch(code, error = function(e) rlang::call_name(e$call))
    expect_identical(call_of(tip_spec("x", style = css)), "tip_spec")
    skip_if_not_installed("DT")
    scores <- data.frame(team = c("Owls", "Foxes"), wins = c(7, 5))
    expect_identical(call_of(tip_table(DT::datatable(scores), rows = "{{ team }}", style = css)), "tip_table")
    expect_error(tip_spec("x", style = css), class = "hovercraft_error_style")
  })
})

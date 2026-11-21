skip_if_not_installed("withr")
button <- function() htmltools::tags$button("Run")

describe("the tip_on() errors", {
  withr::defer(tip_reset())
  tip_reset()

  it("say a tag with no focus gives keyboard users no way to the tip", {
    rlang::reset_warning_verbosity("hovercraft-focus")
    expect_snapshot(invisible(tip_on(htmltools::tags$tr("x"), "Row")))
  })
  it("say a template name is already taken", {
    tip_define(demo = tip_template(tip_text("one")))
    expect_snapshot(invisible(tip_define(demo = tip_template(tip_text("two")))))
  })
  it("say what a bad argument was", {
    expect_snapshot(tip_on(button(), 1), error = TRUE)
    expect_snapshot(tip_define(tip_template(tip_text("a"))), error = TRUE)
    expect_snapshot(tip_rows(list(a = 1)), error = TRUE)
  })
  it("say a tag list is not a tag", {
    expect_snapshot(tip_on(htmltools::tagList(button(), button()), "x"), error = TRUE)
    expect_snapshot(tip_on("x", "x"), error = TRUE)
  })
  it("say there is nothing to show", {
    expect_snapshot(tip_on(button()), error = TRUE)
  })
  it("say a misspelt argument is not taken", {
    expect_snapshot(tip_on(button(), "x", tmplate = "a"), error = TRUE)
  })
  it("say what a record lacks, and what it holds", {
    tip_define(diff = tip_template(tip_title("{{ label }}"), tip_text("{{ n }} {{ m }}")))
    expect_snapshot(tip_on(button(), template = "diff", record = list(label = "x")), error = TRUE)
    expect_snapshot(tip_on(button(), template = "diff"), error = TRUE)
  })
  it("warn about a name not defined, and an engine name", {
    rlang::reset_warning_verbosity("hovercraft-template-dif")
    expect_snapshot(invisible(tip_on(button(), template = "dif", record = list(label = "x"))))
    tip_define(withid = tip_template(tip_text("{{ tip_id }} {{ label }}")))
    expect_snapshot(invisible(tip_on(button(), template = "withid", record = list(label = "x", tip_id = "u"))))
    expect_snapshot(invisible(tip_on(button(), "x", record = list(a = 1))))
  })
})

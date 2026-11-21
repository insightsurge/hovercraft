describe("the template errors", {
  it("say which block is wrong", {
    expect_snapshot(tip_template(), error = TRUE)
    expect_snapshot(tip_template(tip_title("a"), "text"), error = TRUE)
    expect_snapshot(tip_template(list(type = "blink")), error = TRUE)
    expect_snapshot(tip_template(list(type = "rows")), error = TRUE)
    expect_snapshot(tip_template(list(type = "text", text = "x", colour = "red")), error = TRUE)
  })
  it("say what a template name may be", {
    expect_snapshot(tip_define(`My Template` = tip_template(tip_text("a"))), error = TRUE)
  })
  it("print a template", {
    expect_snapshot(tip_template(tip_title("a"), tip_text("b")))
  })
})

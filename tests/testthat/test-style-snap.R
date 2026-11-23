describe("the style errors", {
  it("say style is not CSS, and what it takes", {
    expect_snapshot(tip_as_style("color: red"), error = TRUE)
    expect_snapshot(tip_style("color: red; font-size: 12px"), error = TRUE)
    expect_snapshot(tip_on(htmltools::tags$button("Run"), "t", style = "color:red"), error = TRUE)
  })
  it("say a list is not a style, and where the roles go", {
    expect_snapshot(tip_as_style(list(bg = "red")), error = TRUE)
    expect_snapshot(tip_as_style(3), error = TRUE)
  })
  it("say a name is not a preset, and which are", {
    expect_snapshot(tip_style("sobre"), error = TRUE)
  })
  it("say what a class name is, and which start is reserved", {
    expect_snapshot(tip_style(class = c("fine", "1a", "b.c")), error = TRUE)
    expect_snapshot(tip_style(class = "hc-tip-wide"), error = TRUE)
  })
  it("say ... must be empty", {
    expect_snapshot(tip_style("sober", themee = 1), error = TRUE)
  })
  it("print a style", {
    expect_snapshot(tip_style("dense", theme = tip_theme(light = list(radius = 6)), class = "wide"))
    expect_snapshot(tip_style())
  })
})

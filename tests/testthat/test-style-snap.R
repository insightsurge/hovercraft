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
  it("say a size is outside the engine's range, and which range", {
    expect_snapshot(tip_style(gap = 30), error = TRUE)
    expect_snapshot(tip_style(max_width = 100), error = TRUE)
    expect_snapshot(tip_style(pin_width = 300.5), error = TRUE)
    expect_snapshot(tip_style(gap = "wide"), error = TRUE)
  })
  it("say a motion is not one of the choices, and the close one", {
    expect_snapshot(tip_style(motion = "slid"), error = TRUE)
    expect_snapshot(tip_style(motion = "bounce"), error = TRUE)
  })
  it("say ... must be empty", {
    expect_snapshot(tip_style("sober", themee = 1), error = TRUE)
    expect_snapshot(tip_style(delay = 300), error = TRUE)
  })
  it("print a style", {
    expect_snapshot(tip_style("dense", theme = tip_theme(light = list(radius = 6)), class = "wide"))
    expect_snapshot(tip_style())
    expect_snapshot(tip_style("modern", motion = "slide", gap = 12, max_width = 280, pin_width = 500))
  })
})

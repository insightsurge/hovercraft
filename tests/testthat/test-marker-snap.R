describe("the marker errors", {
  it("say what a marker takes", {
    expect_snapshot(tip_marker("undrline"), error = TRUE)
    expect_snapshot(tip_marker(), error = TRUE)
    expect_snapshot(tip_as_marker(list("underline")), error = TRUE)
    expect_snapshot(tip_on(htmltools::tags$span("Word"), "A tip", marker = "under"), error = TRUE)
  })
  it("say a line is only for an underline, and which lines there are", {
    expect_snapshot(tip_marker("corner", line = "wavy"), error = TRUE)
    expect_snapshot(tip_marker("underline", line = "wavey"), error = TRUE)
  })
  it("say an icon is a tag, and where to get one", {
    expect_snapshot(tip_marker("icon", icon = "info"), error = TRUE)
    expect_snapshot(tip_marker("underline", icon = htmltools::tags$b("?")), error = TRUE)
  })
  it("say a tag that holds nothing has nowhere for a marker", {
    expect_snapshot(tip_on(htmltools::tags$input(), "A tip", marker = "icon"), error = TRUE)
  })
  it("say a block with no words of its own has nothing to underline", {
    expect_snapshot(
      tip_on(htmltools::tags$div(htmltools::tags$p("Only a block"), tabindex = "0"), "A tip", marker = "underline"),
      error = TRUE
    )
  })
  it("say ... must be empty", {
    expect_snapshot(tip_marker("underline", lines = "wavy"), error = TRUE)
  })
})

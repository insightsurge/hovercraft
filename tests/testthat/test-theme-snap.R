describe("the theme errors and warnings", {
  it("say a role is misspelt, and what the roles are", {
    expect_snapshot(tip_theme(light = list(bgg = "red")), error = TRUE)
  })
  it("say what a role takes", {
    expect_snapshot(tip_theme(light = list(bg = "notacolour")), error = TRUE)
    expect_snapshot(tip_theme(dark = list(radius = "1;}")), error = TRUE)
    expect_snapshot(tip_theme(dark = list(font = "a; b")), error = TRUE)
  })
  it("say ... must be empty, and what the arguments are", {
    expect_snapshot(tip_theme(bg = "red"), error = TRUE)
  })
  it("say a mode is not one", {
    expect_snapshot(tip_theme(mode = "x"), error = TRUE)
  })
  it("say a mix of roles and CSS variables is not a theme", {
    expect_snapshot(tip_as_theme(list(bg = "red", "--a" = "1")), error = TRUE)
    expect_snapshot(tip_as_theme(3), error = TRUE)
  })
  it("warn about low contrast with the pair and the ratio", {
    expect_snapshot(invisible(tip_theme(light = list(bg = "#ffffff", ink = "#dddddd"))))
    expect_snapshot(invisible(tip_theme(light = list(bg = "#1b3a6b", ink = "#ffffff", code_bg = "#7a9cc6"))))
  })
  it("print a theme", {
    expect_snapshot(tip_theme(light = list(bg = "navy", radius = 8), dark = list(bg = "#9ec5fe")))
    expect_snapshot(tip_theme())
  })
})

skip_if_not_installed("DT")

scores <- data.frame(team = c("Owls", "Foxes"), wins = c(7, 5), losses = c(3, 5))
dt <- function() DT::datatable(scores, rownames = FALSE)

describe("the tip_table() errors", {
  it("list the columns when a placeholder or a name is not one", {
    expect_snapshot(tip_table(dt(), rows = "{{ nope }}"), error = TRUE)
    expect_snapshot(tip_table(dt(), cells = c(winz = "x")), error = TRUE)
  })
  it("say what a header tip and a reserved name cannot do", {
    expect_snapshot(tip_table(dt(), headers = c(wins = "{{ wins }}")), error = TRUE)
    expect_snapshot(tip_table(dt(), rows = "{{ .value }}"), error = TRUE)
  })
  it("say what the widget and the content must be", {
    expect_snapshot(tip_table(scores, rows = "x"), error = TRUE)
    expect_snapshot(tip_table(dt(), rows = 1), error = TRUE)
    expect_snapshot(tip_table(dt()), error = TRUE)
  })
  it("say what an id and a spec must be", {
    expect_snapshot(tip_table(dt(), rows = "x", id = "two words"), error = TRUE)
    expect_snapshot(tip_table(dt(), cells = tip_spec("x")), error = TRUE)
    expect_snapshot(tip_spec(1), error = TRUE)
    expect_snapshot(tip_spec("x", placement = "above"), error = TRUE)
  })
  it("say what a table can mark", {
    expect_snapshot(tip_table(dt(), cells = c(wins = "x"), marker = "underline"), error = TRUE)
    expect_snapshot(tip_table(dt(), rows = tip_spec("x", marker = "corner")), error = TRUE)
    expect_snapshot(tip_table(dt(), rows = "x", marker = "corner"), error = TRUE)
  })
  it("say what a pin and the events of a row need", {
    expect_snapshot(tip_table(dt(), rows = "x", id = "t", pin = "click"), error = TRUE)
    expect_snapshot(tip_table(dt(), rows = "x", pin = TRUE), error = TRUE)
    expect_snapshot(tip_table(dt(), cells = c(wins = "x"), id = "t", pin = TRUE), error = TRUE)
    expect_snapshot(tip_table(dt(), rows = "x", events = "pin"), error = TRUE)
    expect_snapshot(tip_table(dt(), cells = c(wins = "x"), id = "t", events = "pin"), error = TRUE)
    expect_snapshot(
      tip_table(
        dt(),
        cells = list(wins = tip_spec("x", more = "y", more_on = "pin")),
        rows = "x",
        id = "t",
        pin = TRUE
      ),
      error = TRUE
    )
  })
  it("say what a style must be", {
    expect_snapshot(tip_table(dt(), rows = "x", style = "nope"), error = TRUE)
    expect_snapshot(tip_spec("x", style = "color: red"), error = TRUE)
  })
})

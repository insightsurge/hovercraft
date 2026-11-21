describe("the argument checks", {
  it("pass a good value without a sound", {
    expect_no_error(tip_arg_string("a", "x"))
    expect_no_error(tip_arg_string(NULL, "x", null.ok = TRUE))
    expect_no_error(tip_arg_string(NA_character_, "x", na.ok = TRUE, min.chars = 0L))
    expect_no_error(tip_arg_flag(TRUE, "x"))
    expect_no_error(tip_arg_list(list(a = 1), "x"))
    expect_no_error(tip_arg_chars("a", "x", null.ok = TRUE))
    expect_no_error(tip_arg_chars(NULL, "x", null.ok = TRUE))
    expect_no_error(tip_arg_data(data.frame(a = 1), "x"))
  })

  it("stop with a classed error that names the argument, what is wrong and the fix", {
    err <- expect_error(tip_arg_string(1, "text"), class = "hovercraft_error_argument")
    expect_match(conditionMessage(err), "text", fixed = TRUE)
    expect_match(conditionMessage(err), "Give one non-empty string", fixed = TRUE)
    expect_error(tip_arg_string(NULL, "x"), class = "hovercraft_error_argument")
    expect_match(conditionMessage(expect_error(tip_arg_string(1, "x", null.ok = TRUE))), "NULL")
    expect_error(tip_arg_flag("yes", "x"), class = "hovercraft_error_argument")
    expect_error(tip_arg_list(list(1), "x"), class = "hovercraft_error_argument")
    expect_error(tip_arg_chars(1, "x"), class = "hovercraft_error_argument")
    expect_error(tip_arg_chars(character(), "x"), class = "hovercraft_error_argument")
    expect_error(tip_arg_data(list(), "x"), class = "hovercraft_error_argument")
  })

  it("keep braces in the problem as they are", {
    err <- expect_error(tip_arg("{oops}", "x", "Fix it."), class = "hovercraft_error_argument")
    expect_match(conditionMessage(err), "{oops}", fixed = TRUE)
  })
})

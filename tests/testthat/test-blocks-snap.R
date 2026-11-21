describe("the block errors", {
  it("say what a bad argument was", {
    expect_snapshot(tip_title(1), error = TRUE)
    expect_snapshot(tip_fields(A = 1), error = TRUE)
  })
  it("say where a bad placeholder is", {
    expect_snapshot(tip_text("{{ x | nope }}"), error = TRUE)
    expect_snapshot(tip_fields(Records = "{{ records"), error = TRUE)
  })
  it("say Copy comes later", {
    expect_snapshot(tip_code("sql", copy = TRUE), error = TRUE)
  })
  it("warn about a placeholder in a label", {
    expect_snapshot(tip_fields(`{{ a }}` = "b"))
  })
})

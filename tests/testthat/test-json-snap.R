describe("the record errors", {
  it("name every value that cannot be shown", {
    expect_snapshot(tip_record(list(a = list(1), b = 1:2, c = I("x"), d = 1i, ok = 1)), error = TRUE)
  })
  it("say where tip_rows() is", {
    expect_snapshot(tip_record(data.frame(a = 1:2)), error = TRUE)
  })
  it("name a column with braces as it is", {
    expect_snapshot(tip_record(list(`a{b}` = list(1))), error = TRUE)
  })
})

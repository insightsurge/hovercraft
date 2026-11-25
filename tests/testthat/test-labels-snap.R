describe("the error for a key that is not a label", {
  it("prints the way out for one misspelt key", {
    expect_snapshot(tip_check_labels(c(copy_faild = "x")), error = TRUE)
  })
})

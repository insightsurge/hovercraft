describe("the placeholder errors", {
  it("name every mistake in a text", {
    expect_snapshot(
      tip_check_text("{{ a | numbr:2 }} {{ b | date:d }} {{ }} {{ c | number:two }} {{ d {{ e }} {{ f", "The text"),
      error = TRUE
    )
  })
  it("say a chain of filters is one too many", {
    expect_snapshot(tip_check_text("{{ a | number:1 | percent:1 }}", "The text"), error = TRUE)
  })
})

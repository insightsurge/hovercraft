describe("the block functions", {
  it("make classed lists in the engine's words", {
    expect_identical(unclass(tip_title("{{ label }}")), list(type = "title", text = "{{ label }}"))
    expect_identical(unclass(tip_text("**x**")), list(type = "text", text = "**x**"))
    expect_identical(unclass(tip_badge("ok")), list(type = "badge", text = "ok"))
    expect_identical(unclass(tip_badge("{{ s }}", tone = "warning"))$tone, "warning")
    expect_identical(unclass(tip_code("sql")), list(type = "code", field = "sql"))
    expect_identical(unclass(tip_code("sql", language = "sql"))$language, "sql")
    for (block in list(tip_title("a"), tip_text("a"), tip_badge("a"), tip_fields(A = "a"), tip_code("a"))) {
      expect_s3_class(block, "hovercraft_block")
    }
  })

  it("make fields as an array of label and value pairs, one pair too", {
    one <- tip_fields(Records = "{{ records | number:0 }}")
    expect_identical(unclass(one)$fields, list(list(label = "Records", value = "{{ records | number:0 }}")))
    two <- tip_fields(A = "a", B = "b")
    expect_length(unclass(two)$fields, 2L)
  })

  it("check each argument", {
    expect_error(tip_title(1), "text")
    expect_error(tip_title(""), "text")
    expect_error(tip_badge("a", tone = 1), "tone")
    expect_error(tip_fields(), "...")
    expect_error(tip_fields("a"), "\\.\\.\\.")
    expect_error(tip_fields(A = 1), "A")
    expect_error(tip_code(1), "field")
    expect_error(tip_code("a", language = 1), "language")
    expect_error(tip_code("a", copy = "yes"), "copy")
  })

  it("check the placeholders in every text", {
    expect_error(tip_title("{{ a"), class = "hovercraft_error_placeholder")
    expect_error(tip_text("{{ a | nope }}"), class = "hovercraft_error_placeholder")
    expect_error(tip_badge("{{ }}"), class = "hovercraft_error_placeholder")
    expect_error(tip_fields(A = "{{ a | number:x }}"), class = "hovercraft_error_placeholder")
  })

  it("accept {{ in a label, but warn: a label is plain text", {
    expect_warning(tip_fields(`{{ a }}` = "b"), class = "hovercraft_warning_label")
  })

  it("refuse Copy until the tip can stay open", {
    expect_error(tip_code("a", copy = TRUE), class = "hovercraft_error_pin")
  })

  it("print one line", {
    expect_output(print(tip_title("a")), "<tip block: title>", fixed = TRUE)
    expect_output(returned <- print(tip_title("a")))
    expect_identical(returned, tip_title("a"))
  })
})

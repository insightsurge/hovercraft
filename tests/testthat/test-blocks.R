describe("the block functions", {
  it("make classed lists in the engine's words", {
    expect_identical(unclass(tip_title("{{ label }}")), list(type = "title", text = "{{ label }}"))
    expect_identical(unclass(tip_text("**x**")), list(type = "text", text = "**x**"))
    expect_identical(unclass(tip_badge("ok")), list(type = "badge", text = "ok"))
    expect_identical(unclass(tip_badge("{{ s }}", status = "warning"))$tone, "warning")
    expect_identical(unclass(tip_code("sql")), list(type = "code", field = "sql"))
    expect_identical(unclass(tip_code("sql", language = "sql"))$language, "sql")
    for (block in list(tip_title("a"), tip_text("a"), tip_badge("a"), tip_fields(A = "a"), tip_code("a"))) {
      expect_s3_class(block, "hovercraft_block")
    }
  })

  it("write a badge's status into the engine's tone field, for each of the four words or a placeholder", {
    for (status in c("success", "warning", "danger", "info")) {
      expect_identical(unclass(tip_badge("a", status = status))$tone, status)
    }
    expect_identical(unclass(tip_badge("{{ label }}", status = "{{ tone }}"))$tone, "{{ tone }}")
    expect_null(unclass(tip_badge("a"))$tone)
  })

  it("check the placeholder syntax when a badge's status uses one", {
    expect_error(tip_badge("a", status = "{{ }}"), class = "hovercraft_error_placeholder")
    expect_error(tip_badge("a", status = "{{ tone"), class = "hovercraft_error_placeholder")
    expect_error(tip_badge("a", status = "{{ tone | nope }}"), class = "hovercraft_error_placeholder")
  })

  it("take a status placeholder only when it is the whole value", {
    expect_s3_class(tip_badge("a", status = "{{ tone }}"), "hovercraft_block")
    expect_s3_class(tip_badge("a", status = "  {{ tone }}  "), "hovercraft_block")
    # "pre success" is not one of the four words, so a badge made from it would always be plain.
    expect_error(tip_badge("a", status = "pre {{ tone }}"), class = "hovercraft_error_status")
    expect_error(tip_badge("a", status = "{{ tone }} post"), class = "hovercraft_error_status")
    expect_error(tip_badge("a", status = "{{ a }}{{ b }}"), class = "hovercraft_error_status")
  })

  it("stop on any other status word, and list the four", {
    err <- expect_error(tip_badge("a", status = "ok"), class = "hovercraft_error_status")
    for (word in c("success", "warning", "danger", "info")) {
      expect_match(conditionMessage(err), word, fixed = TRUE)
    }
    expect_error(tip_badge("a", status = "Warning"), class = "hovercraft_error_status")
    expect_error(tip_badge("a", tone = "warning"), "unused argument")
  })

  it("make fields as an array of label and value pairs, one pair too", {
    one <- tip_fields(Records = "{{ records | number:0 }}")
    expect_identical(unclass(one)$fields, list(list(label = "Records", value = "{{ records | number:0 }}")))
    two <- tip_fields(A = "a", B = "b")
    expect_length(unclass(two)$fields, 2L)
  })

  it("check each argument, with an error of the package's class that says the fix", {
    expect_error(tip_title(1), "text", class = "hovercraft_error_argument")
    expect_error(tip_title(""), "text", class = "hovercraft_error_argument")
    expect_error(tip_badge("a", status = 1), "status", class = "hovercraft_error_argument")
    expect_error(tip_fields(), "\\.\\.\\.", class = "hovercraft_error_argument")
    expect_error(tip_fields("a"), "\\.\\.\\.", class = "hovercraft_error_argument")
    expect_error(tip_fields(A = 1), "A", class = "hovercraft_error_argument")
    expect_error(tip_code(1), "field", class = "hovercraft_error_argument")
    expect_error(tip_code("a", language = 1), "language", class = "hovercraft_error_argument")
    expect_error(tip_code("a", copy = "yes"), "copy", class = "hovercraft_error_argument")
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

  it("take Copy, which the engine draws in a tip that stays open, and write nothing for no Copy", {
    expect_true(tip_code("a", copy = TRUE)$copy)
    expect_false("copy" %in% names(tip_code("a")))
    expect_error(tip_code("a", copy = "yes"), class = "hovercraft_error_argument")
  })

  it("print one line", {
    expect_output(print(tip_title("a")), "<tip block: title>", fixed = TRUE)
    expect_output(returned <- print(tip_title("a")))
    expect_identical(returned, tip_title("a"))
  })
})

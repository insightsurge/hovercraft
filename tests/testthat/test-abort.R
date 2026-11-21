describe("tip_bullets()", {
  it("marks every problem and says how many more there were", {
    expect_named(tip_bullets(c("a", "b")), c("x", "x"))
    many <- tip_bullets(paste("problem", 1:12))
    expect_length(many, 11L)
    expect_identical(unname(many[["i"]]), "... and 2 more.")
  })
})

describe("tip_near()", {
  it("suggests a close name, and says nothing for a far one", {
    expect_match(tip_near("dif", c("diff", "other")), "Did you mean \"diff\"")
    expect_identical(tip_near("zzzzzz", c("diff", "other")), "")
  })
})

describe("tip_escape()", {
  it("doubles braces so cli prints them as written", {
    expect_identical(tip_escape("{{ a }}"), "{{{{ a }}}}")
    expect_identical(cli::format_inline(tip_escape("{{ a }}")), "{{ a }}")
  })
})

describe("tip_abort() and tip_warn()", {
  it("add the package's class to the one given", {
    expect_error(tip_abort("x", class = "hovercraft_error_demo"), class = "hovercraft_error")
    expect_error(tip_abort("x", class = "hovercraft_error_demo"), class = "hovercraft_error_demo")
    expect_warning(tip_warn("x", class = "hovercraft_warning_demo"), class = "hovercraft_warning")
  })
})

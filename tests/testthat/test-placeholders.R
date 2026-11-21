cases <- jsonlite::read_json(tip_path("placeholders.json"), simplifyVector = FALSE)$cases

describe("tip_parse()", {
  it("reads every shared case as the engine and the build check do", {
    expect_gte(length(cases), 62L)
    for (case in cases) {
      got <- tip_parse(case$text)
      expect_identical(got$fields, as.character(unlist(case$fields)), info = case$name)
      expect_identical(got$kinds, as.character(unlist(case$problems)), info = case$name)
      expect_length(got$sentences, length(got$kinds))
    }
  })

  it("says every mistake in words", {
    got <- tip_parse("{{ }} {{ x | date:d }} {{ y")
    expect_match(got$sentences[[1]], "names no field")
    expect_match(got$sentences[[2]], "later version")
    expect_match(got$sentences[[3]], "at character 24 never closes")
  })

  it("counts characters, not bytes", {
    got <- tip_parse("naïve {{ label")
    expect_match(got$sentences, "at character 7 ")
  })
})

describe("tip_filter_problem()", {
  it("takes the filters and arguments the engine's rules allow", {
    expect_null(tip_filter_problem("number:2"))
    expect_null(tip_filter_problem("percent"))
    expect_identical(tip_filter_problem("number:101")$kind, "arg")
    expect_identical(tip_filter_problem("number:-1")$kind, "arg")
    expect_identical(tip_filter_problem("numbr")$kind, "unknown")
  })

  it("holds an argument that is required to it", {
    rules <- tip_rules()
    rules$filters$number$arg_required <- TRUE
    local_mocked_bindings(tip_rules = function() rules)
    expect_identical(tip_filter_problem("number")$kind, "arg")
    expect_null(tip_filter_problem("number:3"))
  })
})

describe("tip_check_text()", {
  it("returns the fields of a good text, without a sound", {
    expect_identical(tip_check_text("{{ a }} and {{ b | number:1 }}", "A text"), c("a", "b"))
    expect_identical(tip_check_text("no placeholders", "A text"), character())
  })

  it("stops with a classed error that names every mistake", {
    expect_error(tip_check_text("{{ a | nope }} {{", "A text"), class = "hovercraft_error_placeholder")
  })
})

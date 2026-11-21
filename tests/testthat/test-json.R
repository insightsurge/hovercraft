describe("tip_json()", {
  it("writes NA, NaN and infinite numbers as null, and keeps 15 significant digits", {
    expect_identical(
      as.character(tip_json(list(a = NA, b = NaN, c = Inf, d = NA_character_))),
      '{"a":null,"b":null,"c":null,"d":null}'
    )
    expect_identical(as.character(tip_json(list(x = 1 / 3))), '{"x":0.333333333333333}')
  })

  it("escapes < so a value can never close a script", {
    json <- tip_json(list(x = "</script><b>"))
    expect_false(grepl("<", json, fixed = TRUE))
    expect_identical(jsonlite::fromJSON(json)$x, "</script><b>")
  })

  it("keeps lists as arrays and a one-pair list an array", {
    expect_identical(as.character(tip_json(list(list(label = "a", value = "b")))), '[{"label":"a","value":"b"}]')
  })
})

describe("tip_value()", {
  it("keeps text, numbers and logicals, and leaves a NULL out", {
    expect_identical(tip_value("a")$value, "a")
    expect_identical(tip_value(2L)$value, 2)
    expect_identical(tip_value(TRUE)$value, TRUE)
    expect_true(tip_value(NULL)$skip)
  })

  it("turns missing and non-finite numbers into NA", {
    expect_identical(tip_value(NA_character_)$value, NA_character_)
    expect_identical(tip_value(NaN)$value, NA_real_)
    expect_identical(tip_value(-Inf)$value, NA_real_)
  })

  it("writes factors, dates and times as text", {
    expect_identical(tip_value(factor("b"))$value, "b")
    expect_identical(tip_value(as.Date("2026-11-21"))$value, "2026-11-21")
    expect_identical(tip_value(as.POSIXct("2026-11-21 10:30:00", tz = "UTC"))$value, "2026-11-21T10:30:00Z")
    expect_identical(tip_value(as.Date(NA))$value, NA_character_)
  })

  it("refuses a list, a vector, an I() value and a class it cannot show", {
    expect_match(tip_value(list(1))$problem, "is a list")
    expect_match(tip_value(1:3)$problem, "has 3 values")
    expect_match(tip_value(character())$problem, "has 0 values")
    expect_match(tip_value(I("a"))$problem, "I\\(\\)")
    expect_match(tip_value(1i)$problem, "is complex")
  })
})

describe("tip_record()", {
  it("takes a named list and gives back single values, in order", {
    got <- tip_record(list(b = 2, a = "x", gone = NULL, n = NA))
    expect_identical(names(got), c("b", "a", "n"))
  })

  it("takes a data frame of one row, a factor and a list-column cell included", {
    one <- data.frame(label = "ADAE", n = 3)
    one$cell <- list("kept")
    expect_identical(tip_record(one), list(label = "ADAE", n = 3, cell = "kept"))
  })

  it("gives {} for no values", {
    expect_identical(as.character(tip_json(tip_record(list()))), "{}")
  })

  it("refuses a data frame of more than one row, and says where tip_rows() is", {
    expect_error(tip_record(data.frame(a = 1:2)), class = "hovercraft_error_record")
  })

  it("collects every bad value, with its path", {
    err <- expect_error(
      tip_record(list(a = list(1), b = 1:2, c = I("x"), ok = 1)),
      class = "hovercraft_error_record"
    )
    expect_match(conditionMessage(err), "record$a", fixed = TRUE)
    expect_match(conditionMessage(err), "record$b", fixed = TRUE)
    expect_match(conditionMessage(err), "record$c", fixed = TRUE)
    expect_no_match(conditionMessage(err), "record$ok", fixed = TRUE)
  })

  it("needs names", {
    expect_error(tip_record(list(1, 2)), "record")
  })
})

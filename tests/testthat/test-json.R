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
  it("keeps text, numbers and logicals, and writes NULL as missing", {
    expect_identical(tip_value("a")$value, "a")
    expect_identical(tip_value(2L)$value, 2)
    expect_identical(tip_value(TRUE)$value, TRUE)
    expect_identical(tip_value(NULL)$value, NA)
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

  it("writes a POSIXlt time like a POSIXct one, in UTC", {
    expect_identical(tip_value(as.POSIXlt("2026-11-21 10:30:00", tz = "UTC"))$value, "2026-11-21T10:30:00Z")
    expect_identical(tip_value(as.POSIXlt("2026-11-21 12:30:00", tz = "Europe/Berlin"))$value, "2026-11-21T11:30:00Z")
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
    expect_identical(names(got), c("b", "a", "gone", "n"))
  })

  it("takes a data frame of one row, a factor and a list-column cell included", {
    one <- data.frame(label = "ADAE", n = 3)
    one$cell <- list("kept")
    expect_identical(tip_record(one), list(label = "ADAE", n = 3, cell = "kept"))
  })

  it("gives {} for no values", {
    expect_identical(as.character(tip_json(tip_record(list()))), "{}")
  })

  it("refuses a data frame of more than one row", {
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

  it("writes a NULL as null, as the engine shows a dash for it", {
    expect_identical(as.character(tip_json(tip_record(list(a = NULL)))), '{"a":null}')
  })

  it("keeps only the columns asked for, before it looks at any value", {
    got <- tip_record(list(a = 1, b = list(1, 2), c = "x"), keep = c("a", "c", "zzz"))
    expect_identical(got, list(a = 1, c = "x"))
    one <- data.frame(a = 1, c = "x")
    one$b <- list(1:3)
    expect_identical(tip_record(one, keep = c("a", "c")), list(a = 1, c = "x"))
  })

  it("refuses a data-frame or matrix column, and says how to spread it", {
    frame <- data.frame(a = 1)
    frame$m <- matrix(1:2, nrow = 1)
    expect_error(tip_record(frame), class = "hovercraft_error_record")
    inner <- data.frame(a = 1)
    inner$d <- data.frame(x = 1, y = 2)
    expect_error(tip_record(inner), "d", class = "hovercraft_error_record")
  })

  it("warns about a whole number above 2^53: ids belong in text", {
    expect_warning(tip_record(list(id = 2^60, ok = 2^53)), class = "hovercraft_warning_number")
    expect_warning(tip_record(list(id = 1234567890123456789)), class = "hovercraft_warning_number")
    expect_no_warning(tip_record(list(id = 2^53, x = 1.5e300, y = NA_real_)))
  })

  it("needs names", {
    expect_error(tip_record(list(1, 2)), "record")
  })
})

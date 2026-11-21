describe("tip_channels()", {
  it("reads names, hex, rgb() and rgba() text, and three numbers", {
    expect_identical(tip_channels("white"), c(255, 255, 255, 1))
    expect_identical(tip_channels("#102030"), c(16, 32, 48, 1))
    expect_identical(tip_channels("#fff"), c(255, 255, 255, 1))
    expect_identical(tip_channels("rgb(1, 2, 3)"), c(1, 2, 3, 1))
    expect_identical(tip_channels("RGBA(1, 2, 3, 0.5)"), c(1, 2, 3, 0.5))
    expect_identical(tip_channels(c(10, 20, 30)), c(10, 20, 30, 1))
    expect_equal(tip_channels("transparent")[[4]], 0)
  })

  it("refuses what is not a colour, and a number that could be an R palette index", {
    expect_null(tip_channels("notacolour"))
    expect_null(tip_channels(3))
    expect_null(tip_channels(c(1, 2, 300)))
    expect_null(tip_channels("rgb(1, 2)"))
    expect_null(tip_channels("rgb(1, 2, 3, 4)"))
    expect_null(tip_channels("rgb(256, 0, 0)"))
    expect_null(tip_channels("rgba(1, 2, 3, 2)"))
    expect_null(tip_channels("rgb(a, 2, 3)"))
    expect_null(tip_channels("rgb(1, 2, 3"))
    expect_null(tip_channels(NA_character_))
    expect_null(tip_channels(list()))
  })
})

describe("tip_colour()", {
  it("writes #rrggbb in lower case, and rgba() when the colour is not opaque", {
    expect_identical(tip_colour("Navy"), "#000080")
    expect_identical(tip_colour("rgb(1, 2, 3)"), "#010203")
    expect_identical(tip_colour("rgba(0, 0, 0, 0.5)"), "rgba(0, 0, 0, 0.5)")
    expect_identical(tip_colour(c(1.5, 2, 3)), "rgba(1.5, 2, 3, 1)")
  })

  it("passes a CSS variable through, as var()", {
    expect_identical(tip_colour("--team-blue"), "var(--team-blue)")
    expect_identical(tip_colour("var( --team-blue )"), "var( --team-blue )")
    expect_true(tip_is_var("--a1-b_c"))
    expect_false(tip_is_var("--1a"))
    expect_false(tip_is_var(c("--a", "--b")))
  })

  it("gives NULL for what is neither", {
    expect_null(tip_colour("nope"))
  })
})

describe("tip_contrast()", {
  it("is 21:1 for black on white, 1:1 for the same colour, and the same either way round", {
    expect_equal(tip_contrast("#000000", "#ffffff"), 21)
    expect_equal(tip_contrast("#777777", "#777777"), 1)
    expect_equal(tip_contrast("#ffffff", "#000000"), 21)
  })

  it("lays an ink with alpha over the fill", {
    expect_equal(tip_contrast("rgba(0, 0, 0, 0)", "#ffffff"), 1)
    expect_gt(tip_contrast("rgba(0, 0, 0, 0.5)", "#ffffff"), 3)
  })

  it("measures the 4.5:1 line the way WCAG does", {
    expect_gt(tip_contrast("#767676", "#ffffff"), 4.5)
    expect_lt(tip_contrast("#777777", "#ffffff"), 4.5)
  })
})

describe("tip_css_text()", {
  it("keeps short safe text and trims it", {
    expect_identical(tip_css_text(" 0 4px 14px rgba(0, 0, 0, 0.18) "), "0 4px 14px rgba(0, 0, 0, 0.18)")
    expect_identical(tip_css_text("\"Open Sans\", system-ui, sans-serif"), "\"Open Sans\", system-ui, sans-serif")
  })

  it("refuses what could end a declaration, open a block, add a URL or a comment", {
    for (bad in c(
      "a; b",
      "a}",
      "a{",
      "<b>",
      "a\\b",
      "url(x)",
      "expression(x)",
      "@import x",
      "a/*b*/",
      "",
      NA_character_
    )) {
      expect_null(tip_css_text(bad), info = bad)
    }
    expect_null(tip_css_text(1))
  })
})

describe("tip_length()", {
  it("takes pixels, a length and a variable", {
    expect_identical(tip_length(8), "8px")
    expect_identical(tip_length(0), "0px")
    expect_identical(tip_length("0.5rem"), "0.5rem")
    expect_identical(tip_length("4"), "4")
    expect_identical(tip_length("50%"), "50%")
    expect_identical(tip_length("--r"), "var(--r)")
  })

  it("refuses the rest", {
    for (bad in list(-1, "1;", "px", "1 2", NA_real_, c(1, 2), "calc(1px)")) {
      expect_null(tip_length(bad), info = format(bad))
    }
  })
})

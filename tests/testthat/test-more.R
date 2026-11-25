describe("the long form's layer files", {
  css <- paste(readLines(system.file("more", "more.css", package = "hovercraft")), collapse = "\n")
  js <- paste(readLines(system.file("more", "more.js", package = "hovercraft")), collapse = "\n")

  it("spell the button's class and the engine's hint box, which the room for the button is reserved on", {
    expect_true(grepl(".hovercraft-more-strip", css, fixed = TRUE))
    expect_true(grepl(paste0(".", tip_names()$css, "[data-hovercraft-more-room]"), css, fixed = TRUE))
    expect_true(grepl("hovercraft-more-strip", js, fixed = TRUE))
  })

  it("get every name of the engine from R, and write none of the engine's own", {
    expect_false(grepl(tip_names()$attribute, js, fixed = TRUE))
    dependency <- tip_more_dependency()
    expect_null(dependency$head)
    config <- jsonlite::fromJSON(
      rawToChar(jsonlite::base64_dec(dependency$meta[["hovercraft-more-config"]])),
      simplifyVector = TRUE
    )
    expect_identical(config$attributes$more, tip_attribute("more"))
    expect_identical(config$attributes$more_template, tip_attribute("more_template"))
    expect_identical(config$attributes$more_on, tip_attribute("more_on"))
    expect_identical(config$attributes$part, tip_contract()$attributes$part)
    expect_identical(config$attributes$held, tip_more_attribute("held"))
    expect_identical(config$attributes$held_template, tip_more_attribute("held-template"))
    expect_identical(config$attributes$label, tip_more_attribute("label"))
    expect_identical(config$events$hint, tip_contract()$events$hint$name)
    expect_identical(config$events$tip, tip_contract()$events$tip$name)
    expect_identical(config$css, tip_names()$css)
    expect_identical(config$key, "m")
    expect_identical(config$labels$more, "More")
    expect_identical(config$labels$less, "Less")
  })

  it("rely on the engine's `more` attributes, which it watches and draws again", {
    engine <- paste(readLines(system.file("engine", "www", "tip.js", package = "hovercraft")), collapse = "\n")
    expect_true(grepl("function keepSpot(", engine, fixed = TRUE))
    expect_true(grepl("more_on", paste(names(tip_contract()$attributes), collapse = " "), fixed = TRUE))
  })
})

describe("the long form's attributes", {
  it("are named after the engine's prefix", {
    expect_identical(tip_more_attribute("held"), paste0("data-", tip_names()$prefix, "-more-held"))
    expect_identical(tip_more_key(), "m")
    expect_identical(tip_keyshortcuts(TRUE, TRUE), "P M")
    expect_identical(tip_keyshortcuts(FALSE, TRUE), "M")
    expect_identical(tip_keyshortcuts(TRUE, FALSE), "P")
    expect_null(tip_keyshortcuts(FALSE, FALSE))
  })

  it("write the marks of a click and of a button that pins, for the script to read", {
    click <- tip_more_values("click", text = "Long")
    expect_named(click, c(tip_more_attribute("held"), tip_more_attribute("click")))
    pinned <- tip_more_values("button", text = "Long", pin = TRUE)
    expect_named(pinned, c(tip_more_attribute("held"), tip_more_attribute("pin")))
    # They are read by name from the dependency's config, not typed in the script.
    config <- jsonlite::fromJSON(
      rawToChar(jsonlite::base64_dec(tip_more_dependency()$meta[["hovercraft-more-config"]])),
      simplifyVector = TRUE
    )
    expect_identical(config$attributes$click, tip_more_attribute("click"))
    expect_identical(config$attributes$pin_too, tip_more_attribute("pin"))
  })

  it("hold the template of a long form behind the button, or the engine's attributes for the other moments", {
    behind <- tip_more_values("button", template = "more_abc", label = c(more = "Details"))
    expect_named(behind, c(tip_more_attribute("held-template"), tip_more_attribute("label")))
    hover <- tip_more_values("hover", text = "Long")
    expect_identical(hover[[tip_attribute("more")]], "Long")
    expect_identical(hover[[tip_attribute("more_on")]], "hover")
    expect_identical(tip_more_values("pin", template = "more_abc")[[tip_attribute("more_template")]], "more_abc")
  })

  it("are all taken off a tag that is tipped again", {
    tag <- tip_on(htmltools::tags$button("Run"), "Short", more = tip_more("Long", label = "Details"))
    again <- tip_on(tag, "Short only")
    expect_false(any(grepl("-more", names(again$attribs), fixed = TRUE)))
    state <- htmltools::tagAppendAttributes(tag, !!tip_more_attribute("state") := "long")
    expect_false(tip_more_attribute("state") %in% names(tip_on(state, "Short only")$attribs))
  })
})

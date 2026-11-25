describe("the keys of the page's words", {
  it("are the engine's, read from its rules, then hovercraft's own, in one list", {
    defaults <- tip_label_defaults()
    engine_keys <- names(tip_rules()$labels$defaults)
    expect_identical(names(defaults), c(engine_keys, "less", "pin"))
    expect_false(anyDuplicated(names(defaults)) > 0L)
    expect_identical(defaults[["pin"]], "Pin this tip")
    expect_identical(defaults[["less"]], "Less")
    expect_identical(defaults[["close"]], tip_rules()$labels$defaults$close)
  })

  it("give the pin and More scripts the same built-in words as the list", {
    expect_identical(dep_config(tip_pin_dependency(), "hovercraft-pin-config")$labels$pin, "Pin this tip")
    more <- dep_config(tip_more_dependency(), "hovercraft-more-config")$labels
    expect_identical(more$more, tip_label_defaults()[["more"]])
    expect_identical(more$less, tip_label_defaults()[["less"]])
    runtime <- dep_config(tip_runtime_dependency())
    expect_identical(unlist(runtime$labels), tip_label_defaults())
    expect_identical(runtime$attributes$labels, tip_labels_attribute())
  })

  it("name every key in the help text", {
    help <- tip_label_help()
    for (key in names(tip_label_defaults())) {
      expect_true(grepl(paste0("`", key, "`"), help, fixed = TRUE), info = key)
    }
  })

  it("write the attribute after the engine's prefix", {
    expect_identical(tip_labels_attribute(), paste0("data-", tip_names()$prefix, "-labels"))
  })
})

describe("tip_check_labels()", {
  it("gives NULL for none", {
    expect_null(tip_check_labels(NULL))
  })

  it("takes a named character vector or a list of strings, and keeps the words as written", {
    expect_identical(
      tip_check_labels(c(close = "Schliessen", pinned = "{label}: angeheftet")),
      list(close = "Schliessen", pinned = "{label}: angeheftet")
    )
    expect_identical(tip_check_labels(list(copy = "Kopieren")), list(copy = "Kopieren"))
  })

  it("takes every key of the list, hovercraft's own too", {
    all <- stats::setNames(paste("word", seq_along(tip_label_defaults())), names(tip_label_defaults()))
    expect_identical(tip_check_labels(all), as.list(all))
  })

  it("stops on a value that is not a named, non-empty, unique set of words", {
    bad <- list(
      "one word" = "Close",
      "no names" = c("a", "b"),
      "an NA" = c(close = NA_character_),
      "an empty word" = c(close = ""),
      "a number" = c(close = 1),
      "a repeated name" = c(close = "a", close = "b"),
      "an empty list" = list(),
      "a list of numbers" = list(close = 1)
    )
    for (what in names(bad)) {
      expect_error(tip_check_labels(bad[[what]]), class = "hovercraft_error_argument", info = what)
    }
  })

  it("stops on a key that is not a label, names the valid keys and guesses a misspelt one", {
    error <- expect_error(tip_check_labels(c(clsoe = "x", other = "y")), class = "hovercraft_error_labels")
    message <- conditionMessage(error)
    expect_match(message, "clsoe", fixed = TRUE)
    expect_match(message, "other", fixed = TRUE)
    expect_match(message, "Did you mean \"close\" for \"clsoe\"", fixed = TRUE)
    expect_false(grepl("for \"other\"", message, fixed = TRUE))
    for (key in names(tip_label_defaults())) {
      expect_match(message, paste0("\"", key, "\""), fixed = TRUE)
    }
    expect_s3_class(error, "hovercraft_error")
  })
})

describe("tip_on(labels = )", {
  tipped <- function(...) tip_on(htmltools::tags$button("a"), "A tip", ...)

  it("writes the words as JSON on the tag, and nothing without them", {
    tag <- tipped(labels = c(close = "Schliessen", more_pinned = "Mehr, wenn angeheftet"))
    expect_identical(
      jsonlite::fromJSON(htmltools::tagGetAttribute(tag, tip_labels_attribute()), simplifyVector = FALSE),
      list(close = "Schliessen", more_pinned = "Mehr, wenn angeheftet")
    )
    expect_null(htmltools::tagGetAttribute(tipped(), tip_labels_attribute()))
  })

  it("leaves a {label} placeholder as written", {
    tag <- tipped(labels = c(pinned = "{label}: angeheftet"))
    expect_match(htmltools::tagGetAttribute(tag, tip_labels_attribute()), "{label}: angeheftet", fixed = TRUE)
  })

  it("needs no pin and keeps the runtime in the dependencies", {
    tag <- tipped(labels = c(copy = "Kopieren"))
    names <- vapply(htmltools::findDependencies(tag), function(d) d$name, "")
    expect_true("hovercraft-runtime" %in% names)
    expect_false("hovercraft-pin" %in% names)
  })

  it("stops on a key that is not a label, and on words that are not words", {
    expect_error(tipped(labels = c(nope = "x")), class = "hovercraft_error_labels")
    expect_error(tipped(labels = "x"), class = "hovercraft_error_argument")
  })

  it("is replaced with the tip when the tag is tipped again", {
    first <- tipped(labels = c(close = "Schliessen"))
    again <- tip_on(first, "Another tip")
    expect_null(htmltools::tagGetAttribute(again, tip_labels_attribute()))
    changed <- tip_on(first, "Another tip", labels = c(copy = "Kopieren"))
    expect_identical(
      jsonlite::fromJSON(htmltools::tagGetAttribute(changed, tip_labels_attribute()), simplifyVector = FALSE),
      list(copy = "Kopieren")
    )
  })

  it("goes with the tip when the tip is taken away", {
    first <- tipped(labels = c(close = "Schliessen"))
    gone <- tip_on(first, NA_character_)
    expect_null(htmltools::tagGetAttribute(gone, tip_labels_attribute()))
  })
})

describe("use_hovercraft(labels = )", {
  ops <- function(deps) {
    found <- Filter(function(d) startsWith(d$name, "hovercraft-labels-"), deps)
    lapply(found, function(d) jsonlite::fromJSON(dep_ops(d), simplifyVector = FALSE))
  }

  it("adds one dependency of data, before the runtime, and none without labels", {
    deps <- use_hovercraft(labels = c(copy = "Kopieren", close = "Schliessen"))
    names <- vapply(deps, function(d) d$name, "")
    expect_identical(sum(startsWith(names, "hovercraft-labels-")), 1L)
    expect_identical(names[[length(names)]], "hovercraft-runtime")
    expect_true(which(startsWith(names, "hovercraft-labels-")) < length(names))
    expect_identical(
      ops(deps)[[1]],
      list(list(op = "labels", labels = list(copy = "Kopieren", close = "Schliessen")))
    )
    expect_length(ops(use_hovercraft()), 0L)
    expect_length(ops(use_hovercraft(labels = NULL)), 0L)
  })

  it("names a dependency by its words, so other words are another dependency", {
    a <- use_hovercraft(labels = c(copy = "Kopieren"))
    b <- use_hovercraft(labels = c(copy = "Copier"))
    same <- use_hovercraft(labels = c(copy = "Kopieren"))
    name <- function(deps) Filter(function(n) startsWith(n, "hovercraft-labels-"), vapply(deps, function(d) d$name, ""))
    expect_false(identical(name(a), name(b)))
    expect_identical(name(a), name(same))
  })

  it("carries no inline script or style, only data in a meta", {
    dep <- Filter(function(d) startsWith(d$name, "hovercraft-labels-"), use_hovercraft(labels = c(copy = "x")))[[1]]
    expect_null(dep$script)
    expect_null(dep$head)
    expect_false(is.null(dep$meta[["hovercraft-ops"]]))
  })

  it("stops on a key that is not a label", {
    expect_error(use_hovercraft(labels = c(nope = "x")), class = "hovercraft_error_labels")
    expect_error(use_hovercraft(labels = "x"), class = "hovercraft_error_argument")
  })
})

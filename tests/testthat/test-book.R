# hovercraft_book() and hovercraft book (inst/book). The book is an app folder, not package code: its files are loaded
# into an environment, as Shiny loads them, and its parts are tested one by one; the whole app runs in
# test-book-browser.R.

# Building the page runs the book's code, whose tip_define() calls register templates in this R process; forget them
# when this file ends, so a later test file starts with none.
withr::defer(tip_reset())

# The book's files, loaded as Shiny loads them: every file in R/, in order, into one environment.
load_book <- function() {
  folder <- system.file("book", package = "hovercraft")
  env <- new.env(parent = globalenv())
  for (file in sort(list.files(file.path(folder, "R"), pattern = "[.]R$", full.names = TRUE))) {
    sys.source(file, envir = env)
  }
  env
}

# A section's own function or value, such as `plate_pins_ui` or `pins_covers`, found by its id.
section_get <- function(book, id, pattern) get(sprintf(pattern, id), envir = book)

# The choices a section offers, from its own UI: list(input id = its values), one entry per choice set.
section_choices <- function(book, id) {
  ui <- section_get(book, id, "plate_%s_ui")
  html <- as.character(htmltools::renderTags(if (length(formals(ui))) ui(list()) else ui())$html)
  sets <- regmatches(
    html,
    gregexpr("<fieldset id=\"[^\"]+\" class=\"choice shiny-input-radiogroup\">.*?</fieldset>", html)
  )
  sets <- sets[[1]]
  stats::setNames(
    lapply(sets, function(set) {
      sub("value=\"([^\"]*)\"", "\\1", regmatches(set, gregexpr("value=\"[^\"]*\"", set))[[1]])
    }),
    sub("^<fieldset id=\"([^\"]+)\".*$", "\\1", sets)
  )
}

describe("hovercraft_book()", {
  it("checks its arguments, with the package's own error class", {
    expect_error(hovercraft_book(section = "nope", run = FALSE), class = "hovercraft_error_argument")
    expect_error(hovercraft_book(section = c("start", "pins"), run = FALSE), class = "hovercraft_error_argument")
    expect_error(hovercraft_book(section = 1, run = FALSE), class = "hovercraft_error_argument")
    expect_error(hovercraft_book(checklist = NA, run = FALSE), class = "hovercraft_error_argument")
    expect_error(hovercraft_book(run = "no"), class = "hovercraft_error_argument")
    expect_error(hovercraft_book("pins", TRUE), class = "hovercraft_error_argument")
    expect_error(hovercraft_book(chekclist = TRUE, run = FALSE), "must be empty", class = "hovercraft_error_argument")
  })

  it("returns the app with run = FALSE, and puts the section and the checklist in the first page's address", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("bslib")
    app <- hovercraft_book(section = "pins", checklist = TRUE, run = FALSE)
    expect_s3_class(app, "shiny.appobj")
    # The page request is passed on with the choices in its address; the real handler is swapped for one that
    # returns the address it was given.
    environment(app$httpHandler)$handler <- function(request) request$QUERY_STRING
    expect_identical(app$httpHandler(list(QUERY_STRING = "")), "?section=pins&checklist=1")
    # An address that names its own section keeps it.
    expect_identical(app$httpHandler(list(QUERY_STRING = "?section=more")), "?section=more&checklist=1")
    plain <- hovercraft_book(run = FALSE)
    environment(plain$httpHandler)$handler <- function(request) request$QUERY_STRING
    expect_identical(plain$httpHandler(list(QUERY_STRING = "")), "")
  })

  it("asks R whether the session is interactive: one TRUE or FALSE", {
    expect_identical(book_interactive(), interactive())
    expect_true(checkmate::test_flag(book_interactive()))
  })

  it("runs the app only in an interactive session", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("bslib")
    local_mocked_bindings(book_interactive = function() FALSE)
    expect_error(hovercraft_book(), "interactive R session", class = "hovercraft_error_book")
    local_mocked_bindings(book_interactive = function() TRUE)
    local_mocked_bindings(runApp = function(appDir, ...) inherits(appDir, "shiny.appobj"), .package = "shiny")
    expect_true(hovercraft_book())
  })
})

describe("the book's files", {
  book <- load_book()

  it("all parse, and the app's sections are the ones hovercraft_book() takes, each in a part", {
    folder <- system.file("book", package = "hovercraft")
    files <- c(file.path(folder, "app.R"), list.files(file.path(folder, "R"), full.names = TRUE))
    for (file in files) {
      expect_no_error(parse(file, keep.source = FALSE))
    }
    expect_identical(book$book_sections$id, book_section_ids)
    expect_true(all(book$book_sections$part %in% book$book_parts$n))
    expect_true(all(book$book_parts$n %in% book$book_sections$part))
    expect_false(anyDuplicated(book$book_sections$n) > 0)
  })

  it("has the five functions of a section, and 3 to 5 test steps, for every section", {
    for (id in book$book_sections$id) {
      for (pattern in c("plate_%s_ui", "plate_%s_server", "%s_steps")) {
        expect_true(is.function(section_get(book, id, pattern)), info = sprintf(pattern, id))
      }
      expect_type(section_get(book, id, "%s_covers"), "character")
      expect_true(length(section_get(book, id, "%s_steps")()) %in% 3:5, info = id)
    }
  })

  it("shows every export and every argument of the package in some section", {
    covers <- unlist(lapply(book$book_sections$id, function(id) section_get(book, id, "%s_covers")))
    exports <- getNamespaceExports("hovercraft")
    needed <- unlist(lapply(exports, function(name) {
      args <- setdiff(names(formals(getExportedValue("hovercraft", name))), "...")
      if (length(args)) paste0(name, "(", args, ")") else name
    }))
    # A feature added to the package and not to the book fails here: add it to a section and its `<id>_covers`.
    expect_identical(setdiff(needed, covers), character())
    # Each cover names a real export, and an argument it has (or one its `...` takes, such as tip_update(text)).
    fn <- sub("\\(.*$", "", covers)
    expect_identical(setdiff(fn, exports), character())
    dotted <- vapply(fn, function(name) "..." %in% names(formals(getExportedValue("hovercraft", name))), TRUE)
    expect_identical(setdiff(covers[!dotted], needed), character())
    # And a section covers only what its own file shows: each function it lists is in that file.
    folder <- system.file("book", "R", package = "hovercraft")
    for (id in book$book_sections$id) {
      text <- paste(readLines(file.path(folder, paste0("plate-", id, ".R"))), collapse = "\n")
      for (name in unique(sub("\\(.*$", "", section_get(book, id, "%s_covers")))) {
        expect_true(grepl(name, text, fixed = TRUE), info = paste(id, name))
      }
    }
  })
})

describe("the book's page", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  book <- load_book()
  page <- as.character(htmltools::renderTags(book$book_page(book$book_query(list())))$html)

  it("has one of each id, headings in order and no inline script or style", {
    ids <- regmatches(page, gregexpr(" id=\"[^\"]+\"", page))[[1]]
    expect_identical(ids[duplicated(ids)], character())
    levels <- as.integer(regmatches(page, gregexpr("(?<=<h)[1-6]", page, perl = TRUE))[[1]])
    expect_identical(levels[[1]], 1L)
    expect_true(all(diff(levels) <= 1L))
    scripts <- regmatches(page, gregexpr("<script[^>]*>", page))[[1]]
    expect_true(all(grepl("src=|type=\"application/json\"", scripts)))
    expect_false(grepl("<style", page, fixed = TRUE))
    expect_false(grepl(" style=\"", page, fixed = TRUE))
  })

  it("has a section for each id, a heading for each part, and a search entry for each section", {
    for (id in book$book_sections$id) {
      expect_match(page, paste0("<section class=\"plate[^\"]*\" id=\"", id, "\""))
      expect_match(page, paste0("data-plate=\"", id, "\" data-search=\"[^\"]+\""))
    }
    for (title in book$book_parts$title) {
      expect_match(page, paste0("<span>", title, "</span>\n?\\s*</h2>"))
    }
    # A search finds a section by a function or an argument it shows.
    expect_match(page, "data-plate=\"output\" data-search=\"[^\"]*keep_alive", perl = TRUE)
  })

  it("takes its starting settings from the address, and only values it offers", {
    query <- book$book_query(list(QUERY_STRING = "?section=pins&checklist=1&preset=editorial&motion=slide"))
    expect_identical(
      query[c("section", "checklist", "preset", "motion")],
      list(
        section = "pins",
        checklist = TRUE,
        preset = "editorial",
        motion = "slide"
      )
    )
    odd <- book$book_query(list(QUERY_STRING = "?section=%3Cscript%3E&checklist=yes&preset=x"))
    expect_identical(odd[c("section", "checklist", "preset")], list(section = "", checklist = FALSE, preset = "sober"))
  })
})

describe("the book's server", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("reactable")
  book <- load_book()
  page <- as.character(htmltools::renderTags(book$book_page(book$book_query(list())))$html)
  html <- function(x) if (is.null(x)) "" else paste(x$html, collapse = "\n")

  it("draws every section's figure for every combination of its choices, with no error", {
    # testServer runs its code in an environment of its own: the figures are kept in this one.
    seen <- new.env()
    seen$drawn <- character()
    shiny::testServer(book$book_server, {
      session$setInputs(page_mode = "light", book_ready = 1)
      for (id in book$book_sections$id) {
        choices <- section_choices(book, id)
        grid <- if (length(choices)) expand.grid(choices, stringsAsFactors = FALSE) else data.frame(x = 1)[, 0]
        for (i in seq_len(max(1L, nrow(grid)))) {
          if (length(choices)) {
            do.call(session$setInputs, as.list(grid[i, , drop = FALSE]))
          }
          figure <- html(output[[paste0(id, "_figure")]])
          notes <- html(output[[paste0(id, "_notes")]])
          what <- paste(id, if (length(choices)) paste(grid[i, ], collapse = " / "))
          expect_no_match(notes, "is-error", fixed = TRUE, info = what)
          # The one section whose figure warns on purpose: colours that fail the contrast check.
          if (id != "colours") {
            expect_no_match(notes, "Warning:", fixed = TRUE, info = what)
          }
          if (i == 1L) seen$drawn <- c(seen$drawn, figure)
        }
      }
      # The hint, pin and copy inputs a reader sends, and a hand tick, reach the observers.
      session$setInputs(
        visits_hint = list(state = "shown", by = "reader"),
        hero_copy = list(ok = TRUE, by = "reader"),
        col_wait_min_tip = list(state = "pinned", by = "reader"),
        region_north_tip = list(state = "pinned", by = "reader"),
        onboarding_hint = list(state = "shown", by = "host", via = "command"),
        srv_show = 1,
        book_tick = list(plate = "tables", i = 0, on = TRUE)
      )
    })
    # Every test step's target is in the page or in a figure the server draws.
    all <- paste(c(page, seen$drawn), collapse = "\n")
    for (id in book$book_sections$id) {
      for (target in vapply(section_get(book, id, "%s_steps")(), function(x) x$target, "")) {
        # The first part of each selector: an element id, or a tip id in the engine's attribute.
        first <- sub(" .*$", "", target)
        found <- if (startsWith(first, "#")) {
          grepl(paste0(" id=\"", substring(first, 2), "\""), all, fixed = TRUE)
        } else {
          grepl(sub("^\\[(.*)\\]$", "\\1", first), all, fixed = TRUE)
        }
        expect_true(found, info = paste(id, target))
      }
    }
  })
})

describe("the code each section shows", {
  book <- load_book()
  exports <- book$book_exports()

  it("runs the title's tip from the code the margin shows", {
    result <- book$run_code(book$start_hero_code(), exports)
    expect_null(result$error)
    expect_identical(result$warnings, character())
    hero <- as.character(htmltools::renderTags(result$ui)$html)
    expect_match(hero, paste0(tip_names()$attribute, "-id=\"hero\""), fixed = TRUE)
    expect_match(hero, paste0(tip_names()$attribute, "-preset=\"editorial\""), fixed = TRUE)
  })

  it("puts each choice in the code, and the code makes the figure that is on screen", {
    attribute <- tip_names()$attribute
    code <- book$placement_code(list(side = "top"))
    expect_match(code, "placement = \"top\"", fixed = TRUE)
    html <- as.character(htmltools::renderTags(book$run_code(code, exports)$ui)$html)
    expect_match(html, paste0(attribute, "-placement=\"top\""), fixed = TRUE)
    pins <- book$pins_code(list(copy = "off"))
    expect_no_match(pins, "copy = TRUE", fixed = TRUE)
    expect_match(book$pins_code(list(copy = "on")), "copy = TRUE", fixed = TRUE)
  })

  it("gives the default code for a value the section does not offer", {
    hostile <- "x\"); system(\"id\"); \""
    for (id in book$book_sections$id) {
      make <- get0(paste0(id, "_code"), envir = book, mode = "function")
      if (is.null(make) || !identical(names(formals(make)), "settings")) {
        next
      }
      # Every name a choice could have, as the section's server passes it: the input id without the section's word.
      names <- c(sub(paste0("^", id, "_"), "", names(section_choices(book, id))), "pin", "more_on")
      settings <- stats::setNames(rep(list(hostile), length(names)), names)
      expect_identical(make(settings), make(list()), info = id)
    }
    expect_identical(book$looks_page_code(hostile, hostile), book$looks_page_code("sober", "fade"))
    expect_identical(book$output_server_code(hostile), book$output_server_code("plot"))
    expect_identical(book$disabled_server_code(hostile), book$disabled_server_code("loading"))
  })

  it("writes made-up data as code that gives the same data back", {
    for (name in c("book_queues", "book_columns", "book_regions", "book_branches")) {
      data <- get(name, envir = book)
      env <- new.env()
      eval(parse(text = book$code_df("x", data)), env)
      expect_identical(env$x, data, info = name)
    }
  })

  it("puts an argument name R can't read bare between backticks", {
    code <- book$code_call("tip_fields", "Week 46" = "\"a\"", Owner = "\"b\"")
    expect_match(code, "`Week 46` = \"a\"", fixed = TRUE)
    expect_identical(names(as.list(parse(text = code)[[1]]))[-1], c("Week 46", "Owner"))
  })

  it("does not run code that leans on something outside the exports", {
    expect_match(book$run_code("tip_contract()", exports)$error, "tip_contract")
  })

  it("skips library(hovercraft), and keeps an error as a message", {
    result <- book$run_code("library(hovercraft)\nstop(\"no data\")", exports)
    expect_identical(result$error, "no data")
  })

  it("server code that parses, for every choice", {
    for (mode in book$output_modes) {
      expect_no_error(parse(text = book$output_server_code(mode)))
    }
    for (state in book$disabled_states) {
      expect_no_error(parse(text = book$disabled_server_code(state)))
    }
    expect_no_error(parse(text = book$server_server_code()))
  })

  it("is the render the server runs: the shown body draws a plot, errors before Retry and gives NULL for no rows", {
    skip_if_not_installed("shiny")
    run <- function(mode, retry = 0) {
      env <- new.env(parent = baseenv())
      env$weeks <- book$book_weeks
      env$open <- function() "Coast"
      env$input <- list(region_trend_retry = retry)
      grDevices::pdf(NULL)
      on.exit(grDevices::dev.off())
      eval(parse(text = paste(c("{", book$output_render_body(mode), "}"), collapse = "\n"))[[1]], env)
    }
    expect_length(run("plot"), length(book$book_weeks$Coast))
    expect_error(run("error"), "timed out")
    expect_length(run("error", retry = 1), length(book$book_weeks$Coast))
    expect_null(run("empty"))
    for (mode in book$output_modes) {
      expect_match(
        book$output_server_code(mode),
        paste(book$output_render_body(mode), collapse = "\n"),
        fixed = TRUE
      )
    }
  })
})

describe("the test steps and their proofs", {
  book <- load_book()

  it("tick on the reader's own event, and never on the page's or on a tag drawn again", {
    pinned <- book$on_pin("col_wait_min")
    expect_true(pinned$test(list(state = "pinned", by = "reader"), list()))
    expect_false(pinned$test(list(state = "pinned", by = "host"), list()))
    expect_false(pinned$test(list(state = "closed", by = "reader"), list()))
    escape <- book$on_hint("wait_term", state = "hidden", reason = "escape")
    expect_true(escape$test(list(state = "hidden", reason = "escape", by = "reader"), list()))
    expect_false(escape$test(list(state = "hidden", reason = "outside", by = "reader"), list()))
    expect_false(escape$test(list(state = "hidden", reason = "gone", by = "data"), list()))
    # The hint of the From the server plate is closed by the host, Escape or another tip: not by the pointer leaving.
    closed <- book$server_steps()[[2]]$proof
    expect_true(closed$test(list(state = "hidden", reason = "host", by = "host"), list()))
    expect_false(closed$test(list(state = "hidden", reason = "leave", by = "reader"), list()))
    # The Disabled plate's first step needs the reason in the hint's words; its third, the button's own tip.
    reason <- book$disabled_steps()[[1]]$proof
    own <- book$disabled_steps()[[3]]$proof
    said <- list(state = "shown", by = "reader", text = "Not available: the data is still loading.")
    plain <- list(state = "shown", by = "reader", text = "Saves the visits of week 46 as a CSV file.")
    expect_true(reason$test(said, list(export_csv_tip_hint = said)))
    expect_false(reason$test(plain, list(export_csv_tip_hint = plain)))
    expect_true(own$test(plain, list(export_csv_tip_hint = plain)))
    expect_false(own$test(said, list(export_csv_tip_hint = said)))
    dark <- book$on_hint("visits", when = function(input) identical(input$page_mode, "dark"))
    expect_false(dark$test(list(state = "shown", by = "reader"), list(page_mode = "light")))
    expect_true(dark$test(list(state = "shown", by = "reader"), list(page_mode = "dark")))
  })

  it("are ticked by the server from the inputs it receives; by hand only when they have no proof", {
    skip_if_not_installed("shiny")
    server <- function(input, output, session) {
      session$userData$pins <- book$steps_server(input, session, "pins", book$pins_steps())
    }
    shiny::testServer(server, {
      ticks <- session$userData$pins
      session$setInputs(col_wait_min_tip = list(state = "pinned", by = "host"))
      expect_identical(ticks$done(), integer())
      session$setInputs(col_wait_min_tip = list(state = "pinned", by = "reader"))
      expect_identical(ticks$done(), 1L)
      session$setInputs(col_region_copy = list(ok = TRUE, by = "reader"))
      expect_identical(ticks$done(), c(1L, 3L))
      # Every Pins step has a proof: a click on its box does not tick it, but unticks it so it can be tried again.
      session$setInputs(book_tick = list(plate = "pins", i = 0, on = TRUE))
      expect_identical(ticks$done(), c(1L, 3L))
      session$setInputs(book_tick = list(plate = "pins", i = 1, on = FALSE))
      expect_identical(ticks$done(), 3L)
      # Another plate's tick, or one past the end, changes nothing here.
      session$setInputs(book_tick = list(plate = "more", i = 2, on = TRUE))
      session$setInputs(book_tick = list(plate = "pins", i = 9, on = TRUE))
      expect_identical(ticks$done(), 3L)
    })
    tables <- function(input, output, session) {
      session$userData$tables <- book$steps_server(input, session, "tables", book$tables_steps())
    }
    shiny::testServer(tables, {
      session$setInputs(book_tick = list(plate = "tables", i = 1, on = TRUE))
      expect_identical(session$userData$tables$done(), 1L)
      # A pinnable row sends its pin under the table's id: the reader's own pin ticks the pin step.
      pin <- function(by) stats::setNames(list(list(state = "pinned", by = by)), "branches/row/2_tip")
      do.call(session$setInputs, pin("host"))
      expect_identical(session$userData$tables$done(), 1L)
      do.call(session$setInputs, pin("reader"))
      expect_identical(session$userData$tables$done(), c(1L, 3L))
    })
  })

  it("say what proved them", {
    expect_identical(
      book$proof_says("col_wait_min_tip", list(state = "closed", reason = "escape", by = "reader")),
      "input$col_wait_min_tip, state \"closed\", reason \"escape\""
    )
    expect_identical(book$proof_says("book:kept", "the server kept the plot"), "the server kept the plot")
  })
})

describe("the words of a step", {
  book <- load_book()

  it("turn backticks into code and stars into italics, escape the rest, and add no space before punctuation", {
    expect_identical(
      as.character(book$inline_code("Pin *North*: `input$x_tip` <arrives>.")),
      "Pin <i>North</i>: <code>input$x_tip</code> &lt;arrives&gt;."
    )
  })
})

describe("the inputs log", {
  book <- load_book()

  it("prints a value as R would", {
    expect_identical(
      book$r_value(list(state = "pinned", ok = TRUE, n = 2, reason = NULL)),
      "list(state = \"pinned\", ok = TRUE, n = 2, reason = NULL)"
    )
    expect_identical(book$r_value(NULL), "NULL")
    expect_identical(book$r_value(list()), "list()")
    expect_identical(book$r_value(NA), "NA")
  })

  it("names an input as R reads it, with [[ ]] for a name that is not syntactic", {
    expect_identical(book$input_ref("visits_hint"), "input$visits_hint")
    expect_identical(book$input_ref("table_pins_rows/row/1_tip"), "input[[\"table_pins_rows/row/1_tip\"]]")
    expect_identical(
      book$proof_says("table_pins_rows/row/2_tip", list(state = "pinned", by = "reader")),
      "input[[\"table_pins_rows/row/2_tip\"]], state \"pinned\""
    )
  })
})

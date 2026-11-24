# tip_studio() and hovercraft studio (inst/studio). The studio is an app folder, not package code: its files are loaded
# into an environment, as Shiny loads them, and its parts are tested one by one; the whole app runs in
# test-studio-browser.R.

# The studio's files, loaded as Shiny loads them: every file in R/, in order, into one environment.
load_studio <- function() {
  folder <- system.file("studio", package = "hovercraft")
  env <- new.env(parent = globalenv())
  for (file in sort(list.files(file.path(folder, "R"), pattern = "[.]R$", full.names = TRUE))) {
    sys.source(file, envir = env)
  }
  env
}

describe("tip_studio()", {
  it("checks its arguments, with the package's own error class", {
    expect_error(tip_studio(plate = "nope", run = FALSE), class = "hovercraft_error_argument")
    expect_error(tip_studio(plate = c("start", "pins"), run = FALSE), class = "hovercraft_error_argument")
    expect_error(tip_studio(plate = 1, run = FALSE), class = "hovercraft_error_argument")
    expect_error(tip_studio(guide = NA, run = FALSE), class = "hovercraft_error_argument")
    expect_error(tip_studio(run = "no"), class = "hovercraft_error_argument")
    expect_error(tip_studio("pins", TRUE), class = "hovercraft_error_argument")
    expect_error(tip_studio(gide = TRUE, run = FALSE), "must be empty", class = "hovercraft_error_argument")
  })

  it("returns the app with run = FALSE, and puts the plate and the guide in the first page's address", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("bslib")
    app <- tip_studio(plate = "pins", guide = TRUE, run = FALSE)
    expect_s3_class(app, "shiny.appobj")
    # The page request is passed on with the choices in its address; the real handler is swapped for one that
    # returns the address it was given.
    environment(app$httpHandler)$handler <- function(request) request$QUERY_STRING
    expect_identical(app$httpHandler(list(QUERY_STRING = "")), "?plate=pins&guide=1")
    # An address that names its own plate keeps it.
    expect_identical(app$httpHandler(list(QUERY_STRING = "?plate=more")), "?plate=more&guide=1")
    plain <- tip_studio(run = FALSE)
    environment(plain$httpHandler)$handler <- function(request) request$QUERY_STRING
    expect_identical(plain$httpHandler(list(QUERY_STRING = "")), "")
  })

  it("runs the app only in an interactive session", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("bslib")
    local_mocked_bindings(tip_studio_interactive = function() FALSE)
    expect_error(tip_studio(), "interactive R session", class = "hovercraft_error_studio")
    local_mocked_bindings(tip_studio_interactive = function() TRUE)
    local_mocked_bindings(runApp = function(appDir, ...) inherits(appDir, "shiny.appobj"), .package = "shiny")
    expect_true(tip_studio())
  })
})

describe("the studio's files", {
  it("all parse, and the app's plates are the ones tip_studio() takes", {
    folder <- system.file("studio", package = "hovercraft")
    files <- c(file.path(folder, "app.R"), list.files(file.path(folder, "R"), full.names = TRUE))
    for (file in files) {
      expect_no_error(parse(file, keep.source = FALSE))
    }
    expect_identical(load_studio()$studio_plates$id, tip_studio_plates)
  })
})

describe("the studio's page", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  studio <- load_studio()
  page <- as.character(htmltools::renderTags(studio$studio_page(studio$studio_query(list())))$html)

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

  it("has a plate for each of the nine, and a target in the page for every test step", {
    for (id in studio$studio_plates$id) {
      expect_match(page, paste0("<section class=\"plate[^\"]*\" id=\"", id, "\""))
    }
    steps <- list(
      studio$start_steps(),
      studio$basics_steps(),
      studio$templates_steps(),
      studio$more_steps(),
      studio$pins_steps(),
      studio$disabled_steps(),
      studio$output_steps(),
      studio$tables_steps(),
      studio$server_steps()
    )
    # The figures the server draws into the page, from each plate's first code.
    exports <- studio$studio_exports()
    figures <- vapply(
      list(
        studio$start_figure_code(),
        studio$basics_code(list()),
        studio$templates_code(list()),
        studio$more_code(list()),
        studio$pins_code(list()),
        studio$disabled_code(),
        studio$output_ui_code(),
        studio$server_ui_code()
      ),
      function(code) as.character(htmltools::renderTags(studio$run_code(code, exports)$ui)$html),
      ""
    )
    drawn <- paste(c(page, figures), collapse = "\n")
    targets <- unlist(lapply(steps, function(s) vapply(s, function(x) x$target, "")))
    for (target in targets) {
      # The first part of each selector: an element id, or a tip id in the engine's attribute.
      first <- sub(" .*$", "", target)
      found <- if (startsWith(first, "#")) {
        grepl(paste0(" id=\"", substring(first, 2), "\""), drawn, fixed = TRUE)
      } else {
        grepl(sub("^\\[(.*)\\]$", "\\1", first), drawn, fixed = TRUE)
      }
      expect_true(found, info = target)
    }
  })
})

describe("the code each plate shows", {
  studio <- load_studio()
  exports <- studio$studio_exports()

  it("runs with the package's exports only, for every choice, with no error or warning", {
    skip_if_not_installed("DT")
    skip_if_not_installed("reactable")
    # Every combination of each plate's choices.
    each <- function(make, ...) {
      grid <- expand.grid(..., stringsAsFactors = FALSE)
      lapply(seq_len(nrow(grid)), function(i) make(as.list(grid[i, , drop = FALSE])))
    }
    cases <- c(
      each(
        function(x) paste0(studio$start_look_code(x$preset, x$motion), "\n\n", studio$start_figure_code()),
        preset = tip_presets()$name,
        motion = unname(studio$start_motions)
      ),
      each(
        studio$basics_code,
        marker = unname(studio$basics_markers),
        placement = unname(studio$basics_placements),
        look = unname(studio$basics_looks)
      ),
      each(studio$templates_code, record = unname(studio$templates_records), slots = unname(studio$templates_slots)),
      each(studio$more_code, more_on = unname(studio$more_on_choices), pin = unname(studio$more_pin_choices)),
      each(studio$pins_code, placement = unname(studio$pins_placements), copy = unname(studio$pins_copy)),
      list(studio$disabled_code(), studio$output_ui_code(), studio$server_ui_code()),
      each(studio$tables_code, widget = unname(studio$tables_widgets), parts = unname(studio$tables_parts))
    )
    for (code in cases) {
      result <- studio$run_code(code, exports)
      expect_null(result$error, info = code)
      expect_identical(result$warnings, character(), info = code)
      expect_gt(nchar(as.character(htmltools::renderTags(result$ui)$html)), 0L)
    }
  })

  it("puts each choice in the code, and the code makes the figure that is on screen", {
    attribute <- tip_names()$attribute
    code <- studio$basics_code(list(marker = "corner", placement = "top", look = "editorial"))
    expect_match(code, "placement = \"top\"", fixed = TRUE)
    expect_match(code, "style = \"editorial\"", fixed = TRUE)
    html <- as.character(htmltools::renderTags(studio$run_code(code, exports)$ui)$html)
    expect_match(html, paste0(attribute, "-placement=\"top\""), fixed = TRUE)
    expect_match(html, paste0(attribute, "-preset=\"editorial\""), fixed = TRUE)
    pins <- studio$pins_code(list(copy = "off"))
    expect_no_match(pins, "copy = TRUE", fixed = TRUE)
    expect_match(studio$pins_code(list(copy = "on")), "copy = TRUE", fixed = TRUE)
  })

  it("does not run code that leans on something outside the exports", {
    expect_match(studio$run_code("tip_contract()", exports)$error, "tip_contract")
  })

  it("skips library(hovercraft), and keeps an error as a message", {
    result <- studio$run_code("library(hovercraft)\nstop(\"no data\")", exports)
    expect_identical(result$error, "no data")
  })

  it("server code that parses, for every choice", {
    for (mode in studio$output_modes) {
      expect_no_error(parse(text = studio$output_server_code(mode)))
    }
    for (state in studio$disabled_states) {
      expect_no_error(parse(text = studio$disabled_server_code(state)))
    }
    expect_no_error(parse(text = studio$server_server_code()))
  })

  it("is the render the server runs: the shown body draws a plot, errors before Retry and gives NULL for no rows", {
    skip_if_not_installed("shiny")
    run <- function(mode, retry = 0) {
      env <- new.env(parent = baseenv())
      env$weeks <- studio$studio_weeks
      env$open <- function() "Coast"
      env$input <- list(region_trend_retry = retry)
      grDevices::pdf(NULL)
      on.exit(grDevices::dev.off())
      eval(parse(text = paste(c("{", studio$output_render_body(mode), "}"), collapse = "\n"))[[1]], env)
    }
    expect_length(run("plot"), length(studio$studio_weeks$Coast))
    expect_error(run("error"), "timed out")
    expect_length(run("error", retry = 1), length(studio$studio_weeks$Coast))
    expect_null(run("empty"))
    for (mode in studio$output_modes) {
      expect_match(
        studio$output_server_code(mode),
        paste(studio$output_render_body(mode), collapse = "\n"),
        fixed = TRUE
      )
    }
  })
})

describe("the test steps and their proofs", {
  studio <- load_studio()

  it("tick on the reader's own event, and never on the page's or on a tag drawn again", {
    pinned <- studio$on_pin("col_wait_min")
    expect_true(pinned$test(list(state = "pinned", by = "reader"), list()))
    expect_false(pinned$test(list(state = "pinned", by = "host"), list()))
    expect_false(pinned$test(list(state = "closed", by = "reader"), list()))
    escape <- studio$on_hint("wait_term", state = "hidden", reason = "escape")
    expect_true(escape$test(list(state = "hidden", reason = "escape", by = "reader"), list()))
    expect_false(escape$test(list(state = "hidden", reason = "outside", by = "reader"), list()))
    expect_false(escape$test(list(state = "hidden", reason = "gone", by = "data"), list()))
    # The hint of the From the server plate is closed by the host, Escape or another tip: not by the pointer leaving.
    closed <- studio$server_steps()[[2]]$proof
    expect_true(closed$test(list(state = "hidden", reason = "host", by = "host"), list()))
    expect_false(closed$test(list(state = "hidden", reason = "leave", by = "reader"), list()))
    # The Disabled plate's first step needs the reason in the hint's words; its third, the button's own tip.
    reason <- studio$disabled_steps()[[1]]$proof
    own <- studio$disabled_steps()[[3]]$proof
    said <- list(state = "shown", by = "reader", text = "Not available: the data is still loading.")
    plain <- list(state = "shown", by = "reader", text = "Saves the visits of week 46 as a CSV file.")
    expect_true(reason$test(said, list(export_csv_tip_hint = said)))
    expect_false(reason$test(plain, list(export_csv_tip_hint = plain)))
    expect_true(own$test(plain, list(export_csv_tip_hint = plain)))
    expect_false(own$test(said, list(export_csv_tip_hint = said)))
    dark <- studio$on_hint("visits", when = function(input) identical(input$page_mode, "dark"))
    expect_false(dark$test(list(state = "shown", by = "reader"), list(page_mode = "light")))
    expect_true(dark$test(list(state = "shown", by = "reader"), list(page_mode = "dark")))
  })

  it("are ticked by the server from the inputs it receives; by hand only when they have no proof", {
    skip_if_not_installed("shiny")
    server <- function(input, output, session) {
      session$userData$pins <- studio$steps_server(input, session, "pins", studio$pins_steps())
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
      session$setInputs(studio_tick = list(plate = "pins", i = 0, on = TRUE))
      expect_identical(ticks$done(), c(1L, 3L))
      session$setInputs(studio_tick = list(plate = "pins", i = 1, on = FALSE))
      expect_identical(ticks$done(), 3L)
      # Another plate's tick, or one past the end, changes nothing here.
      session$setInputs(studio_tick = list(plate = "more", i = 2, on = TRUE))
      session$setInputs(studio_tick = list(plate = "pins", i = 9, on = TRUE))
      expect_identical(ticks$done(), 3L)
    })
    tables <- function(input, output, session) {
      session$userData$tables <- studio$steps_server(input, session, "tables", studio$tables_steps())
    }
    shiny::testServer(tables, {
      session$setInputs(studio_tick = list(plate = "tables", i = 1, on = TRUE))
      expect_identical(session$userData$tables$done(), 1L)
    })
  })

  it("say what proved them", {
    expect_identical(
      studio$proof_says("col_wait_min_tip", list(state = "closed", reason = "escape", by = "reader")),
      "input$col_wait_min_tip, state \"closed\", reason \"escape\""
    )
    expect_identical(studio$proof_says("studio:kept", "the server kept the plot"), "the server kept the plot")
  })
})

describe("the words of a step", {
  studio <- load_studio()

  it("turn backticks into code and stars into italics, escape the rest, and add no space before punctuation", {
    expect_identical(
      as.character(studio$inline_code("Pin *North*: `input$x_tip` <arrives>.")),
      "Pin <i>North</i>: <code>input$x_tip</code> &lt;arrives&gt;."
    )
  })
})

describe("the inputs log", {
  studio <- load_studio()

  it("prints a value as R would", {
    expect_identical(
      studio$r_value(list(state = "pinned", ok = TRUE, n = 2, reason = NULL)),
      "list(state = \"pinned\", ok = TRUE, n = 2, reason = NULL)"
    )
    expect_identical(studio$r_value(NULL), "NULL")
    expect_identical(studio$r_value(list()), "list()")
    expect_identical(studio$r_value(NA), "NA")
  })
})

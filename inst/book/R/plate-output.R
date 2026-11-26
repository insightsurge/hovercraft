# Section 5.2, Live output: a Shiny plot inside a pinned tip, drawn by the server for the region under the pointer, kept
# alive when the tip closes and shared by three rows; and its loading, error-with-Retry and empty states.

output_modes <- c("a plot" = "plot", "a slow plot" = "slow", "an error" = "error", "no rows" = "empty")

output_keep_alives <- c("kept when it closes" = "true", "destroyed when it closes" = "false")

output_ui_code <- function(keep_alive = "true") {
  keep_alive <- if (identical(choice(keep_alive, output_keep_alives, "true"), "false")) "FALSE" else "TRUE"
  data <- code_df("regions", book_regions)
  define <- code_call(
    "tip_define",
    region = code_call(
      "tip_template",
      head = code_call("tip_title", code_str("{{ region }}")),
      body = code_call(
        "list",
        code_call(
          "tip_fields",
          Total = code_str("{{ total | number:0 }}"),
          "Week 46" = code_str("{{ latest | number:0 }}")
        ),
        code_call("tip_output", code_str("region_trend"), keep_alive = keep_alive)
      )
    )
  )
  rows <- code_call(
    "htmltools::tags$ul",
    class = code_str("regions"),
    code_lapply(
      "seq_len(nrow(regions))",
      "i",
      code_call(
        "tip_on",
        code_call(
          "htmltools::tags$li",
          code_call("htmltools::span", "regions$region[[i]]", class = code_str("r")),
          code_call("htmltools::span", "regions$latest[[i]]", class = code_str("v")),
          code_call("htmltools::span", "regions$change[[i]]", class = code_str("d")),
          tabindex = code_str("0")
        ),
        template = code_str("region"),
        record = "regions[i, ]",
        pin = "TRUE",
        id = "paste0(\"region_\", tolower(regions$region[[i]]))",
        events = code_chr(c("hint", "pin"))
      )
    )
  )
  paste(
    c(
      data,
      "",
      "# Once in the page's UI: the template, and the output it moves into whichever tip is open",
      define,
      code_call("shiny::plotOutput", code_str("region_trend"), height = code_str("150px")),
      "",
      rows
    ),
    collapse = "\n"
  )
}

# The body of the render for a mode: the lines inside renderPlot(), shown in the code note and run by the server as
# they are shown. It reads the Retry input, so a click on Retry runs it again.
output_render_body <- function(mode) {
  draw <- c(
    "  graphics::par(mar = c(2.2, 2.6, 1.6, 0.4), cex = 0.8)",
    "  graphics::barplot(weeks[[open()]], names.arg = 39:46, main = open(), col = \"#4f7aa3\", border = NA)"
  )
  c(
    "  input$region_trend_retry",
    switch(
      choice(mode, output_modes, "plot"),
      plot = draw,
      slow = c("  Sys.sleep(2)", draw),
      error = c(
        "  # Fails until Retry is pressed once",
        "  if (!isTRUE(input$region_trend_retry > 0)) stop(\"The visits query timed out after 10 s.\")",
        draw
      ),
      empty = "  NULL # no rows for this region: the tip says so"
    )
  )
}

# The server's code that says which region is open, from the pin and hint inputs (a hint never takes the plot away
# from a pinned tip). Shown in the code note, and run by the server as it is shown.
output_observer_code <- function() {
  weeks <- paste0("weeks <- ", do.call(code_call, c(list("list"), lapply(book_weeks, code_value), list(width = 0L))))
  paste(
    c(
      weeks,
      "",
      "# The region whose tip is open: the pin and hint inputs say which",
      "open <- shiny::reactiveVal(\"North\")",
      "pinned <- shiny::reactiveVal(NULL)",
      "for (region in regions$region) {",
      "  local({",
      "    region <- region",
      "    id <- paste0(\"region_\", tolower(region))",
      "    shiny::observeEvent(input[[paste0(id, \"_pin\")]], {",
      "      state <- input[[paste0(id, \"_pin\")]]$state",
      "      if (state == \"pinned\") {",
      "        pinned(region)",
      "        open(region)",
      "      } else if (state == \"closed\" && identical(pinned(), region)) {",
      "        pinned(NULL)",
      "      }",
      "    })",
      "    shiny::observeEvent(input[[paste0(id, \"_hint\")]], {",
      "      if (input[[paste0(id, \"_hint\")]]$state == \"shown\" && is.null(pinned())) open(region)",
      "    })",
      "  })",
      "}"
    ),
    collapse = "\n"
  )
}

# The server's whole code for a mode: the observers, then the render.
output_server_code <- function(mode) {
  paste(
    c(output_observer_code(), "output$region_trend <- shiny::renderPlot({", output_render_body(mode), "})"),
    collapse = "\n"
  )
}

output_covers <- c(
  "tip_define",
  "tip_template(head)",
  "tip_template(body)",
  "tip_title(text)",
  "tip_fields",
  "tip_output(output)",
  "tip_output(keep_alive)",
  "tip_on(tag)",
  "tip_on(template)",
  "tip_on(record)",
  "tip_on(pin)",
  "tip_on(id)",
  "tip_on(events)"
)

output_steps <- function() {
  list(
    step(
      "Pin *North*: the plot is there at once, sized before it draws.",
      "the tip jumps when the plot arrives.",
      tip_target("region_north"),
      on_pin("region_north")
    ),
    step(
      paste(
        "Close the tip and pin *North* again: the same plot is back, and the log says *kept*, not *drawn*. Choose",
        "*destroyed when it closes* and the plot is built again instead: no *kept*."
      ),
      "the plot is drawn again while it is kept, or a destroyed plot is still there.",
      tip_target("region_north"),
      proof("book:kept", function(value, input) TRUE)
    ),
    step(
      "Pin *Coast*: the one output is drawn again for Coast; title and bars agree.",
      "the tip shows North's bars.",
      tip_target("region_coast"),
      on_pin("region_coast")
    ),
    step(
      "Choose *an error*, pin a region, then press Retry in the tip: a message and Retry first, then the plot.",
      "an empty tip, or Retry that does nothing.",
      "#output_mode",
      proof("region_trend_retry", function(value, input) TRUE)
    ),
    step(
      "Choose *no rows*, then pin a region: the tip says there is nothing to show.",
      "empty axes or a blank box.",
      "#output_mode",
      proof("book:empty", function(value, input) TRUE)
    )
  )
}

plate_output_ui <- function() {
  plate(
    "output",
    lede = paste(
      "A pinned tip can hold a Shiny output: a plot or a table drawn by the server for the record under the pointer.",
      "The output is kept alive when the tip closes, and several rows share it."
    ),
    caption = "Visits a week by region, weeks 39 to 46. The plot is drawn by renderPlot() in the server.",
    steps = output_steps(),
    code = code_text(output_ui_code(), output_server_code("plot")),
    log_empty = "Nothing yet. Pin a region.",
    log_title = "Inputs sent, outputs drawn",
    wide = TRUE,
    controls = htmltools::tagList(
      "The server answers with",
      choice_set("output_mode", "Server answer", output_modes),
      "and the plot is",
      choice_set("output_keep_alive", "When the tip closes", output_keep_alives)
    )
  )
}

plate_output_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "output", output_steps())
  plate_code(
    input,
    output,
    session,
    "output",
    ui_code = shiny::reactive(output_ui_code(input$output_keep_alive)),
    server_code = shiny::reactive(output_server_code(input$output_mode))
  )
  # The shown code, run as it is shown: the observers from their text, then a render whose body is run from its
  # text. Around them, and outside the shown code, the book counts the plot's draws and tells the log what it drew.
  env <- run_server_code(output_observer_code(), input, session, regions = book_regions)
  seen <- new.env()
  seen$draws <- 0L
  seen$drawn_for <- NULL
  seen$closed <- list() # region -> the number of draws when its tip closed
  output$region_trend <- shiny::renderPlot({
    region <- env$open()
    mode <- choice(input$output_mode, output_modes, "plot")
    body <- parse(text = paste(c("{", output_render_body(mode), "}"), collapse = "\n"), keep.source = FALSE)[[1]]
    if (mode == "empty") {
      log_line(session, "output", "output$region_trend", paste("no rows for", region), "out")
      ticks$check("book:empty", paste0("the server had no rows for ", region))
    } else if (mode == "error" && !isTRUE(input$region_trend_retry > 0)) {
      log_line(session, "output", "output$region_trend", paste("error for", region), "out")
    } else {
      seen$draws <- seen$draws + 1L
      seen$drawn_for <- region
      log_line(session, "output", "output$region_trend", paste("drawn for", region), "out")
    }
    eval(body, env)
  })
  # "Kept": a region's tip pinned again after it closed, and a second later the plot has not been drawn since that tip
  # closed (not even by the hint that opens as the reader comes back) and is still the one drawn for that region.
  # Decided from the draws counted above, after Shiny has had time to draw.
  pending <- shiny::reactiveVal(NULL)
  for (region in book_regions$region) {
    local({
      region <- region
      name <- paste0("region_", tolower(region), "_pin")
      shiny::observeEvent(input[[name]], {
        value <- input[[name]]
        if (identical(value$state, "closed")) {
          seen$closed[[region]] <- seen$draws
        }
        if (identical(value$state, "pinned") && identical(value$by, "reader")) {
          if (!is.null(seen$closed[[region]])) {
            pending(list(region = region, draws = seen$closed[[region]], at = Sys.time()))
          }
          seen$closed[[region]] <- NULL
        }
      })
    })
  }
  shiny::observe({
    p <- pending()
    if (is.null(p)) {
      return()
    }
    wait <- 1.2 - as.numeric(difftime(Sys.time(), p$at, units = "secs"))
    if (wait > 0) {
      shiny::invalidateLater(ceiling(wait * 1000) + 50)
      return()
    }
    pending(NULL)
    kept <- seen$draws == p$draws &&
      identical(seen$drawn_for, p$region) &&
      identical(shiny::isolate(choice(input$output_mode, output_modes, "plot")), "plot") &&
      identical(shiny::isolate(choice(input$output_keep_alive, output_keep_alives, "true")), "true")
    if (kept) {
      log_line(session, "output", "output$region_trend", paste("kept for", p$region, "(not drawn again)"), "out")
      ticks$check("book:kept", paste0("the server kept the plot for ", p$region))
    }
  })
  ids <- paste0("region_", tolower(book_regions$region))
  log_inputs(input, session, "output", c(paste0(ids, "_pin"), "region_trend_retry"))
  ticks
}

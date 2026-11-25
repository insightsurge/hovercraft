# Shiny outputs inside a tip: tip_output(output, keep_alive = TRUE).
# R writes a plain block `{ type: "output", output: "<id>", keep_alive: true|false }` with no random or per-session
# ids, so tip markup stays cacheable with shiny::bindCache(). inst/shiny/shiny.js registers the "output" block through
# the engine's public block() door, moves the bound Shiny output into the open tip, refreshes Shiny's hidden/size
# state, and either parks it in a hidden holder (keep_alive = TRUE) or unbinds and destroys it (keep_alive = FALSE).

# Finds the Shiny output id inside a tag or wrapper (such as shinycssloaders::withSpinner(plotOutput("p"))). Prefers a
# tag carrying a Shiny output class, falling back to the first tag with a non-loader id.
tip_output_tag_id <- function(x) {
  tags <- tip_tags_in(x)
  is_output <- function(tag) {
    cls <- paste(htmltools::tagGetAttribute(tag, "class") %||% "", collapse = " ")
    grepl("shiny-[a-z0-9_-]*output|html-widget-output|gt_shiny", cls)
  }
  get_id <- function(tag) {
    id <- htmltools::tagGetAttribute(tag, "id")
    if (is.character(id) && length(id) == 1L && nzchar(id)) id else NULL
  }
  for (tag in tags) {
    if (is_output(tag)) {
      id <- get_id(tag)
      if (!is.null(id)) {
        return(id)
      }
    }
  }
  for (tag in tags) {
    id <- get_id(tag)
    if (!is.null(id) && !grepl("^(spinner|waiter|bones)-", id)) {
      return(id)
    }
  }
  NULL
}

# Extracts any Shiny output tags attached to tip_output() blocks in a list of templates, deduplicated by output id.
tip_output_tags <- function(templates) {
  seen <- character()
  out <- list()
  for (template in templates) {
    for (block in template) {
      tag <- attr(block, "hovercraft_output_tag")
      if (!is.null(tag) && !block$output %in% seen) {
        seen <- c(seen, block$output)
        out <- c(out, list(tag))
      }
    }
  }
  out
}

# Lists the Shiny output ids referenced by tip_output() blocks across a list of templates.
tip_output_ids <- function(templates) {
  out <- character()
  for (template in templates) {
    for (block in template) {
      if (identical(block$type, "output") && is.character(block$output)) {
        out <- c(out, block$output)
      }
    }
  }
  unique(out)
}

# The CSS class of the hidden parking container that holds outputs while their tip is closed.
tip_output_park_class <- function() paste0(sub("-tip$", "", tip_names()$css), "-output-park")

#' A live Shiny output inside a tip template
#'
#' `r lifecycle::badge("experimental")`
#' Puts any Shiny output — a plot, a `DT` or `reactable` table, a `gt` table, a chart or `uiOutput()` — inside a
#' [tip_template()]. Give `output` as the Shiny output id (with `ns()` inside a module, such as `ns("detail_plot")`),
#' or as the output tag itself (such as `shiny::plotOutput(ns("detail_plot"))` or one wrapped in a loader like
#' `shinycssloaders::withSpinner()`). It needs Shiny: on a page without a server (R Markdown, Quarto, a static page)
#' the output stays empty.
#'
#' When `output` is given as an id string, place the matching `*Output()` tag in your UI (plain or wrapped in a loader
#' such as `shinycssloaders`); hovercraft parks it out of sight until a tip opens and moves it
#' into whichever item's tip is open. When `output` is given as an output tag, [tip_define()] places it in the hidden
#' holder for you together with its HTML dependencies.
#'
#' With `keep_alive = TRUE` (the default), closing the tip parks the bound output in a hidden holder instead of
#' destroying it, so reopening the tip shows it at once without recomputing unless its reactive inputs changed. When a
#' parked output is shown again, hovercraft refreshes its visibility and size state so suspended outputs update.
#' With `keep_alive = FALSE`, closing the tip unbinds and removes the output from the page, and the next open builds it
#' again.
#'
#' While the output computes (including async renders), hovercraft shows a plain loading placeholder unless the output
#' already uses an external loader. `shinycssloaders` is tested; `waiter` and `bones` are expected to work but are
#' untested. If the render errors, the tip shows the error with a Retry button. If the output returns `NULL` or empty
#' text, the tip shows an empty state.
#'
#' @section Retry:
#' Retry sets the input `input$<output>_retry` (for an output with the id `report_table`, `input$report_table_retry`).
#' Shiny runs a render again only when something it reads changes, so **the render must read that input** to run again
#' on Retry: put `input$report_table_retry` on a line of its own at the top of the render, as in the example below. If
#' the server neither starts nor finishes the output within a few seconds of the click, the tip goes back to the error
#' state with a message that says the server did not run the output again, and Retry stays available.
#'
#' @section One output for several items: One output has one place in the page, so a template that many items
#'   share shows one output, drawn for one item at a time. Your server decides for which: ask the items for the
#'   `"hint"` input (`tip_on(events = "hint")`) and read `input$<id>_hint` in the render, as in the second example
#'   below; its `state` is `"shown"` while the item's hint is open. When a tip opens for a different item than the
#'   one the output was last shown for, the tip shows the loading state and hides the old content until the
#'   server's new value arrives (a server that never reads the open item sends none, and the old content stays
#'   after about a second and a half). A pinned tip sends `input$<id>_pin` (with `events = "pin"`), which the
#'   render can read in the same way, and the output of several open pins sits in the tip that opened last: the
#'   others say "Shown in another tip." While one tip holds the output and another opens for the same output, the
#'   first says "Shown in another tip." and gets the output back when the other closes.
#'
#' The tip's own markup is plain attributes and template JSON with no random or per-session ids, so
#' `shiny::renderUI() |> shiny::bindCache()` and `render*() |> shiny::bindCache()` work across sessions.
#'
#' @param output The Shiny id of the output (one non-empty string with no whitespace, so `ns("detail_plot")` works in
#'   a module), or an output tag (such as `shiny::plotOutput("detail_plot")` or a loader-wrapped output tag).
#' @param keep_alive `TRUE` (the default) to park the bound output in a hidden holder when the tip closes so it stays
#'   alive; `FALSE` to unbind and destroy it on close.
#'
#' @returns A block, to give to [tip_template()].
#' @family tooltips
#' @examples
#' tip_template(
#'   tip_title("{{ label }}"),
#'   tip_output("detail_plot")
#' )
#'
#' tip_template(
#'   tip_title("{{ label }}"),
#'   tip_output("detail_table", keep_alive = FALSE)
#' )
#' @examplesIf interactive()
#' # Retry: the render reads the retry input, so a click on Retry runs it again.
#' library(shiny)
#' ui <- fluidPage(
#'   tip_define(report = tip_template(tip_title("{{ label }}"), tip_output("report_text"))),
#'   textOutput("report_text"),
#'   tip_on(
#'     actionButton("run", "Report"),
#'     template = "report",
#'     record = list(label = "Report"),
#'     id = "run"
#'   )
#' )
#' server <- function(input, output, session) {
#'   tries <- 0
#'   output$report_text <- renderText({
#'     input$report_text_retry
#'     tries <<- tries + 1
#'     if (tries == 1) stop("The first try fails")
#'     "The report is ready"
#'   })
#' }
#' shinyApp(ui, server)
#'
#' # One output for two items: the render reads the hint input to learn which item is open.
#' ui <- fluidPage(
#'   tip_define(detail = tip_template(tip_title("{{ label }}"), tip_output("detail_text"))),
#'   textOutput("detail_text"),
#'   tip_on(
#'     actionButton("a", "Item A"),
#'     template = "detail",
#'     record = list(label = "A"),
#'     id = "a",
#'     events = "hint"
#'   ),
#'   tip_on(
#'     actionButton("b", "Item B"),
#'     template = "detail",
#'     record = list(label = "B"),
#'     id = "b",
#'     events = "hint"
#'   )
#' )
#' server <- function(input, output, session) {
#'   open_item <- reactiveVal("none")
#'   observeEvent(input$a_hint, if (input$a_hint$state == "shown") open_item("A"))
#'   observeEvent(input$b_hint, if (input$b_hint$state == "shown") open_item("B"))
#'   output$detail_text <- renderText(paste("Details of item", open_item()))
#' }
#' shinyApp(ui, server)
#' @export
tip_output <- function(output, keep_alive = TRUE) {
  call <- rlang::current_env()
  tip_arg_flag(keep_alive, "keep_alive", call = call)
  tag <- NULL
  if (inherits(output, c("shiny.tag", "shiny.tag.list"))) {
    id <- tip_output_tag_id(output)
    if (is.null(id)) {
      tip_abort(
        c(
          "{.arg output} is an HTML tag with no Shiny output id.",
          i = paste0(
            "Give a Shiny output tag such as {.code shiny::plotOutput(\"detail_plot\")}, ",
            "or pass the output id as a string."
          )
        ),
        class = "hovercraft_error_argument",
        call = call
      )
    }
    tip_check_id(id, arg = "output", null_ok = FALSE, call = call)
    tag <- output
    output <- id
  } else {
    tip_check_id(output, arg = "output", null_ok = FALSE, call = call)
  }
  block <- tip_block("output", output = output, keep_alive = keep_alive)
  if (!is.null(tag)) {
    attr(block, "hovercraft_output_tag") <- tag
  }
  block
}

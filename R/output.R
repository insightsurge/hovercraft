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
#' `shinycssloaders::withSpinner()`).
#'
#' When `output` is given as an id string, place the matching `*Output()` tag in your UI (plain or wrapped in a loader
#' such as `waiter`, `shinycssloaders` or `bones`); hovercraft parks it out of sight until a tip opens and moves it
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
#' already uses an external loader (`waiter`, `shinycssloaders` or `bones`). If the render errors, the tip shows the
#' error with a Retry button (which also increments `input$<output>_retry` and re-binds the output). If the output
#' returns `NULL` or empty text, the tip shows an empty state.
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

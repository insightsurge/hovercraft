# tip_spec(): a tip for one target of tip_table() that needs settings of its own. It only holds the settings;
# tip_table() checks the words against the table's columns and writes the tip.

#' A tip with settings of its own, for a table
#'
#' `r lifecycle::badge("experimental")`
#' Wraps the words of one [tip_table()] target (`rows`, one column of `cells`, or one column of `headers`) together
#' with settings that only that target needs. A setting left as `NULL` is taken from [tip_table()]. The API shape may
#' change in upcoming releases: later versions add more settings, and `style` and `marker` are not here yet.
#'
#' Give a spec as `rows`, or as an entry of a named list given to `cells` or `headers`. Plain words and a
#' [tip_template()] are accepted in all three places too, and mean "the table's settings".
#'
#' @param content The words of the tip (markdown, with `{{ column }}` placeholders for `rows` and `cells`), or a
#'   [tip_template()]. `NA` and `""` mean no tip.
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param placement Where this tip shows: `"auto"`, `"top"`, `"bottom"`, `"left"` or `"right"`, or `NULL` to take the
#'   table's. A cell or header cell with no placement of its own follows the table's, not its row's.
#'
#' @returns An object of class `hovercraft_spec`, to give to [tip_table()].
#' @family tooltips
#' @examplesIf requireNamespace("DT", quietly = TRUE)
#' scores <- data.frame(team = c("Owls", "Foxes"), wins = c(7, 5), note = c("Top", "Late"))
#' DT::datatable(scores) |>
#'   tip_table(
#'     rows = "**{{ team }}**: {{ wins | number:0 }} wins",
#'     cells = list(note = tip_spec("{{ note }} in the table", placement = "right"))
#'   )
#' @export
tip_spec <- function(content, ..., placement = NULL) {
  call <- rlang::current_env()
  tip_check_dots(..., args = c("content", "placement"))
  if (!tip_content_ok(content)) {
    tip_abort(
      c(
        "{.arg content} must be a string or a {.fn tip_template}, not {.obj_type_friendly {content}}.",
        i = "Give the words of the tip, or {.code NA} for no tip."
      ),
      class = "hovercraft_error_argument",
      call = call
    )
  }
  tip_check_placement(placement, call = call)
  structure(list(content = content, placement = placement), class = "hovercraft_spec")
}

# Words, a template or a missing value: what a tip's content can be before the table's columns are known.
tip_content_ok <- function(x) {
  inherits(x, "hovercraft_template") ||
    (is.character(x) && length(x) == 1L) ||
    (is.atomic(x) && length(x) == 1L && is.na(x))
}

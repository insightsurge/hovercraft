# tip_spec(): a tip for one target of tip_table() that needs settings of its own. It only holds the settings;
# tip_table() checks the words against the table's columns and writes the tip.

#' A tip with settings of its own, for a table
#'
#' `r lifecycle::badge("experimental")`
#' Wraps the words of one [tip_table()] target (`rows`, one column of `cells`, or one column of `headers`) together
#' with settings that only that target needs. A setting left as `NULL` is taken from [tip_table()]. The API shape may
#' change in upcoming releases: later versions add more settings.
#'
#' Give a spec as `rows`, or as an entry of a named list given to `cells` or `headers`. Plain words and a
#' [tip_template()] are accepted in all three places too, and mean "the table's settings".
#'
#' @param content The words of the tip (markdown, with `{{ column }}` placeholders for `rows` and `cells`), or a
#'   [tip_template()]. `NA` and `""` mean no tip.
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param placement Where this tip shows: `"auto"`, `"top"`, `"bottom"`, `"left"` or `"right"`, or `NULL` to take the
#'   table's. A cell or header cell with no placement of its own follows the table's, not its row's.
#' @param style How this target's tips look, or `NULL` to take the table's: whatever `style` takes in [tip_on()] (a
#'   preset name, a [tip_style()], a [tip_theme()] or a [bslib::bs_theme()]). A row's own style also reaches the cells
#'   in that row that have none of their own, because the tip engine reads the nearest tagged element.
#' @param marker The visible cue for this target's cells or header cells: `"corner"`, `"none"` to take the marker off
#'   a column when the table has one, or `NULL` (the default) to take the table's. A spec for `rows` takes none: a row
#'   is not marked. See the `marker` argument of [tip_table()].
#'
#' @returns An object of class `hovercraft_spec`, to give to [tip_table()].
#' @family tooltips
#' @examplesIf requireNamespace("DT", quietly = TRUE)
#' scores <- data.frame(team = c("Owls", "Foxes"), wins = c(7, 5), note = c("Top", "Late"))
#' DT::datatable(scores) |>
#'   tip_table(
#'     rows = "**{{ team }}**: {{ wins | number:0 }} wins",
#'     cells = list(
#'       note = tip_spec("{{ note }}", style = "sober", placement = "right", marker = "corner")
#'     )
#'   )
#' @export
tip_spec <- function(content, ..., style = NULL, placement = NULL, marker = NULL) {
  call <- rlang::current_env()
  tip_check_dots(..., args = c("content", "style", "placement", "marker"))
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
  style <- tip_as_style(style, call = call)
  marker <- if (!is.null(marker)) tip_as_marker(marker, call = call)
  structure(list(content = content, style = style, placement = placement, marker = marker), class = "hovercraft_spec")
}

# Words, a template or a missing value: what a tip's content can be before the table's columns are known.
tip_content_ok <- function(x) {
  inherits(x, "hovercraft_template") ||
    (is.character(x) && length(x) == 1L) ||
    (is.atomic(x) && length(x) == 1L && is.na(x))
}

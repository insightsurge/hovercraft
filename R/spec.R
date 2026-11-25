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
#'   in that row that have none of their own, because the tip engine reads the nearest tagged element. A list of CSS
#'   variables (`tip_style(theme = list("--x" = ...))`) is for the whole page, so a spec refuses it with an error: give
#'   it to [use_hovercraft()], or use the roles of [tip_theme()].
#' @param to `r lifecycle::badge("experimental")` Where this target's pinned tip is drawn: `NULL` or `"float"` (the
#'   default) for a tip that floats beside the row, or `"in_place"` for a panel under the row that holds what any
#'   pinned tip holds (its blocks, the long form, a [tip_output()]). The hover tip still floats at `placement`. In place
#'   needs a pin (`pin = TRUE`), the `rows` target and the table's `id` ([tip_table()] says which is missing), and it
#'   opens by the pin handle or the pin key only. Any other string is an error. See [tip_table()] for what it does when
#'   the table sorts, filters and pages, and for what is not covered.
#' @param marker The visible cue for this target's cells or header cells: `"corner"`, `"none"` to take the marker off
#'   a column when the table has one, or `NULL` (the default) to take the table's. A spec for `rows` takes none: a row
#'   is not marked. See the `marker` argument of [tip_table()].
#' @param more Extra content for the long form of this tip, or `NULL` (the default) for none: words (markdown, with
#'   `{{ column }}` placeholders for `rows` and `cells`), a [tip_template()], or a [tip_more()] that also says when the
#'   long form shows. `NA` and `""` mean no long form. Words and a template are the same as `tip_more()` with its
#'   default: a "More" button in the tip opens the long form, in a row, a cell and a header cell alike. `tip_more(on =
#'   "pin")` and `tip_more(pin = TRUE)` work only in the spec for `rows` of a [tip_table()] with `pin = TRUE`, the one
#'   target that can pin; anywhere else in a table they stop with an error. `tip_more(on = "click")` stops with an error
#'   in a table, where a click selects a row.
#' @param pin Whether a reader can keep this target's tip open: `NULL` (the default) or `FALSE` for no pin, or `TRUE`
#'   for a pin handle in the corner of the tip and a pin key, as in [tip_on()]. Only the tips of `rows` pin for now: a
#'   spec with a pin given to `cells` or `headers` stops with "only rows pin for now". A click on a row selects it in
#'   both widgets, so the `"click"` pin of [tip_on()] is refused in a table. A pin needs the table's `id`. With
#'   `pin = TRUE`, `tip_more(on = "pin")` opens the long form when the row is pinned.
#' @param events The Shiny inputs this target's tips send: one or more of `"hint"`, `"pin"` and `"copy"`, or `NULL` (the
#'   default) for none. `"pin"` and `"copy"` belong to a pinned tip, so asking for either without a `pin` stops with an
#'   error, as in [tip_on()]. Only the tips of `rows` send them for now ("only rows send events for now"), and they need
#'   the table's `id`. The inputs belong to the table, one per event, not one per row: see the "Shiny inputs" section of
#'   [tip_table()].
#'
#' @returns An object of class `hovercraft_spec`, to give to [tip_table()].
#' @family tooltips
#' @examplesIf requireNamespace("DT", quietly = TRUE)
#' scores <- data.frame(team = c("Owls", "Foxes"), wins = c(7, 5), note = c("Top", "Late"))
#' DT::datatable(scores) |>
#'   tip_table(
#'     rows = "**{{ team }}**: {{ wins | number:0 }} wins",
#'     cells = list(
#'       note = tip_spec(
#'         "{{ team }}",
#'         more = tip_more("{{ note }}", on = "hover"),
#'         style = "sober",
#'         placement = "right",
#'         marker = "corner"
#'       )
#'     )
#'   )
#'
#' # A row that can be pinned, and a Shiny input that says which row was pinned
#' DT::datatable(scores) |>
#'   tip_table(
#'     rows = tip_spec("**{{ team }}**: {{ wins | number:0 }} wins", pin = TRUE, events = "pin"),
#'     id = "scores_tips"
#'   )
#' @export
tip_spec <- function(
  content,
  ...,
  style = NULL,
  placement = NULL,
  to = NULL,
  marker = NULL,
  more = NULL,
  pin = NULL,
  events = NULL
) {
  call <- rlang::current_env()
  tip_check_dots(..., args = c("content", "style", "placement", "to", "marker", "more", "pin", "events"))
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
  if (!is.null(more) && !tip_content_ok(more) && !inherits(more, "hovercraft_more")) {
    tip_abort(
      c(
        "{.arg more} must be a string, a {.fn tip_template} or a {.fn tip_more}, not {.obj_type_friendly {more}}.",
        i = "Give the words of the long form, a {.fn tip_template}, a {.fn tip_more}, or {.code NULL}."
      ),
      class = "hovercraft_error_argument",
      call = call
    )
  }
  tip_check_placement(placement, call = call)
  to <- tip_check_to(to, call = call)
  pin <- tip_check_pin(pin, call = call)
  events <- tip_check_events(events, call = call)
  tip_check_events_pin(events, pin, call = call)
  style <- tip_as_style(style, call = call)
  tip_check_style_tokens(style, call)
  marker <- if (!is.null(marker)) tip_as_marker(marker, call = call)
  structure(
    list(
      content = content,
      style = style,
      placement = placement,
      to = to,
      marker = marker,
      more = more,
      pin = pin,
      events = events
    ),
    class = "hovercraft_spec"
  )
}

# Words, a template or a missing value: what a tip's content can be before the table's columns are known.
tip_content_ok <- function(x) {
  inherits(x, "hovercraft_template") ||
    (is.character(x) && length(x) == 1L) ||
    (is.atomic(x) && length(x) == 1L && is.na(x))
}

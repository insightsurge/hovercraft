# The blocks a template is made of. Each is a small classed list, `{ type, ... }` in the engine's words, checked when
# it is made: the arguments, then the placeholders in its text. The block types and what they take are the engine's
# (engine.json), so a plain `list(type = "text", text = "...")` is accepted too (see tip_template()) and checked the
# same way.

tip_block <- function(type, ...) {
  fields <- list(...)
  structure(c(list(type = type), fields[!vapply(fields, is.null, logical(1))]), class = "hovercraft_block")
}

#' Blocks for a tip's template
#'
#' `r lifecycle::badge("experimental")`
#' The parts a template is made of. `tip_title()` is the first line, `tip_text()` a paragraph of markdown,
#' `tip_badge()` a short label, `tip_fields()` a list of label and value pairs, and `tip_code()` a piece of code shown
#' as text. Text and values may hold placeholders, `{{ field }}` or `{{ field | number:2 }}`, filled from the record of
#' the item the tip belongs to. A placeholder names one field, matched exactly (dots and spaces allowed), and takes one
#' filter at most: `number` (digits after the point, rounded as R does) or `percent` (the value times 100, then digits,
#' then a percent sign). A value that is missing or blank shows a dash. There is no way to write a literal `{{` yet.
#'
#' @param text One string, with placeholders if you like. In `tip_text()` it is markdown (strong, emphasis, code,
#'   links, short lists), and a value put in by a placeholder is never read as markdown.
#' @param tone One string for a badge's look, or `NULL`. What it means is up to the engine.
#' @param ... Named strings for `tip_fields()`: each name is a label (plain text) and each value a string that may hold
#'   placeholders, as in `tip_fields(Records = "{{ records | number:0 }}")`.
#' @param field One string: the name of the record field that holds the code. The code is shown as text, never run,
#'   and never filled in by placeholders.
#' @param language One string naming the code's language, or `NULL`.
#' @param copy `TRUE` asks for a Copy button, which lives in a tip that stays open. That comes in a later version, so
#'   for now only `FALSE` is accepted.
#'
#' @returns A block, to give to [tip_template()].
#' @family tooltips
#' @examples
#' tip_title("{{ label }}")
#' tip_text("**{{ differences | number:0 }}** differences")
#' tip_badge("{{ status }}", tone = "warning")
#' tip_fields(Records = "{{ records | number:0 }}", Files = "{{ files }}")
#' tip_code("sql", language = "sql")
#' @name tip_blocks
NULL

#' @rdname tip_blocks
#' @export
tip_title <- function(text) {
  checkmate::assert_string(text, min.chars = 1L)
  tip_check_text(text, "The text of `tip_title()`")
  tip_block("title", text = text)
}

#' @rdname tip_blocks
#' @export
tip_text <- function(text) {
  checkmate::assert_string(text, min.chars = 1L)
  tip_check_text(text, "The text of `tip_text()`")
  tip_block("text", text = text)
}

#' @rdname tip_blocks
#' @export
tip_badge <- function(text, tone = NULL) {
  checkmate::assert_string(text, min.chars = 1L)
  checkmate::assert_string(tone, min.chars = 1L, null.ok = TRUE)
  tip_check_text(text, "The text of `tip_badge()`")
  tip_block("badge", text = text, tone = tone)
}

#' @rdname tip_blocks
#' @export
tip_fields <- function(...) {
  pairs <- list(...)
  checkmate::assert_list(pairs, names = "unique", min.len = 1L, .var.name = "...")
  for (label in names(pairs)) {
    checkmate::assert_string(pairs[[label]], min.chars = 1L, .var.name = label)
    tip_check_text(pairs[[label]], sprintf("The value of \"%s\" in `tip_fields()`", label))
    if (grepl(tip_rules()$placeholder$open, label, fixed = TRUE)) {
      tip_warn(
        c(
          "The label {.val {label}} in {.fn tip_fields} holds a placeholder mark.",
          i = "Labels are plain text; only the values are filled in."
        ),
        class = "hovercraft_warning_label"
      )
    }
  }
  tip_block(
    "fields",
    fields = unname(Map(function(label, value) list(label = label, value = value), names(pairs), pairs))
  )
}

#' @rdname tip_blocks
#' @export
tip_code <- function(field, language = NULL, copy = FALSE) {
  checkmate::assert_string(field, min.chars = 1L)
  checkmate::assert_string(language, min.chars = 1L, null.ok = TRUE)
  checkmate::assert_flag(copy)
  if (copy) {
    tip_abort(
      c(
        "{.arg copy} is {.code TRUE}, but a Copy button lives in a tip that stays open.",
        i = "That comes in a later version; leave {.arg copy} out for now."
      ),
      class = "hovercraft_error_pin"
    )
  }
  tip_block("code", field = field, language = language)
}

#' @export
print.hovercraft_block <- function(x, ...) {
  cli::cat_line("<tip block: ", x$type, ">")
  invisible(x)
}

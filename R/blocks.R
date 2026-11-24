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
#' @param status What the badge says about its item, or `NULL` for a plain badge: one of `"success"`, `"warning"`,
#'   `"danger"` or `"info"`, the words bslib uses, or a placeholder such as `"{{ tone }}"` filled from the record. Any
#'   other fixed word is an error. When filled from a record, one of the four words colours the badge and any other
#'   value shows a plain badge. The badge is coloured from the page's Bootstrap colours for that status (with plain
#'   colours on a page without Bootstrap), and its text should say the same thing, because colour alone is not enough. A
#'   block written as a plain list, `list(type = "badge", text = "a", tone = "info")`, uses the engine's name for it,
#'   `tone`, with the same rule.
#' @param ... Named strings for `tip_fields()`: each name is a label (plain text) and each value a string that may hold
#'   placeholders, as in `tip_fields(Records = "{{ records | number:0 }}")`.
#' @param field One string: the name of the record field that holds the code. The code is shown as text, never run,
#'   and never filled in by placeholders.
#' @param language One string naming the code's language, or `NULL`.
#' @param copy `TRUE` asks for a Copy button under the code, which lives in a tip that stays open: give the tag that has
#'   this template `pin = TRUE` or `pin = "click"` in [tip_on()], or the button never shows. The button copies the code
#'   as text, also on a page that is not served over https (where the browser's clipboard is closed to a page, the
#'   copy is done the older way), and says "Copied" or "Copy failed" beside the button. With `events = "copy"` in
#'   [tip_on()], the server reads each press as `input$<id>_copy`.
#'
#' @returns A block, to give to [tip_template()].
#' @family tooltips
#' @examples
#' tip_title("{{ label }}")
#' tip_text("**{{ differences | number:0 }}** differences")
#' tip_badge("{{ status }}", status = "warning")
#' tip_fields(Records = "{{ records | number:0 }}", Files = "{{ files }}")
#' tip_code("sql", language = "sql")
#' @name tip_blocks
NULL

#' @rdname tip_blocks
#' @export
tip_title <- function(text) {
  tip_arg_string(text, "text")
  tip_check_text(text, "The text of `tip_title()`")
  tip_block("title", text = text)
}

#' @rdname tip_blocks
#' @export
tip_text <- function(text) {
  tip_arg_string(text, "text")
  tip_check_text(text, "The text of `tip_text()`")
  tip_block("text", text = text)
}

#' @rdname tip_blocks
#' @export
tip_badge <- function(text, status = NULL) {
  tip_arg_string(text, "text")
  tip_arg_string(status, "status", null_ok = TRUE)
  tip_check_status(status)
  tip_check_text(text, "The text of `tip_badge()`")
  # The engine's badge calls this field `tone`.
  tip_block("badge", text = text, tone = status)
}

#' @rdname tip_blocks
#' @export
tip_fields <- function(...) {
  pairs <- list(...)
  tip_arg_list(
    pairs,
    "...",
    min.len = 1L,
    fix = "Give each value a label: {.code tip_fields(Label = \"{{{{ field }}}}\")}."
  )
  for (label in names(pairs)) {
    tip_arg_string(pairs[[label]], label)
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
  tip_arg_string(field, "field")
  tip_arg_string(language, "language", null_ok = TRUE)
  tip_arg_flag(copy, "copy")
  tip_block("code", field = field, language = language, copy = if (copy) TRUE)
}

#' @export
print.hovercraft_block <- function(x, ...) {
  cli::cat_line("<tip block: ", x$type, ">")
  invisible(x)
}

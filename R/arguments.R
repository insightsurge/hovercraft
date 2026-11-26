# Argument checks. checkmate does the checking; a failed check becomes an error of this package's own class
# (`hovercraft_error_argument`) that names the argument, says what is wrong and says the fix, instead of checkmate's
# "Assertion on 'x' failed". `result` is what a checkmate `check_*()` gives: TRUE, or its message.

tip_arg <- function(result, arg, fix, call = rlang::caller_env()) {
  if (!isTRUE(result)) {
    tip_abort(
      c(paste0("{.arg ", arg, "} is not right: ", tip_escape(result)), i = fix),
      class = "hovercraft_error_argument",
      call = call
    )
  }
  invisible()
}

# Stops when `...` holds anything: it is there so a misspelt argument is an error, and `args` names the real ones.
tip_check_dots <- function(..., args, class = "hovercraft_error_argument", call = rlang::caller_env()) {
  if (...length()) {
    given <- ...names()
    given <- if (is.null(given)) rep("(unnamed)", ...length()) else ifelse(given == "", "(unnamed)", given)
    tip_abort(
      c(
        "{.arg ...} must be empty.",
        x = "Not an argument: {.val {given}}.",
        i = "The arguments are {.arg {args}}."
      ),
      class = class,
      call = call
    )
  }
  invisible()
}

tip_arg_string <- function(x, arg, min_chars = 1L, null_ok = FALSE, na_ok = FALSE, call = rlang::caller_env()) {
  fix <- paste0(
    "Give one ",
    if (min_chars > 0L) "non-empty ",
    "string",
    if (null_ok) ", or {.code NULL}",
    "."
  )
  tip_arg(checkmate::check_string(x, min.chars = min_chars, null.ok = null_ok, na.ok = na_ok), arg, fix, call)
}

tip_arg_flag <- function(x, arg, call = rlang::caller_env()) {
  tip_arg(checkmate::check_flag(x), arg, "Give {.code TRUE} or {.code FALSE}.", call)
}

tip_arg_list <- function(
  x,
  arg,
  min.len = 0L,
  null_ok = FALSE,
  fix = "Give a list with a different name for each value.",
  call = rlang::caller_env()
) {
  tip_arg(checkmate::check_list(x, names = "unique", min.len = min.len, null.ok = null_ok), arg, fix, call)
}

tip_arg_chars <- function(x, arg, null_ok = FALSE, call = rlang::caller_env()) {
  tip_arg(
    checkmate::check_character(x, any.missing = FALSE, min.len = 1L, null.ok = null_ok),
    arg,
    paste0("Give the names as a character vector", if (null_ok) ", or {.code NULL}", "."),
    call
  )
}

tip_arg_data <- function(x, arg, call = rlang::caller_env()) {
  tip_arg(checkmate::check_data_frame(x), arg, "Give a data frame.", call)
}

# A Shiny id, the same rule for every `id =` in the package: one non-empty string with no whitespace and no colon.
# Hyphens are fine, so an id made by `ns()` works. A colon is not: Shiny reads the part of an input's name after it as
# the input's type, so `a:b_pin` would ask for a handler of type `b_pin` and end the session. `null_ok`: `NULL` means no
# id.
tip_check_id <- function(x, arg = "id", null_ok = TRUE, call = rlang::caller_env()) {
  ok <- checkmate::test_string(x, min.chars = 1L, null.ok = null_ok) &&
    (is.null(x) || !grepl("[[:space:]:]", x))
  tip_arg(
    if (ok) TRUE else "Must be one non-empty string without whitespace or a colon.",
    arg,
    paste0(
      "Give one non-empty string with no spaces and no {.code :} (Shiny reads what follows a colon as the type of an ",
      "input), such as {.code \"scores\"}",
      if (null_ok) ", or {.code NULL}",
      "."
    ),
    call
  )
}

# The tag's own id is the base of the tip's Shiny inputs when `events` or a pin is given and the call gives no `id`, so
# it follows the rule of `tip_check_id()`.
tip_check_tag_id <- function(tag_id, call = rlang::caller_env()) {
  if (!checkmate::test_string(tag_id, min.chars = 1L) || grepl("[[:space:]:]", tag_id)) {
    tip_abort(
      c(
        "The tag's own id, {.val {tag_id}}, names the tip's Shiny inputs, and it has whitespace or a colon.",
        i = paste(
          "Give {.fn tip_on} an {.arg id} of its own (one string with no spaces and no {.code :}),",
          "or change the tag's id."
        )
      ),
      class = "hovercraft_error_argument",
      call = call
    )
  }
  invisible()
}

# Where a pinned tip of a table is drawn: floating beside its target (NULL or "float") or "in_place", a panel under the
# row. "float" is NULL: the checks and the page see one value.
tip_check_to <- function(x, arg = "to", call = rlang::caller_env()) {
  tip_arg(
    checkmate::check_choice(x, c("float", "in_place"), null.ok = TRUE),
    arg,
    "Give {.code \"float\"} or {.code NULL} for a tip that floats, or {.code \"in_place\"} for a panel under the row.",
    call
  )
  if (identical(x, "in_place")) x
}

# Where a tip shows: one of the engine's placements, or `NULL` for the page's choice.
tip_check_placement <- function(x, arg = "placement", call = rlang::caller_env()) {
  places <- unlist(tip_rules()$placement$values)
  tip_arg(
    checkmate::check_choice(x, places, null.ok = TRUE),
    arg,
    paste0("Give one of ", paste0("\"", places, "\"", collapse = ", "), ", or {.code NULL}."),
    call
  )
}

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

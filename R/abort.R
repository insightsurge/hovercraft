# Errors and warnings are classed, so a caller (and a test) can tell one kind from another. Every error also has the
# class `hovercraft_error`, every warning `hovercraft_warning`. `call` is the user-facing function. Data that goes into
# a message is substituted by cli (`{.val {x}}`), which keeps braces in it from being read again.

tip_abort <- function(message, class = NULL, ..., call = rlang::caller_env(), .envir = parent.frame()) {
  cli::cli_abort(message, class = c(class, "hovercraft_error"), ..., call = call, .envir = .envir)
}

tip_warn <- function(message, class = NULL, ..., call = rlang::caller_env(), .envir = parent.frame()) {
  cli::cli_warn(message, class = c(class, "hovercraft_warning"), ..., call = call, .envir = .envir)
}

# The first `n` problems as bullets, and a line saying how many more there were.
tip_bullets <- function(problems, n = 10L) {
  shown <- utils::head(problems, n)
  bullets <- stats::setNames(shown, rep("x", length(shown)))
  if (length(problems) > n) {
    bullets <- c(bullets, i = paste0("... and ", length(problems) - n, " more."))
  }
  bullets
}

# "; did you mean ..." for a name that is close to one of `known`, else "".
tip_near <- function(name, known) {
  near <- agrep(name, known, max.distance = 2L, value = TRUE)
  if (length(near)) paste0(" Did you mean ", paste0("\"", near, "\"", collapse = " or "), "?") else ""
}

# Braces in data doubled, so cli prints them as they are.
tip_escape <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}

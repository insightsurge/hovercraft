# Shared helpers: R code as text. Shiny loads every file in R/ before app.R, so this file only defines functions.
#
# The idea: a plate keeps its settings as R code (a string built from its controls). The figure is that code, run;
# the code note shows the same text. So what is on screen is what the code makes.

# ---- Building code --------------------------------------------------------------------------------------------------

# An R string literal, double-quoted: code_str("a \"b\"") is "a \"b\"" with its escapes.
code_str <- function(x) encodeString(x, quote = "\"")

# A vector of strings as c("a", "b"), or one string when there is just one.
code_chr <- function(x) {
  if (length(x) == 1L) code_str(x) else paste0("c(", paste(vapply(x, code_str, ""), collapse = ", "), ")")
}

# One call as text. `...` are the arguments as code (strings), named or not; NULL and "" are left out. The call fits on
# one line when it is short; otherwise every argument gets its own line, indented two spaces.
code_call <- function(fn, ..., width = 100L) {
  args <- list(...)
  args <- args[!vapply(args, function(a) is.null(a) || identical(a, ""), NA)]
  if (!length(args)) {
    return(paste0(fn, "()"))
  }
  names <- names(args) %||% rep("", length(args))
  parts <- ifelse(nzchar(names), paste0(names, " = ", unlist(args)), unlist(args))
  one <- paste0(fn, "(", paste(parts, collapse = ", "), ")")
  if (nchar(one) <= width && !grepl("\n", one, fixed = TRUE)) {
    return(one)
  }
  indented <- vapply(parts, function(p) gsub("\n", "\n  ", p, fixed = TRUE), "", USE.NAMES = FALSE)
  paste0(fn, "(\n", paste0("  ", indented, collapse = ",\n"), "\n)")
}

# A named list as list(a = "x", b = 1): `values` are R values, written as code.
code_list <- function(values) {
  parts <- vapply(
    values,
    function(v) {
      if (is.null(v)) {
        "NULL"
      } else if (is.character(v)) {
        code_chr(v)
      } else {
        paste(format(v, scientific = FALSE), collapse = ", ")
      }
    },
    ""
  )
  do.call(code_call, c(list("list"), as.list(parts)))
}

# The code a plate shows: a line to load the package, then the UI part, then the server part when there is one.
code_text <- function(ui, server = NULL) {
  paste(
    c("library(hovercraft)", "", ui, if (!is.null(server)) c("", "# In the server function", server)),
    collapse = "\n"
  )
}

# `x` when it is one of `allowed`, else `default`: a value from the page or the address is never put into code
# unchecked.
choice <- function(x, allowed, default = "") {
  if (is.character(x) && length(x) == 1L && !is.na(x) && x %in% allowed) x else default
}

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
# one line when it is short; otherwise every argument gets its own line, indented two spaces. The width is about what
# the margin shows on one line, so the code reads there without wrapping.
code_call <- function(fn, ..., width = 56L) {
  args <- list(...)
  args <- args[!vapply(args, function(a) is.null(a) || identical(a, ""), NA)]
  if (!length(args)) {
    return(paste0(fn, "()"))
  }
  names <- names(args) %||% rep("", length(args))
  # A name R can't read bare, such as `Week 46`, goes between backticks.
  names <- ifelse(nzchar(names) & make.names(names) != names, paste0("`", names, "`"), names)
  parts <- ifelse(nzchar(names), paste0(names, " = ", unlist(args)), unlist(args))
  one <- paste0(fn, "(", paste(parts, collapse = ", "), ")")
  if (nchar(one) <= width && !grepl("\n", one, fixed = TRUE)) {
    return(one)
  }
  indented <- vapply(parts, function(p) gsub("\n", "\n  ", p, fixed = TRUE), "", USE.NAMES = FALSE)
  paste0(fn, "(\n", paste0("  ", indented, collapse = ",\n"), "\n)")
}

# One value as code: a string or a number, or c(...) of several, which wraps like any call when it is long.
code_value <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  write <- if (is.character(x)) code_str else function(v) format(v, scientific = FALSE, trim = TRUE)
  one <- vapply(x, write, "", USE.NAMES = FALSE)
  if (length(one) == 1L) one else do.call(code_call, c(list("c"), as.list(one)))
}

# A named list as list(a = "x", b = 1): `values` are R values, written as code.
code_list <- function(values) {
  do.call(code_call, c(list("list"), lapply(values, code_value)))
}

# A data frame as code, `name <- data.frame(...)`, one column to a line: how a section's made-up data is shown.
code_df <- function(name, df) {
  paste0(name, " <- ", do.call(code_call, c(list("data.frame"), lapply(df, code_value), list(width = 0L))))
}

# Code moved right by `n` spaces on every line after the first, to sit inside a block that is already indented.
code_indent <- function(code, n = 2L) gsub("\n", paste0("\n", strrep(" ", n)), code)

# `lapply(over, function(var) { body })` as code, the body indented inside the braces.
code_lapply <- function(over, var, body) {
  paste0("lapply(", over, ", function(", var, ") {\n  ", code_indent(body), "\n})")
}

# A word a reader can rest on or reach with Tab: `htmltools::tags$span(text, tabindex = "0")` as code. `text` is
# written as a string, or as it is when given as code with I(), such as I("columns$name[[i]]").
code_word <- function(text, class = NULL) {
  label <- if (inherits(text, "AsIs")) as.character(text) else code_str(text)
  code_call("htmltools::tags$span", label, tabindex = code_str("0"), class = if (!is.null(class)) code_str(class))
}

# A sentence of the specimen's text as code: `...` alternate words and code, starting and ending with words, so
# code_sentence("The tip of", tip, "opens here.") is one paragraph with one tipped word in it.
code_sentence <- function(...) {
  pieces <- list(...)
  pieces[seq(1L, length(pieces), by = 2L)] <- lapply(pieces[seq(1L, length(pieces), by = 2L)], code_str)
  do.call(code_call, c(list("htmltools::p"), pieces, list(class = code_str("specimen-text"))))
}

# A row of words, one tip each, as code: a small title, then `over` run through `lapply()`, each item a list item
# whose tip `tip` builds from `var`.
code_strip <- function(title, over, var, tip) {
  paste0(
    code_call("htmltools::p", code_str(title), class = code_str("strip-title")),
    "\n",
    code_call(
      "htmltools::tags$ul",
      class = code_str("strip"),
      code_lapply(over, var, code_call("htmltools::tags$li", tip))
    )
  )
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

# The events a tip can ask for, as a choice set offers them (sections 3.3 and 6.3), and the events each choice means.
event_choices <- c("hint" = "hint", "hint and pin" = "pin", "hint, pin and copy" = "all")
events_of <- function(x) {
  switch(choice(x, event_choices, "all"), hint = "hint", pin = c("hint", "pin"), all = c("hint", "pin", "copy"))
}

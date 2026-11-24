# What a generated dependency carries, read the way a page reads it. The CSS is the text of the stylesheet file that the
# dependency serves; operations for the runtime script are base64 of JSON in a <meta>.
dep_css <- function(dep) {
  paste(readLines(file.path(dep$src$file, dep$stylesheet), warn = FALSE), collapse = "\n")
}

dep_ops <- function(dep) {
  rawToChar(jsonlite::base64_dec(dep$meta[["hovercraft-ops"]]))
}

# The runtime dependency's configuration, as a list.
dep_config <- function(dep, name = "hovercraft-config") {
  jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(dep$meta[[name]])), simplifyVector = FALSE)
}

# The text of a dependency's one style sheet file, or "" when it is not a file on this machine (a widget's own).
dep_css_if_file <- function(dep) {
  path <- file.path(dep$src$file, dep$stylesheet)
  if (file.exists(path)) dep_css(dep) else ""
}

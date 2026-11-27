# What a generated dependency carries, read the way a page reads it: operations for the runtime script, as base64 of JSON in
# a <meta>. The CSS of a theme or a style is one of them.
dep_ops <- function(dep) {
  rawToChar(jsonlite::base64_dec(dep$meta[["hovercraft-ops"]]))
}

# The CSS text a dependency carries, or "" when it carries none.
dep_css <- function(dep) {
  if (is.null(dep$meta[["hovercraft-ops"]])) {
    return("")
  }
  ops <- jsonlite::fromJSON(dep_ops(dep), simplifyVector = FALSE)
  paste(vapply(Filter(function(op) identical(op$op, "css"), ops), function(op) op$text, ""), collapse = "\n")
}

# The runtime dependency's configuration, as a list.
dep_config <- function(dep, name = "hovercraft-config") {
  jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(dep$meta[[name]])), simplifyVector = FALSE)
}

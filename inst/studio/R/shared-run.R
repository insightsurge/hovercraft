# Shared helpers: running the code a plate shows.

# An environment that holds hovercraft's exported functions and nothing else, as `library(hovercraft)` gives a reader.
# The code a plate shows runs here, so it can't lean on anything a reader's own app would not have.
studio_exports <- function() {
  env <- new.env(parent = baseenv())
  for (name in getNamespaceExports("hovercraft")) {
    assign(name, getExportedValue("hovercraft", name), envir = env)
  }
  env
}

# Runs the UI part of a plate's code: every top-level expression, in a fresh child of `studio_exports()`, and keeps
# the visible results that are UI (tags, tag lists, HTML dependencies). The first line, `library(hovercraft)`, is
# skipped: the exports are there already. Warnings are kept beside the result, so a plate can say what the package
# warned about; a template defined again with other blocks (every control change does that) is expected and not
# kept. An error becomes a message, not a crash.
run_code <- function(code, exports = studio_exports()) {
  state <- new.env()
  state$warnings <- character()
  state$error <- NULL
  env <- new.env(parent = exports)
  out <- list()
  tryCatch(
    withCallingHandlers(
      {
        for (expr in parse(text = code, keep.source = FALSE)) {
          if (is.call(expr) && identical(expr[[1L]], as.name("library"))) {
            next
          }
          value <- withVisible(eval(expr, env))
          if (value$visible && is_ui(value$value)) out[[length(out) + 1L]] <- as_ui(value$value)
        }
      },
      warning = function(w) {
        if (!inherits(w, "hovercraft_warning_template")) {
          state$warnings <- c(state$warnings, conditionMessage(w))
        }
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) state$error <- conditionMessage(e)
  )
  list(ui = htmltools::tagList(out), warnings = unique(state$warnings), error = state$error)
}

is_ui <- function(x) {
  inherits(x, c("shiny.tag", "shiny.tag.list", "html_dependency", "htmlwidget")) ||
    (is.list(x) && length(x) > 0L && all(vapply(x, inherits, NA, c("shiny.tag", "shiny.tag.list", "html_dependency"))))
}

# A dependency (or a list of them) becomes an empty hidden tag that carries it.
as_ui <- function(x) {
  if (inherits(x, "html_dependency") || (is.list(x) && all(vapply(x, inherits, NA, "html_dependency")))) {
    return(htmltools::attachDependencies(htmltools::tags$span(hidden = NA), x))
  }
  if (is.list(x) && !inherits(x, c("shiny.tag", "shiny.tag.list"))) {
    return(htmltools::tagList(x))
  }
  x
}

# Runs a plate's server code as it is shown: every expression, with hovercraft's exports, the session and its inputs in
# reach, as in a reader's own server function.
run_server_code <- function(code, input, session, ..., exports = studio_exports()) {
  env <- new.env(parent = exports)
  env$input <- input
  env$session <- session
  # What the shown code reads from the page's UI code, such as its data.
  list2env(list(...), envir = env)
  for (expr in parse(text = code, keep.source = FALSE)) {
    eval(expr, env)
  }
  invisible(env)
}

# What a plate shows under its figure: the package's warnings and any error, as plain notes.
notes_ui <- function(result) {
  if (is.null(result$error) && !length(result$warnings)) {
    return(NULL)
  }
  htmltools::tagList(
    if (!is.null(result$error)) {
      htmltools::p(class = "figure-note is-error", role = "alert", htmltools::strong("Error: "), result$error)
    },
    lapply(result$warnings, function(w) {
      htmltools::p(class = "figure-note", role = "status", htmltools::strong("Warning: "), w)
    })
  )
}

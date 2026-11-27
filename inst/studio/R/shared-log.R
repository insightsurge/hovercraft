# Shared helpers: the Inputs sent note. The server sends each line by a custom message; shared.js adds it to the
# plate's list with textContent (no HTML from data), keeps the newest 14 and counts unseen lines on a phone.

# One line in a plate's log: the input's name and its value as R would print it.
log_line <- function(session, plate, name, value, kind = "input") {
  session$sendCustomMessage(
    "studio-log",
    list(plate = plate, name = name, value = r_value(value), kind = kind, time = format(Sys.time(), "%H:%M:%S"))
  )
}

# Every change of the inputs `names` goes to the plate's log, as `input$<name>`.
log_inputs <- function(input, session, plate, names) {
  for (name in names) {
    local({
      name <- name
      shiny::observeEvent(
        input[[name]],
        log_line(session, plate, paste0("input$", name), input[[name]]),
        ignoreInit = TRUE
      )
    })
  }
}

# A value as R code, short: list(state = "pinned", by = "reader"), a number, a string, NULL.
r_value <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (is.list(x)) {
    if (!length(x)) {
      return("list()")
    }
    keys <- names(x) %||% rep("", length(x))
    parts <- vapply(
      seq_along(x),
      function(i) paste0(if (nzchar(keys[[i]])) paste0(keys[[i]], " = "), r_value(x[[i]])),
      ""
    )
    return(paste0("list(", paste(parts, collapse = ", "), ")"))
  }
  if (is.character(x)) {
    return(code_chr(x))
  }
  if (is.logical(x) && length(x) == 1L) {
    return(
      if (is.na(x)) {
        "NA"
      } else if (x) {
        "TRUE"
      } else {
        "FALSE"
      }
    )
  }
  paste(format(x, scientific = FALSE, trim = TRUE), collapse = ", ")
}

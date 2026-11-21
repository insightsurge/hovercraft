# What goes to the page as JSON. Every value in a tip is a single value (text, a number, true or false, or null), so
# `auto_unbox` is safe; lists stay arrays. `NA`, `NaN` and infinite numbers are `null` (the engine shows a dash for
# them), numbers keep 15 significant digits (jsonlite's most), and "<" is escaped so a value can never close a script.

tip_json <- function(x) {
  json <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  gsub("<", "\\u003c", as.character(json), fixed = TRUE)
}

# One value of a record: list(value) for a value the engine can show, list(skip = TRUE) for NULL (the name is left
# out), list(problem) for one it cannot.
tip_value <- function(x) {
  if (is.null(x)) {
    return(list(skip = TRUE))
  }
  if (inherits(x, "AsIs")) {
    return(list(problem = "is wrapped in `I()`; a record holds single values, so drop `I()`"))
  }
  if (is.list(x)) {
    return(list(problem = "is a list; a tip shows a dash for it. Give one value"))
  }
  if (length(x) != 1L) {
    return(list(problem = sprintf("has %d values; give one", length(x))))
  }
  if (is.factor(x)) {
    x <- as.character(x)
  } else if (inherits(x, "Date")) {
    x <- format(x, "%Y-%m-%d")
  } else if (inherits(x, "POSIXt")) {
    x <- format(as.POSIXct(x), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  if (is.numeric(x)) {
    x <- if (is.finite(x)) as.numeric(x) else NA_real_
  } else if (is.character(x)) {
    x <- enc2utf8(x)
  } else if (!is.logical(x)) {
    return(list(problem = sprintf("is %s; give text, a number or true or false", class(x)[[1]])))
  }
  list(value = x)
}

# A record (a named list, or a data frame of one row) as a named list of single values, ready for JSON.
tip_record <- function(record, call = rlang::caller_env()) {
  if (is.data.frame(record)) {
    if (nrow(record) != 1L) {
      tip_abort(
        c(
          "{.arg record} is a data frame of {nrow(record)} rows; a tip takes one.",
          i = "Use {.code tip_rows(data)} for one record per row, then give {.arg record} one of them."
        ),
        class = "hovercraft_error_record",
        call = call
      )
    }
    record <- lapply(record, function(column) column[[1L]])
  }
  checkmate::assert_list(record, names = "unique", .var.name = "record")
  out <- list()
  problems <- character()
  for (name in names(record)) {
    got <- tip_value(record[[name]])
    if (!is.null(got$problem)) {
      problems <- c(problems, paste0("{.code record$", tip_escape(name), "} ", got$problem, "."))
    } else if (is.null(got$skip)) {
      out[[name]] <- got$value
    }
  }
  if (length(problems)) {
    tip_abort(
      c(
        "{.arg record} has values a tip cannot show.",
        tip_bullets(problems),
        i = "Keep only the columns the template names."
      ),
      class = "hovercraft_error_record",
      call = call
    )
  }
  structure(out, names = names(out) %||% character())
}

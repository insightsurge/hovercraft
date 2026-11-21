# What goes to the page as JSON. Every value in a tip is a single value (text, a number, true or false, or null), so
# `auto_unbox` is safe; lists stay arrays. `NA`, `NaN` and infinite numbers are `null` (the engine shows a dash for
# them), numbers keep 15 significant digits (jsonlite's most), and "<" is escaped so a value can never close a script.

tip_json <- function(x) {
  json <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  gsub("<", "\\u003c", as.character(json), fixed = TRUE)
}

# One value of a record: list(value) for a value the engine can show (NULL is written as missing, which the engine
# shows as a dash, as it does NA), list(problem) for one it cannot.
tip_value <- function(x) {
  if (is.null(x)) {
    return(list(value = NA))
  }
  if (inherits(x, "POSIXlt")) {
    x <- as.POSIXct(x)
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

# Stops when a data frame has a column that is itself a data frame or a matrix: a tip takes one value per column.
tip_check_columns <- function(data, call = rlang::caller_env()) {
  bad <- names(data)[vapply(data, function(column) is.data.frame(column) || is.matrix(column), NA)]
  if (length(bad)) {
    tip_abort(
      c(
        "Columns that hold a data frame or a matrix: {.val {bad}}. A tip takes one value per column.",
        i = "Spread them into plain columns first, one column for each value."
      ),
      class = "hovercraft_error_record",
      call = call
    )
  }
  invisible(data)
}

# A record (a named list, or a data frame of one row) as a named list of single values, ready for JSON. `keep` names the
# columns to keep (the ones a template names), or NULL for all of them; the others are dropped before any value is read.
tip_record <- function(record, keep = NULL, call = rlang::caller_env()) {
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
    if (!is.null(keep)) {
      record <- record[names(record) %in% keep]
    }
    tip_check_columns(record, call)
    record <- lapply(record, function(column) column[[1L]])
  }
  tip_arg_list(record, "record", fix = "Give a named list, or a data frame of one row.")
  if (!is.null(keep)) {
    record <- record[names(record) %in% keep]
  }
  out <- list()
  problems <- character()
  for (name in names(record)) {
    got <- tip_value(record[[name]])
    if (!is.null(got$problem)) {
      problems <- c(problems, paste0("{.code record$", tip_escape(name), "} ", got$problem, "."))
    } else {
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
  big <- names(out)[vapply(
    out,
    function(v) is.numeric(v) && !is.na(v) && abs(v) > 2^53 && abs(v) < 1e21 && v == trunc(v),
    NA
  )]
  if (length(big)) {
    tip_warn(
      c(
        "{.val {big}} {?is a/are} whole number{?s} above 2^53, which cannot be kept exactly.",
        i = "Ids belong in text: use {.code as.character()}."
      ),
      class = "hovercraft_warning_number",
      call = call
    )
  }
  structure(out, names = names(out) %||% character())
}

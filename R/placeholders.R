# The placeholder grammar of a template's text: {{ field }} or {{ field | filter:argument }}. The marks, the filters and
# their limits are the engine's own rules (engine.json), so nothing here is typed twice. The text is read once, left to
# right: up to the first "}}" after each "{{", the field up to the first "|", trimmed, one filter after it. The engine
# shows a placeholder it cannot use as written; here it is an error, so it is found before the page is. The shared
# cases (inst/engine/placeholders.json) hold this parser, the engine's own tests and its build check together.

tip_trim <- function(x) trimws(x, whitespace = "[\\h\\v]")

# The character position of `what` in `text` at or after `from`, or NA.
tip_find <- function(text, what, from) {
  if (from > nchar(text)) {
    return(NA_integer_)
  }
  hit <- regexpr(what, substr(text, from, nchar(text)), fixed = TRUE)
  if (hit < 0L) NA_integer_ else from + as.integer(hit) - 1L
}

# NULL when the words after the filter mark are a filter the engine takes, else list(kind, text).
tip_filter_problem <- function(word) {
  rules <- tip_rules()
  ph <- rules$placeholder
  problem <- function(kind, text) list(kind = kind, text = text)
  if (grepl(ph$filter, word, fixed = TRUE)) {
    return(problem("chained", "one filter only; a placeholder takes no chain of filters"))
  }
  at <- tip_find(word, ph$arg, 1L)
  name <- tip_trim(if (is.na(at)) word else substr(word, 1L, at - 1L))
  arg <- tip_trim(if (is.na(at)) "" else substr(word, at + nchar(ph$arg), nchar(word)))
  if (name %in% unlist(rules$filters_later)) {
    return(problem("later", sprintf("the filter \"%s\" comes with a later version", name)))
  }
  if (!name %in% names(rules$filters)) {
    return(problem(
      "unknown",
      sprintf("no filter \"%s\"; the filters are %s", name, paste(names(rules$filters), collapse = ", "))
    ))
  }
  spec <- rules$filters[[name]]
  if (!is.na(at) || isTRUE(spec$arg_required)) {
    good <- grepl(spec$arg_pattern, arg) && as.numeric(arg) >= spec$min && as.numeric(arg) <= spec$max
    if (!good) {
      return(problem(
        "arg",
        sprintf("\"%s\" takes a whole number from %s to %s after the colon (no quotes)", name, spec$min, spec$max)
      ))
    }
  }
  NULL
}

# What a text's placeholders name and what is wrong with them: list(fields, kinds, sentences). `fields` are the fields
# the placeholders name, in order (a placeholder with a bad filter still names its field; an empty or nested one names
# none); `kinds` the kinds of mistake in order (unclosed, empty, nested, chained, later, unknown, arg); `sentences`
# say each in words.
tip_parse <- function(text) {
  ph <- tip_rules()$placeholder
  fields <- character()
  problems <- list()
  pos <- 1L
  repeat {
    start <- tip_find(text, ph$open, pos)
    if (is.na(start)) {
      break
    }
    end <- tip_find(text, ph$close, start + nchar(ph$open))
    if (is.na(end)) {
      problems[[length(problems) + 1L]] <- list(
        kind = "unclosed",
        text = sprintf(
          "\"%s\" at character %d never closes; add \"%s\", or write the words without it",
          ph$open,
          start,
          ph$close
        )
      )
      break
    }
    inner <- substr(text, start + nchar(ph$open), end - 1L)
    shown <- paste0(ph$open, inner, ph$close)
    mark <- tip_find(inner, ph$filter, 1L)
    name <- tip_trim(if (is.na(mark)) inner else substr(inner, 1L, mark - 1L))
    problem <- NULL
    if (!nzchar(name)) {
      problem <- list(
        kind = "empty",
        text = sprintf(
          "%s names no field; write one, such as %slabel%s",
          shown,
          ph$open,
          ph$close
        )
      )
    } else if (grepl(ph$open, name, fixed = TRUE)) {
      problem <- list(
        kind = "nested",
        text = sprintf(
          "%s holds \"%s\" inside a placeholder; close the first one before opening another",
          shown,
          ph$open
        )
      )
    } else {
      fields <- c(fields, name)
      if (!is.na(mark)) {
        filter <- tip_filter_problem(tip_trim(substr(inner, mark + nchar(ph$filter), nchar(inner))))
        if (!is.null(filter)) {
          problem <- list(kind = filter$kind, text = paste0(shown, ": ", filter$text))
        }
      }
    }
    if (!is.null(problem)) {
      problems[[length(problems) + 1L]] <- problem
    }
    pos <- end + nchar(ph$close)
  }
  list(
    fields = fields,
    kinds = vapply(problems, function(problem) problem$kind, ""),
    sentences = vapply(problems, function(problem) problem$text, "")
  )
}

# Stops with every mistake in the placeholders of `text`. `what` says where the text is ("the text of tip_text()").
tip_check_text <- function(text, what, call = rlang::caller_env()) {
  parsed <- tip_parse(text)
  if (length(parsed$sentences)) {
    tip_abort(
      c(
        "{what} has a placeholder the engine cannot use.",
        tip_bullets(tip_escape(parsed$sentences)),
        i = paste0(
          "A placeholder is {.code ",
          tip_escape("{{ field }}"),
          "} or {.code ",
          tip_escape("{{ field | number:2 }}"),
          "}, with one filter at most."
        )
      ),
      class = "hovercraft_error_placeholder",
      call = call
    )
  }
  invisible(parsed$fields)
}

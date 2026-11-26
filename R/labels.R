# The words the page shows itself, and the one list of keys that names them. The engine has 12 (copy, close, "more when
# pinned" and the others, in `labels.defaults` of engine.json); hovercraft's own scripts add two (the pin button's name
# and the "Less" button). A user changes any of them with `labels = ` on `use_hovercraft()` (the whole page) or
# `tip_on()` (one tip and what is inside it). R checks the keys and writes the words as data; the page script
# (inst/runtime/runtime.js) hands the engine's keys to the engine, which applies them to a tag and everything inside it,
# and answers the scripts of pins and "More" buttons for their own, the nearest scope winning.

# hovercraft's own words, which the engine knows nothing of.
tip_own_labels <- function() c(less = "Less", pin = "Pin this tip")

# Every key and its built-in word: the engine's, read from engine.json, then hovercraft's own. This is the only list.
tip_label_defaults <- function() c(unlist(tip_rules()$labels$defaults), tip_own_labels())

# The attribute that holds a scope's words as JSON, named after the engine's prefix like the other attributes of the
# package's own scripts.
tip_labels_attribute <- function() paste0("data-", tip_names()$prefix, "-labels")

# What the help pages say about `labels`: every key with its built-in word, made from the list above so it cannot drift.
tip_label_help <- function() {
  defaults <- tip_label_defaults()
  paste0("`", names(defaults), "` (`", defaults, "`)", collapse = ", ")
}

# The words as a named list, or NULL for none. A named character vector, or a list of strings, whose names are keys of
# `tip_label_defaults()`; a `{label}` in a word is left as written, because the engine fills it in.
tip_check_labels <- function(labels, arg = "labels", call = rlang::caller_env()) {
  if (is.null(labels)) {
    return(NULL)
  }
  if (is.list(labels) && all(vapply(labels, function(x) is.character(x) && length(x) == 1L, TRUE))) {
    labels <- unlist(labels)
  }
  result <- checkmate::check_character(labels, any.missing = FALSE, min.chars = 1L, min.len = 1L, names = "unique")
  if (isTRUE(result) && !all(nzchar(trimws(labels)))) {
    result <- "Must not hold a blank word."
  }
  tip_arg(
    result,
    arg,
    paste0(
      "Give a named character vector of words, such as {.code c(close = \"Dismiss\")}, with a different name for ",
      "each, or {.code NULL} for the built-in words."
    ),
    call
  )
  keys <- names(tip_label_defaults())
  unknown <- setdiff(names(labels), keys)
  if (length(unknown)) {
    # The closest key, when one is a typo away: "clsoe" is "close".
    closest <- vapply(unknown, tip_closest_label, "", keys = keys)
    guesses <- tip_escape(
      paste0("Did you mean \"", closest[nzchar(closest)], "\" for \"", unknown[nzchar(closest)], "\"?")
    )
    tip_abort(
      c(
        paste0(
          "{.arg ",
          arg,
          "} has ",
          if (length(unknown) > 1L) "keys that are not labels" else "a key that is not a label",
          ": {.val {unknown}}."
        ),
        stats::setNames(guesses, rep("i", length(guesses))),
        i = "The keys are {.val {keys}}."
      ),
      class = "hovercraft_error_labels",
      call = call
    )
  }
  as.list(labels)
}

# The key nearest to `key` when it is at most two edits away, else "".
tip_closest_label <- function(key, keys) {
  distance <- drop(utils::adist(key, keys))
  if (min(distance) <= 2) keys[[which.min(distance)]] else ""
}

# `use_hovercraft(labels = )` as a dependency of data, named by its words so that two calls with other words are two
# dependencies: the runtime script merges them in the order they come, a later word over an earlier one.
tip_labels_dependency <- function(labels) {
  tip_gen_dependency(
    paste0("hovercraft-labels-", substr(rlang::hash(labels), 1L, 12L)),
    ops = list(list(op = "labels", labels = labels))
  )
}

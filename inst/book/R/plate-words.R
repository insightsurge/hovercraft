# Section "words": the words a tip shows itself (Copy, Copied, Close, More, Less, the pin button), for the whole page
# (`use_hovercraft(labels =)`) or for one tag and the tips inside it (`tip_on(labels =)`).

words_sets <- c("the built-in words" = "built_in", "other English words" = "english", "Dutch words" = "dutch")

# Each set names the keys it changes; a key left out keeps its built-in word. {label} is filled in by the page.
words_values <- list(
  english = c(
    copy = "Copy code",
    copied = "Code copied",
    close = "Dismiss",
    more = "Show all",
    less = "Show less",
    pin = "Keep this tip open",
    pinned = "{label}: details kept open. Tab to reach them."
  ),
  dutch = c(
    copy = "Kopieer code",
    copied = "Gekopieerd",
    close = "Sluiten",
    more = "Meer",
    less = "Minder",
    pin = "Tip vastzetten",
    pinned = "{label}: details vastgezet. Tab om ze te bereiken."
  )
)

# The words as code, `words <- c(copy = "Copy code", ...)`, or NULL for the built-in ones.
words_vector_code <- function(set) {
  if (set == "built_in") {
    return(NULL)
  }
  values <- words_values[[set]]
  paste0("words <- ", do.call(code_call, c(list("c"), lapply(values, code_str), list(width = 0L))))
}

# The figure as code: one template, and two tags that use it. The first carries the words; the second does not.
words_figure_code <- function(set) {
  tag <- function(name, id, own, more) {
    row <- book_columns[book_columns$name == name, c("name", "about", "snippet")]
    code_call(
      "tip_on",
      code_word(name, class = "col-chip"),
      template = code_str("words_column"),
      record = code_list(as.list(row)),
      more = code_str(more),
      pin = "TRUE",
      id = code_str(id),
      events = code_chr(c("pin", "copy")),
      labels = if (own && set != "built_in") "words"
    )
  }
  define <- code_call(
    "tip_define",
    words_column = code_call(
      "tip_template",
      head = code_call("tip_title", code_str("{{ name }}")),
      body = code_call(
        "list",
        code_call("tip_text", code_str("{{ about }}")),
        code_call("tip_code", code_str("snippet"), language = code_str("r"), copy = "TRUE")
      )
    )
  )
  paste(
    c(
      define,
      "",
      if (set == "built_in") "# Two tags with a tip" else "# The first tag carries its words, and the tips in it",
      code_call(
        "htmltools::div",
        tag("wait_min", "words_wait_min", TRUE, "**Business hours only.** Nights and weekends do not count."),
        tag("region", "words_region", FALSE, "**Regions.** North, Coast and Hills, as the branches report them."),
        class = code_str("cols")
      )
    ),
    collapse = "\n"
  )
}

# The page's line, shown and not run: the engine merges a page's words into the whole page's, so every other section of
# the book would change with it.
words_page_code <- function(set) {
  if (set == "built_in") {
    return("# No words given: every tip says Copy, Copied, Close, More, Less and \"Pin this tip\"")
  }
  paste0(
    "# Once in the page's UI: the words of every tip on the page, such as a page in another language\n",
    code_call("use_hovercraft", labels = "words")
  )
}

words_code <- function(settings) {
  set <- choice(settings$words, words_sets, "built_in")
  vector <- words_vector_code(set)
  paste(c(vector, if (!is.null(vector)) "", words_page_code(set), "", words_figure_code(set)), collapse = "\n")
}

# What the figure draws: the words, then the template and the tags. The page's line is left out.
words_run_code <- function(set) {
  paste(c(words_vector_code(set), words_figure_code(set)), collapse = "\n")
}

words_steps <- function() {
  list(
    step(
      "Rest on *wait_min*: the button at the bottom of the tip says More, or the word you chose.",
      "the button keeps its built-in word.",
      tip_target("words_wait_min"),
      on_hint("words_wait_min")
    ),
    step(
      "Press it: the tip grows, and the button now says Less, or your word for it.",
      "it says More again, or the tip closes.",
      tip_target("words_wait_min")
    ),
    step(
      "Pin *wait_min*: the pin button's name and the Close button use your words; `input$words_wait_min_pin` arrives.",
      "the pinned tip keeps the built-in Close.",
      tip_target("words_wait_min"),
      on_pin("words_wait_min")
    ),
    step(
      "Press Copy in the pinned tip: it says your word, then your Copied, and `input$words_wait_min_copy` arrives.",
      "the button says Copy after a different word was chosen.",
      tip_target("words_wait_min"),
      on_copy("words_wait_min")
    ),
    step(
      "Rest on *region*: it has no words of its own, so it keeps the page's, here the built-in ones.",
      "it takes *wait_min*'s words.",
      tip_target("words_region"),
      on_hint("words_region")
    )
  )
}

plate_words_ui <- function() {
  plate(
    "words",
    lede = htmltools::tagList(
      "A tip says a few words itself: Copy, Copied, Close, More, Less, and the name of the pin button. Change them",
      "for a page in another language, or for one place. The page's words go in",
      htmltools::tags$code("use_hovercraft(labels =)"),
      "; one tag's words, and those of every tip inside it, in",
      htmltools::tags$code("tip_on(labels =)"),
      "."
    ),
    caption = paste(
      "Two columns. The first carries the words. The page's line is shown and not run: it would change every other",
      "section of this book."
    ),
    steps = words_steps(),
    code = code_text(words_code(list())),
    log_empty = "Nothing yet. Pin a column.",
    controls = htmltools::tagList(
      "Give the first column",
      choice_set("words_words", "Words", words_sets)
    )
  )
}

plate_words_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "words", words_steps())
  set <- shiny::reactive(choice(input$words_words, words_sets, "built_in"))
  plate_code(
    input,
    output,
    session,
    "words",
    ui_code = shiny::reactive(words_code(list(words = set()))),
    figure_code = shiny::reactive(words_run_code(set()))
  )
  ids <- c("words_wait_min", "words_region")
  log_inputs(input, session, "words", c(paste0(ids, "_pin"), paste0(ids, "_pinned"), paste0(ids, "_copy")))
  ticks
}

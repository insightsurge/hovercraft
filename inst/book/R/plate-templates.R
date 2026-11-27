# Section "templates": the kinds of template, each filled in from a made-up record, kept either by name with
# tip_define() or made on the tag, and plain words that carry {{ field }} too. Its number and title are in contents.R.

templates_kinds <- c(
  "one line" = "line",
  "title and text" = "titled",
  "fields" = "fields",
  "a status badge" = "badge",
  "numbers" = "number",
  "head, body and foot" = "parts",
  "steps" = "steps",
  "plain words with fields" = "words"
)

# Where the template lives: named once for the page, or made on the tag it belongs to.
templates_how <- c("named once with tip_define()" = "named", "made on the tag" = "tag")

# The template and the record of each kind, as code.
templates_kind_code <- function(kind) {
  text <- function(x) code_call("tip_text", code_str(x))
  switch(
    kind,
    line = list(
      name = "ticket_line",
      template = code_call("tip_template", text("{{ note }}")),
      record = list(note = "Waits on the payment provider.")
    ),
    titled = list(
      name = "ticket_titled",
      template = code_call("tip_template", code_call("tip_title", code_str("{{ title }}")), text("{{ note }}")),
      record = list(title = "Booking page times out", note = "Waits on the payment provider.")
    ),
    fields = list(
      name = "ticket_fields",
      template = code_call(
        "tip_template",
        code_call("tip_title", code_str("{{ key }}")),
        code_call(
          "tip_fields",
          Owner = code_str("{{ owner }}"),
          Region = code_str("{{ region }}"),
          Opened = code_str("{{ opened }}")
        )
      ),
      record = list(key = "SUP-2291", owner = "R. Okafor", region = "Coast", opened = "2026-11-16")
    ),
    badge = list(
      name = "ticket_badge",
      template = code_call(
        "tip_template",
        head = code_call(
          "list",
          code_call("tip_title", code_str("{{ key }}")),
          code_call("tip_badge", code_str("{{ status }}"), status = code_str("{{ tone }}"))
        )
      ),
      record = list(key = "SUP-2291", status = "Blocked", tone = "warning")
    ),
    number = list(
      name = "week_numbers",
      template = code_call(
        "tip_template",
        code_call("tip_title", code_str("Visits, week 46")),
        text("**{{ visits | number:2 }}** visits, {{ share | percent }} above the target."),
        text("Median wait: {{ wait | number:2 }} h")
      ),
      record = list(visits = 4812.4, share = 0.0634, wait = NULL)
    ),
    parts = list(
      name = "ticket_parts",
      template = code_call(
        "tip_template",
        head = code_call("tip_title", code_str("{{ key }}: {{ title }}")),
        body = code_call("tip_fields", Owner = code_str("{{ owner }}"), Region = code_str("{{ region }}")),
        foot = text("{{ note }}")
      ),
      record = list(
        key = "SUP-2291",
        title = "Booking page times out",
        owner = "R. Okafor",
        region = "Coast",
        note = "Waits on the payment provider."
      )
    ),
    steps = list(
      name = "ticket_steps",
      template = code_call(
        "tip_template",
        code_call("tip_title", code_str("Reopen {{ key }}")),
        text(paste0(
          "1. Check the payment provider's status page.\n",
          "2. Ask the owner for the last log line.\n",
          "3. Reply to the reader."
        ))
      ),
      record = list(key = "SUP-2291")
    ),
    words = list(
      name = NULL,
      template = NULL,
      record = list(key = "SUP-2291", owner = "R. Okafor", region = "Coast")
    )
  )
}

templates_code <- function(settings) {
  kind <- choice(settings$kind, templates_kinds, "line")
  how <- choice(settings$how, templates_how, "named")
  built <- templates_kind_code(kind)
  words <- identical(kind, "words")
  named <- identical(how, "named") && !words
  note <- switch(
    kind,
    number = "# A missing value (here wait = NULL) shows a dash",
    steps = "# A workaround: the engine has no steps block, so a list in a text block does the job",
    badge = "# The badge's status comes from the record: one of success, warning, danger, info",
    words = "# No template: text fills {{ field }} from the record (without a record the braces stay as written)",
    NULL
  )
  define <- if (named) {
    do.call(code_call, c(list("tip_define"), stats::setNames(list(built$template), built$name)))
  }
  use <- code_call(
    "tip_on",
    code_word("SUP-2291"),
    if (words) code_str("{{ key }} is with {{ owner }}, on the {{ region }} desk."),
    template = if (named) code_str(built$name) else built$template,
    record = code_list(built$record),
    marker = code_str("underline"),
    id = code_str("templates_tip"),
    events = code_str("hint")
  )
  paste(
    c(
      if (named) c("# Once in the page's UI: the template, named", note, define, ""),
      if (!named && !words) "# No tip_define(): the template is made on the tag and travels with it",
      if (!named) note,
      "# One tag and its record",
      code_sentence("The ticket", use, "is the oldest open one.")
    ),
    collapse = "\n"
  )
}


templates_steps <- function() {
  list(
    step(
      "Rest on the ticket with *one line*: a single sentence from the record, and no title.",
      "`{{ ... }}` shown as typed.",
      tip_target("templates_tip"),
      on_hint("templates_tip", when = chose("templates_kind", "line", "line"))
    ),
    step(
      paste(
        "Choose *a status badge*, then rest on the ticket: the badge says Blocked in the colour of a warning, and",
        "its word says the same."
      ),
      "the badge's colour is the only sign of its status.",
      "#templates_kind",
      on_hint("templates_tip", when = chose("templates_kind", "badge", "line"))
    ),
    step(
      paste(
        "Choose *numbers*, then rest on the ticket: 4812.40 visits, 6% above the target, and a dash where the wait",
        "is missing."
      ),
      "it shows NA, null or undefined.",
      "#templates_kind",
      on_hint("templates_tip", when = chose("templates_kind", "number", "line"))
    ),
    step(
      paste(
        "Choose *plain words with fields*, then rest on the ticket: the sentence has the key, owner and region in",
        "it, with no template and no `tip_define()` in the code."
      ),
      "`{{ key }}` shown as typed, although a record is given.",
      "#templates_kind",
      on_hint("templates_tip", when = chose("templates_kind", "words", "line"))
    ),
    step(
      paste(
        "Choose *made on the tag* (with a kind that has a template), then rest on the ticket: the tip is the same as",
        "when named, and the code has no `tip_define()`."
      ),
      "the tip is empty, or the page says the template is not defined.",
      "#templates_how",
      on_hint("templates_tip", when = function(input) identical(input$templates_how, "tag"))
    )
  )
}

plate_templates_ui <- function() {
  plate(
    "templates",
    lede = inline_code(paste(
      "A template names the parts of a tip: title, text, badge, fields, code and live output.",
      "Text is markdown, so a list or steps go in a text block, and a link is a link only in a pinned tip.",
      "Name it once with `tip_define()` and fill it from a record, or make it on the tag, where it travels with the",
      "tag. A record's values show as written, never as markdown. Plain words need no template: `text` fills",
      "`{{ field }}` from `record`, and without a record the braces stay as written."
    )),
    caption = paste(
      "One tag, one record, eight ways to build the tip. The engine has no sparkline, image or progress bar block:",
      "a workaround is tip_output(), in section 5.2."
    ),
    steps = templates_steps(),
    code = code_text(templates_code(list())),
    log_empty = "Nothing yet. Rest on the ticket.",
    controls = htmltools::tagList(
      "Build the tip as",
      choice_set("templates_kind", "Kind", templates_kinds),
      "and keep the template",
      choice_set("templates_how", "Template", templates_how)
    )
  )
}

plate_templates_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "templates", templates_steps())
  code <- shiny::reactive(templates_code(list(kind = input$templates_kind, how = input$templates_how)))
  plate_code(input, output, session, "templates", code)
  log_inputs(input, session, "templates", "templates_tip_hint")
  ticks
}

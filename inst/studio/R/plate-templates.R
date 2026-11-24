# Plate 3, Templates: a named template with a head, a body and a foot, filled in from one record, with a status badge.

templates_records <- c("SUP-2291, blocked" = "SUP-2291", "SUP-2304, no owner yet" = "SUP-2304")
templates_slots <- c("head, body and foot" = "all", "head and body" = "no_foot")
templates_how <- c("named once with tip_define()" = "named", "made on the tag" = "tag")

templates_code <- function(settings) {
  key <- choice(settings$record, templates_records, "SUP-2291")
  slots <- choice(settings$slots, templates_slots, "all")
  how <- choice(settings$how, templates_how, "named")
  record <- studio_tickets[[key]]
  record_code <- code_list(record)
  template <- code_call(
    "tip_template",
    head = code_call(
      "list",
      code_call("tip_title", code_str("{{ key }}: {{ title }}")),
      code_call("tip_badge", code_str("{{ status }}"), status = code_str("{{ tone }}"))
    ),
    body = code_call("tip_fields", Owner = code_str("{{ owner }}"), Region = code_str("{{ region }}")),
    foot = if (slots == "all") code_call("tip_text", code_str("{{ note }}"))
  )
  define <- code_call("tip_define", ticket = template)
  use <- code_call(
    "tip_on",
    code_call("htmltools::tags$span", code_str(key), tabindex = code_str("0")),
    template = if (how == "named") code_str("ticket") else template,
    record = record_code,
    marker = code_str("underline"),
    id = code_str("ticket"),
    events = code_str("hint")
  )
  paste(
    c(
      if (how == "named") c("# Once in the page's UI", define, ""),
      "# One tag and its record: a field that is NULL shows a dash",
      code_call(
        "htmltools::p",
        code_str("The ticket"),
        use,
        code_str("is the oldest open one."),
        class = code_str("specimen-text")
      )
    ),
    collapse = "\n"
  )
}

templates_steps <- function() {
  list(
    step(
      "Rest on the ticket: a title and a badge in the head, two fields in the body, a note in the foot.",
      "a missing block, or `{{ ... }}` shown as typed.",
      tip_target("ticket"),
      on_hint("ticket")
    ),
    step(
      "Choose *SUP-2304, no owner yet*: Owner shows a dash.",
      "it shows NA, null or undefined.",
      "#templates_record",
      on_hint("ticket", when = function(input) identical(input$templates_record, "SUP-2304"))
    ),
    step(
      "Switch the page to dark: the badge stays easy to read.",
      "the badge's words fade into its fill.",
      tip_target("ticket"),
      on_hint("ticket", when = function(input) identical(input$page_mode, "dark"))
    )
  )
}

plate_templates_ui <- function() {
  plate(
    "templates",
    3,
    "Templates",
    lede = "A named template with a head, a body and a foot, filled in from one record, with a status badge.",
    caption = "One record, three slots: title and badge in the head, fields in the body, a note in the foot.",
    steps = templates_steps(),
    code = code_text(templates_code(list())),
    log_empty = "Nothing yet. Rest on the ticket.",
    controls = htmltools::tagList(
      "Fill the template from",
      choice_set("templates_record", "Record", templates_records),
      "with",
      choice_set("templates_slots", "Slots", templates_slots),
      "and keep it",
      choice_set("templates_how", "Template", templates_how)
    )
  )
}

plate_templates_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "templates", templates_steps())
  code <- shiny::reactive(templates_code(list(
    record = input$templates_record,
    slots = input$templates_slots,
    how = input$templates_how
  )))
  plate_code(input, output, session, "templates", code)
  log_inputs(input, session, "templates", "ticket_hint")
  ticks
}

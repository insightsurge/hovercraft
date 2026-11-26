# Section "markers": the cue on a tipped tag that says there is a tip (`tip_on(marker =)`, `tip_marker()`).

markers_kinds <- c("an underline" = "underline", "an icon" = "icon", "a corner" = "corner", "no marker" = "none")
markers_lines <- c(
  "the preset's own" = "preset",
  "dotted" = "dotted",
  "dashed" = "dashed",
  "wavy" = "wavy"
)
markers_icons <- c("the built-in icon" = "builtin", "a ? of my own" = "own")

# The marker of the phrase, as code: a word where a word is enough, tip_marker() where a line or an icon is chosen.
markers_phrase_marker <- function(kind, line, icon) {
  if (kind == "underline" && line != "preset") {
    return(code_call("tip_marker", code_str("underline"), line = code_str(line)))
  }
  if (kind == "icon" && icon == "own") {
    return(
      code_call(
        "tip_marker",
        code_str("icon"),
        icon = code_call("htmltools::tags$span", code_str("?"))
      )
    )
  }
  code_str(kind)
}

markers_code <- function(settings) {
  kind <- choice(settings$kind, markers_kinds, "underline")
  line <- choice(settings$line, markers_lines, "preset")
  icon <- choice(settings$icon, markers_icons, "builtin")
  phrase <- code_call(
    "tip_on",
    code_word("4,812 visits"),
    code_str("Visits that ended in a booking or a reply, Monday to Sunday."),
    marker = markers_phrase_marker(kind, line, icon),
    id = code_str("markers_phrase"),
    events = code_str("hint")
  )
  heading <- code_call(
    "tip_on",
    code_word("Visits by region"),
    code_str("One row for each region, summed over the week."),
    marker = code_str("icon"),
    id = code_str("markers_heading"),
    events = code_str("hint")
  )
  box <- code_call(
    "tip_on",
    code_call("htmltools::div", code_str("North: 4,812"), class = code_str("target"), tabindex = code_str("0")),
    code_str("The largest region this week."),
    marker = code_str("corner"),
    id = code_str("markers_box"),
    events = code_str("hint")
  )
  code_call(
    "htmltools::div",
    code_sentence("Last week the North region logged", phrase, "from walk-in and online bookings."),
    code_call("htmltools::p", heading, class = code_str("strip-title")),
    box
  )
}

markers_steps <- function() {
  list(
    step(
      paste(
        "Rest on *4,812 visits* with the underline: a line under the words, in the preset's own style or the one",
        "you chose, and while the tip is open the words take a soft tint."
      ),
      "the line runs across the whole paragraph, or is not in the text's colour.",
      tip_target("markers_phrase"),
      on_hint("markers_phrase", when = chose("markers_kind", "underline", "underline"))
    ),
    step(
      "Choose *an icon*, then rest on the phrase: a small icon follows the words, the built-in one or your ?.",
      "the icon is read out by a screen reader, or it sits on the next line.",
      tip_target("markers_phrase"),
      on_hint("markers_phrase", when = chose("markers_kind", "icon", "underline"))
    ),
    step(
      "Choose *a corner*, then rest on the phrase: a small triangle sits in the top corner of the words.",
      "no mark, or the triangle covers a letter.",
      tip_target("markers_phrase"),
      on_hint("markers_phrase", when = chose("markers_kind", "corner", "underline"))
    ),
    step(
      paste(
        "Switch between underline, corner and no marker: the words do not move by a pixel. The icon takes a little",
        "room, so only the words after it shift."
      ),
      "the text reflows when an underline or a corner comes or goes.",
      "#markers .specimen-text"
    ),
    step(
      paste(
        "Rest on the heading and on the box: an icon after one, a corner on the other, whatever you chose for the",
        "phrase."
      ),
      "a tip opens without a cue, or the heading takes the phrase's marker.",
      "#markers .target",
      on_hint(c("markers_heading", "markers_box"))
    )
  )
}

plate_markers_ui <- function() {
  plate(
    "markers",
    lede = htmltools::tagList(
      htmltools::tags$code("marker ="),
      "puts a cue on a tipped tag, so a reader knows to rest on it. Leave it out and the tag shows no cue;",
      htmltools::tags$code("\"none\""),
      "says so on purpose. A word is enough for the common cases, and",
      htmltools::tags$code("tip_marker()"),
      "sets the line of an underline or gives your own icon."
    ),
    caption = paste(
      "One phrase takes the marker you choose. The heading always takes an icon, and the box a corner, as a",
      "spreadsheet marks a note."
    ),
    steps = markers_steps(),
    code = code_text(markers_code(list())),
    log_empty = "Nothing yet. Rest on the phrase.",
    controls = htmltools::tagList(
      "Mark the phrase with",
      choice_set("markers_kind", "Marker", markers_kinds),
      "and draw any underline in",
      choice_set("markers_line", "Line", markers_lines),
      "style, and any icon as",
      choice_set("markers_icon", "Icon", markers_icons)
    )
  )
}

plate_markers_server <- function(input, output, session) {
  ticks <- steps_server(input, session, "markers", markers_steps())
  code <- shiny::reactive(
    markers_code(list(kind = input$markers_kind, line = input$markers_line, icon = input$markers_icon))
  )
  plate_code(input, output, session, "markers", code)
  log_inputs(input, session, "markers", c("markers_phrase_hint", "markers_heading_hint", "markers_box_hint"))
  ticks
}

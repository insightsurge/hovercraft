# Which attributes tip_on() writes: the engine's attribute name, and the same with -template and -record.
tip_attributes <- function() {
  base <- tip_names()$attribute
  c(text = base, template = paste0(base, "-template"), record = paste0(base, "-record"))
}

# A tip shows when the pointer rests on a tag or keyboard focus arrives on it, so a tag that cannot take focus gives a
# keyboard user no way to the tip. This only says so, once; it never adds a tabindex (a table of 3,000 rows would become
# 3,000 Tab stops), because the choice of which tags are worth a Tab stop is the page's. A tag is fine when something
# inside it can take focus: Shiny wraps an input in a `div`, and focus on the input reaches the tip.
tip_can_focus <- function(tag) {
  type <- htmltools::tagGetAttribute(tag, "type")
  switch(
    tag$name,
    button = ,
    select = ,
    textarea = ,
    summary = TRUE,
    a = ,
    area = !is.null(htmltools::tagGetAttribute(tag, "href")),
    input = !identical(tolower(as.character(type)), "hidden"),
    !is.null(htmltools::tagGetAttribute(tag, "tabindex"))
  ) ||
    !is.null(htmltools::tagGetAttribute(tag, "tabindex"))
}

# `x` and every tag inside it, as a flat list: `x` is a tag, a tag list, a plain list or text (which holds no tag).
tip_tags_in <- function(x) {
  if (inherits(x, "shiny.tag")) {
    return(c(list(x), Reduce(c, lapply(x$children, tip_tags_in), list())))
  }
  if (is.list(x) && !inherits(x, "html_dependency")) {
    return(Reduce(c, lapply(x, tip_tags_in), list()))
  }
  list()
}

tip_check_focus <- function(tag, call) {
  if (!any(vapply(tip_tags_in(tag), tip_can_focus, NA))) {
    tip_warn(
      c(
        "A {.code <{tag$name}>} tag cannot take keyboard focus, so a keyboard user will not reach its tip.",
        i = "Add {.code tabindex = \"0\"} to the tag, or put the tip on a button or a link.",
        i = "{.fn tip_on} never adds a tab stop itself."
      ),
      class = "hovercraft_warning_focus",
      call = call,
      .frequency = "once",
      .frequency_id = "hovercraft-focus"
    )
  }
  invisible()
}

# Visible words inside `x`: any text that is not blank, or an image with alt text. A fixed icon has none.
tip_has_words <- function(x) {
  if (inherits(x, "shiny.tag")) {
    alt <- htmltools::tagGetAttribute(x, "alt")
    return((identical(x$name, "img") && !is.null(alt) && nzchar(alt)) || tip_has_words(x$children))
  }
  if (is.list(x) && !inherits(x, "html_dependency")) {
    return(any(vapply(x, tip_has_words, NA)))
  }
  if (is.character(x) || is.numeric(x)) {
    # `HTML()` text is markup: only what is left outside the angle brackets is read.
    return(any(nzchar(trimws(gsub("<[^>]*>|&[a-z#0-9]+;", "", as.character(x))))))
  }
  FALSE
}

# A button with no visible words and no label of its own has no accessible name; a screen reader says only "button".
tip_needs_label <- function(tag) {
  is_button <- identical(tag$name, "button") || identical(htmltools::tagGetAttribute(tag, "role"), "button")
  is_button &&
    is.null(htmltools::tagGetAttribute(tag, "aria-label")) &&
    is.null(htmltools::tagGetAttribute(tag, "aria-labelledby")) &&
    !tip_has_words(tag$children)
}

# The words of a markdown tip: markers removed, one line, so a screen reader reads what a sighted user reads.
tip_plain_text <- function(text) {
  text <- gsub("!?\\[([^]]*)\\]\\([^)]*\\)", "\\1", text, perl = TRUE)
  text <- gsub("(?m)^\\s*([-*+]|[0-9]+[.)])\\s+", "", text, perl = TRUE)
  text <- gsub("(\\*\\*|__|\\*|`)", "", text)
  text <- gsub("(?<![[:alnum:]])_([^_]+)_(?![[:alnum:]])", "\\1", text, perl = TRUE)
  trimws(gsub("\\s+", " ", text))
}

# Whether tip_define() has seen this template in this session. A name it has not seen is the page's to report (a UI is
# often written before the templates it uses), so it warns once and says FALSE; with no template defined at all there is
# nothing to compare with and the answer is FALSE without a word.
tip_template_seen <- function(template, call) {
  known <- tip_known()
  if (!length(known)) {
    return(FALSE)
  }
  if (!template %in% names(known)) {
    near <- tip_near(template, names(known))
    tip_warn(
      c(
        "No template {.val {template}} has been defined with {.fn tip_define}.",
        i = if (nzchar(near)) trimws(near) else "The templates defined so far are {.val {names(known)}}.",
        i = "If it is defined later in the page, ignore this."
      ),
      class = "hovercraft_warning_template",
      call = call,
      .frequency = "once",
      .frequency_id = paste0("hovercraft-template-", template)
    )
    return(FALSE)
  }
  TRUE
}

# What tip_on() can check in R about the template it was given: its name against the ones defined in this session, and
# the fields it names against the record (`held`: the names the record has). Only a template tip_define() has seen can
# be checked; any other name is the page's to report (a UI is often written before the templates it uses). This page
# has no list of items, so {{ tip_index }} has nothing to count, and {{ tip_id }} is the tag's own id.
tip_check_use <- function(template, held, id, call) {
  if (!tip_template_seen(template, call)) {
    return(invisible())
  }
  known <- tip_known()
  fields <- known[[template]]$fields
  engine <- unlist(tip_rules()$engine_fields)
  if ("tip_index" %in% fields && !"tip_index" %in% held) {
    tip_abort(
      c(
        "The template {.val {template}} names {.code tip_index}, which has nothing to count here.",
        i = paste0(
          "It comes with a later version, with tips that belong to a list. ",
          "Give the record a value of that name for now."
        )
      ),
      class = "hovercraft_error_field",
      call = call
    )
  }
  if ("tip_id" %in% fields && !"tip_id" %in% held && is.null(id)) {
    tip_abort(
      c(
        "The template {.val {template}} names {.code tip_id}, which is the tag's own id, and the tag has none.",
        i = "Give the tag an {.arg id}, or put a value named {.code tip_id} in the record."
      ),
      class = "hovercraft_error_field",
      call = call
    )
  }
  missing <- setdiff(fields, c(held, engine))
  if (length(missing)) {
    holds <- if (length(held)) "The record holds {.val {held}}." else "No record was given."
    tip_abort(
      c(
        "The template {.val {template}} names fields the record does not hold.",
        x = "Missing: {.val {missing}}.",
        i = holds
      ),
      class = "hovercraft_error_field",
      call = call
    )
  }
  clash <- intersect(intersect(fields, engine), held)
  if (length(clash)) {
    tip_warn(
      c(
        "The record has {.val {clash}}, which the engine also fills in.",
        i = "The record's own value is shown; rename the column if you meant the engine's."
      ),
      class = "hovercraft_warning_engine_name",
      call = call
    )
  }
  invisible()
}

# Parses `more =` on tip_on(): NULL / NA / "" -> NULL; a plain string without placeholders -> list(text = ..., fields =
# character()); a string with placeholders or a tip_template() -> list(template = ..., fields = ...).
tip_parse_more <- function(more, call) {
  if (is.null(more)) {
    return(NULL)
  }
  if (inherits(more, "hovercraft_template")) {
    return(list(template = more, fields = tip_template_fields(more)))
  }
  if (is.atomic(more) && length(more) == 1L && is.na(more)) {
    return(NULL)
  }
  if (is.character(more) && length(more) == 1L) {
    if (!nzchar(more)) {
      return(NULL)
    }
    fields <- tip_check_text(more, "The text of `more`", call = call)
    if (length(fields)) {
      return(list(template = tip_template(tip_text(more)), fields = unique(fields)))
    }
    return(list(text = more, fields = character()))
  }
  tip_abort(
    c(
      "{.arg more} must be a string or a {.fn tip_template}, not {.obj_type_friendly {more}}.",
      i = "Give the words of the long form (with placeholders if you like), a {.fn tip_template}, or {.code NULL}."
    ),
    class = "hovercraft_error_argument",
    call = call
  )
}

# Deterministic HTML dependency defining an inline `more` template (no random or per-session ids, safe for bindCache()).
tip_more_template_dep <- function(template) {
  body <- tip_json(tip_blocks_plain(template))
  hash <- substr(rlang::hash(body), 1L, 12L)
  name <- paste0("more_", hash)
  js <- sprintf("%s.define(%s, %s);", tip_names()$global, tip_json(name), body)
  for (out_id in tip_output_ids(list(template))) {
    js <- c(js, sprintf("%sShiny.park(%s);", tip_names()$global, tip_json(out_id)))
  }
  park_tags <- tip_output_tags(list(template))
  park_html <- if (length(park_tags)) {
    as.character(htmltools::tags$div(
      class = tip_output_park_class(),
      hidden = NA,
      `aria-hidden` = "true",
      `data-hovercraft-keep` = "",
      park_tags
    ))
  } else {
    ""
  }
  dep <- htmltools::htmlDependency(
    name = paste0("hovercraft-more-", hash),
    version = tip_engine_version(),
    src = "engine/www",
    package = "hovercraft",
    head = paste0("<script>", paste(js, collapse = "\n"), "</script>", park_html),
    all_files = FALSE
  )
  extra_deps <- if (length(park_tags)) htmltools::findDependencies(park_tags) else list()
  list(name = name, deps = c(list(dep), extra_deps))
}

# Checks fields named by a `more` template against `record`, matching tip_check_use().
tip_check_more_fields <- function(fields, held, id, call) {
  if (!length(fields)) {
    return(invisible())
  }
  engine <- unlist(tip_rules()$engine_fields)
  if ("tip_index" %in% fields && !"tip_index" %in% held) {
    tip_abort(
      c(
        "{.arg more} names {.code tip_index}, which has nothing to count here.",
        i = "Give the record a value of that name for now."
      ),
      class = "hovercraft_error_field",
      call = call
    )
  }
  if ("tip_id" %in% fields && !"tip_id" %in% held && is.null(id)) {
    tip_abort(
      c(
        "{.arg more} names {.code tip_id}, which is the tag's own id, and the tag has none.",
        i = "Give the tag an {.arg id}, or put a value named {.code tip_id} in the record."
      ),
      class = "hovercraft_error_field",
      call = call
    )
  }
  missing <- setdiff(fields, c(held, engine))
  if (length(missing)) {
    holds <- if (length(held)) "The record holds {.val {held}}." else "No record was given."
    tip_abort(
      c(
        "{.arg more} names fields the record does not hold.",
        x = "Missing: {.val {missing}}.",
        i = holds
      ),
      class = "hovercraft_error_field",
      call = call
    )
  }
  invisible()
}

#' Put a tip on a tag
#'
#' `r lifecycle::badge("experimental")`
#' Gives one HTML tag a tip that shows after the pointer rests on it, or keyboard focus arrives, and hides when either
#' leaves. The tip is plain text, or drawn from a named template ([tip_define()]) filled in from a record: the values of
#' one item, such as a row of a table. The engine's script and CSS are attached to the tag, so it works in a bslib page,
#' in a plain Shiny page and in output made later with `renderUI()`. A tag inside [bslib::tooltip()] or
#' [bslib::popover()], or with `data-bs-toggle`, keeps the tip that bslib draws and this one is left off.
#'
#' A tip shows on keyboard focus as well as on a pointer, so a tag that cannot take focus (a table row, a `div`) gives a
#' keyboard user no way to the tip. `tip_on()` looks at the tag and at everything inside it, so the `div` that Shiny
#' puts around an input is fine: the input takes focus. It warns once about a tag with nothing focusable in it, and
#' never adds a `tabindex` itself, because a table of 3,000 rows would become 3,000 Tab stops: give the tags worth a
#' stop `tabindex = "0"`, or put the tip on a button or a link.
#'
#' A button with no visible words, such as one that holds only an icon, has no name for a screen reader. When it also
#' has no `aria-label` or `aria-labelledby` of its own, and the tip is plain `text`, `tip_on()` gives it an
#' `aria-label` made from the text, with the markdown marks taken out. A tip drawn from a template gives no label,
#' because its words depend on the record: set `aria-label` yourself. The engine reads the tip once: when a tag's
#' name, from `aria-label`, `aria-labelledby` or its own words, is the same as the tip's words, the tip is not also
#' given to a screen reader as a description.
#'
#' If both `text` and `template` are given, the text is shown for this tag. A template whose name has been defined in
#' this session is checked against the record here: a field it names that the record does not hold is an error. A
#' name that has not been defined is left to the page, which says so once in the browser's console.
#'
#' @param tag One HTML tag, such as `htmltools::tags$tr()` or `shiny::actionButton()`.
#' @param text One string of markdown (strong, emphasis, code, links, short lists), or `NULL`. It is shown as written:
#'   build it with [paste0()] or [sprintf()]. `NA` and `""` count as none: the tag comes back as it was.
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param template The name of a template defined with [tip_define()], or `NULL`.
#' @param record A named list, or a data frame of one row: the values the template's placeholders read, one value each.
#'   `NA` and `NULL` show a dash. Dates are written as `YYYY-MM-DD`, times in UTC as `YYYY-MM-DDTHH:MM:SSZ` and
#'   factors as their labels. Once [tip_define()] has seen the template, only the columns it names are kept, so nothing
#'   else in a row is written into the page; for a template not seen here, every column goes, so choose them with
#'   `data[i, c("label", "n")]`.
#'
#' @param style `r lifecycle::badge("experimental")` How this tag's tip looks, or `NULL` (the default) to take the
#'   page's. A preset name from [tip_presets()] (`"none"` clears a preset set above), a [tip_style()], or one part of a
#'   style: a [tip_theme()] or a [bslib::bs_theme()]. The tag's style beats the page's, and a value set nearer wins,
#'   value by value. A list is not taken, and neither is a string that holds `:` or `;` (`style` takes no CSS); see
#'   [tip_style()].
#'
#' @param placement Where the tip opens: `"auto"`, `"top"`, `"bottom"`, `"left"` or `"right"`, or `NULL` (the default)
#'   for the page's choice, which is `"auto"` unless a tagged ancestor sets a side. A chosen side flips to the other
#'   when there is no room, and `"left"` and `"right"` mirror in right-to-left text. On a phone the tip always chooses
#'   its own side.
#'
#' @param marker `r lifecycle::badge("experimental")` A visible cue that the tag holds a tip, off by default: `"none"`,
#'   `"underline"`, `"icon"` or `"corner"`, a [tip_marker()] to choose the line of an underline or give an icon, or a
#'   tag, which is used as the icon (`marker = fontawesome::fa("circle-info")` is `tip_marker("icon", icon = ...)`).
#'   See [tip_marker()] for what each draws. For the cells of a table, use the `marker` argument of [tip_table()].
#'
#' @param id `r lifecycle::badge("experimental")` A Shiny id for this tip, or `NULL` (the default) for none: one
#'   non-empty string with no whitespace, so `ns("baseline_tip")` works in a module. Give it a different id from every
#'   other tip source on the page. [tip_show()], [tip_hide()] and [tip_update()] find the tip by it, and `events` names
#'   the inputs after it.
#'
#' @param events `r lifecycle::badge("experimental")` Which Shiny inputs this tag's tip sends, or `NULL` (the default)
#'   for none. Only `"hint"` is taken in this version: it sends `input$<id>_hint` each time the tip opens or closes,
#'   a list of `state` (`"shown"` or `"hidden"`), `text` (the tip's plain words, cut at 200 characters), `by`
#'   (`"reader"`, `"host"` for [tip_show()] and [tip_hide()], or `"data"` when the tag left the page), `via`
#'   (`"pointer"`, `"focus"`, `"tap"`, `"key"`, `"command"`, or `NULL`) and `reason` (why it
#'   closed, `NULL` on `"shown"`). It is off unless asked for, because it sends a message to the server every time any
#'   reader hovers the tag. It needs an `id`, or a tag with
#'   an `id` of its own; in a module the input is read as `input$<id>_hint`, without the namespace. `"pin"` and
#'   `"copy"` are named but stop with an error until the pin inputs arrive.
#'
#' @param when_disabled `r lifecycle::badge("experimental")` The reason the control is switched off, as one
#'   string of plain text with a word in it, or `NULL` (the default) for none. While the control is disabled,
#'   its tip says "Not available:" and the reason, with a small lock before it; when the control is enabled
#'   again the reason goes and the tag's own tip (`text` or `template`) is shown as usual. `text` and `template`
#'   may be left out, for a tag that only has a reason; then a `marker` is left off with a warning, since it
#'   would show on an enabled control that has no tip. The tag must hold a control that can be disabled: a
#'   `button`, `input`, `select`, `textarea` or `fieldset`. It works when the page is built with the control
#'   disabled and when it is switched off later, by the `update*(disabled = )` functions of Shiny or by script,
#'   on a Bootstrap 5 page (bslib) and on the Bootstrap 3 page of a plain `fluidPage()`; a control disabled by
#'   `aria-disabled` alone is not covered.
#'
#'   A disabled control takes no keyboard focus and, under Bootstrap 5, no pointer, so `tip_on()` puts it in a small
#'   wrapper that does: a `<span>`, or a `<div>` for a tag that is a block, such as the `div` Shiny puts around an
#'   input. While the control is disabled the wrapper is one tab stop, and the control gets no pointer, so the
#'   reason is the wrapper's. A button's wrapper has the role of a button; any other control is a group named by the
#'   control's label, so a screen reader still says what the field is. The wrapper is not a tab stop when the
#'   control is enabled. A control that has the focus when it is switched off hands it to the wrapper, and takes it
#'   back when it is switched on. The tip's `id` and `events` stay on the control, and the wrapper answers to them
#'   while the control is disabled, so [tip_show()] and the hint input name the control either way. The value
#'   returned is the wrapper; calling `tip_on()` on it again changes the control's tip and keeps one wrapper, with
#'   the reason it had unless a new one is given.
#'
#'   The wrapper takes the control's place in a layout: a width, a share of a flex row and a side written in its
#'   `style`, or as the classes `w-25`, `w-50`, `w-75`, `w-100`, `flex-fill`, `flex-grow-*`, `col-*`, `order-*`,
#'   `align-self-*` and `ms-auto`, move to the wrapper, and the control fills the wrapper. A `d-block` control and
#'   the cells of a `d-grid` keep their width. Not covered: a control that is a direct child of a `btn-group`,
#'   `btn-group-vertical` or `input-group`, since the wrapper then comes between the control and the joined borders;
#'   put the tip on the group instead, and do not give a control in a group a reason. A proposed fix is a reason
#'   that needs no wrapper, which asks the tip engine for a keyboard stop of its own.
#'
#' @param more `r lifecycle::badge("experimental")` Extra content for the long form of the tip, or `NULL` (the default)
#'   for none: words (markdown, optionally with `{{ field }}` placeholders filled from `record`) or a [tip_template()].
#'   `NA` and `""` count as none. When `more_on` is `"pin"` (the default), the tag must be able to pin and have an `id`
#'   (either `id =` or the tag's own HTML `id`); otherwise `tip_on()` stops with an error.
#'
#' @param more_on `r lifecycle::badge("experimental")` When the long form (`more`) opens: `"pin"` (the default, shows a
#'   short hint on hover and expands to the long form when pinned) or `"hover"` (shows the long form on hover too), or
#'   `NULL` to use the engine's default (`"pin"`).
#'
#' @returns The tag, with the tip's attributes and the engine's dependency added; with a marker, its attributes,
#'   and the marker's own dependency; with `when_disabled`, inside a wrapper that holds the reason.
#' @family tooltips
#' @examples
#' tip_on(htmltools::tags$button("Run"), "Runs the **check**")
#'
#' # The reason a control is switched off
#' tip_on(
#'   htmltools::tags$button("Run", disabled = NA),
#'   "Runs the **check**",
#'   when_disabled = "Pick a dataset first"
#' )
#'
#' # Short tip on hover with extra detail
#' tip_on(
#'   htmltools::tags$button("Run"),
#'   "Runs the **check**",
#'   more = "Checks every column and prints a summary.",
#'   more_on = "hover"
#' )
#'
#' # A look for this tag only
#' tip_on(htmltools::tags$button("Run"), "Runs the **check**", style = "sober")
#' tip_on(
#'   htmltools::tags$button("Run"),
#'   "Runs the **check**",
#'   style = tip_theme(light = list(bg = "#1b3a6b", ink = "#ffffff"))
#' )
#'
#' # A visible cue that a word holds a tip
#' tip_on(
#'   htmltools::tags$span("Baseline", tabindex = "0"),
#'   "The first **screening** value",
#'   marker = "underline"
#' )
#'
#' tip_define(diff = tip_template(
#'   tip_title("{{ label }}"),
#'   tip_text("{{ differences | number:0 }} differences")
#' ))
#' rows <- data.frame(label = c("ADAE", "ADSL"), differences = c(3, 0))
#' lapply(seq_len(nrow(rows)), function(i) {
#'   tip_on(
#'     htmltools::tags$tr(htmltools::tags$td(rows$label[[i]]), tabindex = "0"),
#'     template = "diff",
#'     record = rows[i, ]
#'   )
#' })
#'
#' # An icon-only button is named from its tip
#' tip_on(htmltools::tags$button(htmltools::tags$i(class = "icon-gear")), "Open the **settings**")
#' @export
tip_on <- function(
  tag,
  text = NULL,
  ...,
  template = NULL,
  record = NULL,
  style = NULL,
  placement = NULL,
  marker = "none",
  id = NULL,
  events = NULL,
  when_disabled = NULL,
  more = NULL,
  more_on = NULL
) {
  if (inherits(tag, "shiny.tag.list")) {
    tip_abort(
      c("{.arg tag} is a list of tags; give one tag.", i = "Call {.fn tip_on} on each tag."),
      class = "hovercraft_error_tag"
    )
  }
  if (!inherits(tag, "shiny.tag")) {
    tip_abort("{.arg tag} must be an HTML tag, not {.obj_type_friendly {tag}}.", class = "hovercraft_error_tag")
  }
  tip_check_dots(
    ...,
    args = c(
      "tag",
      "text",
      "template",
      "record",
      "style",
      "placement",
      "marker",
      "id",
      "events",
      "when_disabled",
      "more",
      "more_on"
    )
  )
  # A tag that an earlier call put in a wrapper: the control inside is tipped again, and the reason is kept or changed.
  wrapper <- NULL
  if (tip_is_disabled_wrapper(tag)) {
    wrapper <- tag
    tag <- wrapper$children[[1]]
    when_disabled <- when_disabled %||% htmltools::tagGetAttribute(wrapper, tip_disabled_attribute())
  }
  tip_arg_string(text, "text", min_chars = 0L, null_ok = TRUE, na_ok = TRUE)
  tip_arg_string(template, "template", null_ok = TRUE)
  style <- tip_as_style(style, "style")
  tip_check_placement(placement)
  tip_check_id(id)
  events <- tip_check_events(events)
  tip_check_when_disabled(when_disabled, tag)
  tip_check_more_on(more_on)
  more_info <- tip_parse_more(more, rlang::current_env())
  tag_id <- htmltools::tagGetAttribute(tag, "id")
  effective_id <- id %||% tag_id
  if (!is.null(events) && is.null(effective_id)) {
    tip_abort(
      c(
        "{.arg events} names its inputs after an id, and the tag has none.",
        i = "Give {.fn tip_on} an {.arg id}, or the tag an {.code id} of its own."
      ),
      class = "hovercraft_error_events"
    )
  }
  effective_more_on <- more_on %||% tip_contract()$more$on$default
  if (!is.null(more_info) && identical(effective_more_on, "pin")) {
    has_pin <- !is.null(htmltools::tagGetAttribute(tag, tip_attribute("pin")))
    if (!has_pin || is.null(effective_id)) {
      why <- if (!has_pin && is.null(effective_id)) {
        "the tag cannot pin and has no {.arg id}."
      } else if (!has_pin) {
        "the tag cannot pin."
      } else {
        "the tag has no {.arg id}."
      }
      tip_abort(
        c(
          paste0("{.arg more} with {.code more_on = \"pin\"} opens on a pin, but ", why),
          i = "Allow the tip to pin and give it an {.arg id}, or set {.code more_on = \"hover\"} to show it on hover."
        ),
        class = "hovercraft_error_pin"
      )
    }
  }
  marker <- tip_as_marker(marker, "marker")
  has_text <- !is.null(text) && !is.na(text) && nzchar(text)
  if (!has_text && is.null(template) && is.null(when_disabled)) {
    if (is.null(text)) {
      tip_abort("Give {.arg text} or {.arg template}; there is nothing to show.", class = "hovercraft_error_tip")
    }
    return(tag) # a missing or empty text is no tip, so a table row with no note stays as it was
  }
  if (!has_text && is.null(template) && !identical(marker$type, "none")) {
    tip_warn(
      c(
        "{.arg marker} is left off: the tag has no tip of its own, only a reason for when it is disabled.",
        i = "A marker says the tag has a tip, and it would show on an enabled control that shows none.",
        i = "Give {.arg text} or {.arg template} to have a marker."
      ),
      class = "hovercraft_warning_marker"
    )
    marker <- tip_as_marker("none", "marker")
  }
  tip_check_focus(tag, rlang::current_env())
  held <- names(record)
  more_fields <- more_info$fields %||% character()
  short_known <- !is.null(template) && template %in% names(tip_known())
  keep <- if (!is.null(template)) {
    if (short_known) unique(c(tip_known()[[template]]$fields, more_fields)) else NULL
  } else if (length(more_fields)) {
    more_fields
  } else {
    NULL
  }
  if (!is.null(record)) {
    record <- tip_record(record, keep)
  }
  if (!is.null(template)) {
    tip_check_use(template, held, effective_id, rlang::current_env())
  } else if (!is.null(record) && !length(more_fields)) {
    tip_warn(
      c("{.arg record} is ignored without a {.arg template}.", i = "A plain text tip has no placeholders to fill in."),
      class = "hovercraft_warning_record"
    )
    record <- NULL
  }
  if (length(more_fields)) {
    tip_check_more_fields(more_fields, held, effective_id, rlang::current_env())
  }
  attrs <- tip_attributes()
  values <- list()
  if (has_text) {
    values[[attrs[["text"]]]] <- text
  }
  if (!is.null(template)) {
    values[[attrs[["template"]]]] <- template
  }
  if (!is.null(record)) {
    values[[attrs[["record"]]]] <- tip_json(record)
  }
  if (!is.null(placement)) {
    values[[tip_attribute("placement")]] <- placement
  }
  if (!is.null(id)) {
    values[[tip_attribute("id")]] <- id
  }
  if (!is.null(events)) {
    values[[tip_shiny_attribute()]] <- events
  }
  more_deps <- list()
  if (!is.null(more_info)) {
    if (!is.null(more_info$template)) {
      built_more <- tip_more_template_dep(more_info$template)
      values[[tip_attribute("more_template")]] <- built_more$name
      more_deps <- built_more$deps
    } else {
      values[[tip_attribute("more")]] <- more_info$text
    }
  }
  if (!is.null(more_on)) {
    values[[tip_attribute("more_on")]] <- more_on
  }
  # A label made from an earlier tip on this tag goes with that tip; a label the page wrote itself stays.
  earlier <- htmltools::tagGetAttribute(tag, attrs[["text"]])
  if (!is.null(earlier) && identical(htmltools::tagGetAttribute(tag, "aria-label"), tip_plain_text(earlier))) {
    tag$attribs[names(tag$attribs) == "aria-label"] <- NULL
  }
  look <- tip_style_tag(style)
  # A tip made before is replaced, its look too.
  own <- c(
    attrs,
    vapply(c("preset", "class", "placement", "id", "more", "more_template", "more_on"), tip_attribute, ""),
    tip_theme_attribute(),
    tip_shiny_attribute()
  )
  tag$attribs[names(tag$attribs) %in% own] <- NULL
  if (has_text && tip_needs_label(tag)) {
    label <- tip_plain_text(text)
    if (nzchar(label)) {
      values[["aria-label"]] <- label
    }
  }
  tag <- do.call(htmltools::tagAppendAttributes, c(list(tag), values, look$attribs))
  tag <- tip_marker_apply(tag, marker, rlang::current_env())
  deps <- c(list(tip_dependency()), look$deps, more_deps)
  if (!is.null(when_disabled)) {
    place <- if (!is.null(placement)) stats::setNames(list(placement), tip_attribute("placement"))
    tag <- tip_disabled_wrap(tag, when_disabled, c(look$attribs, place), wrapper)
    deps <- c(deps, list(tip_disabled_dependency()))
  }
  htmltools::attachDependencies(tag, deps, append = TRUE)
}

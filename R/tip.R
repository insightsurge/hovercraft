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

# `tag` without hovercraft's own dependencies; the page's others stay. A tip that is replaced takes its own with it.
tip_drop_deps <- function(tag) {
  deps <- htmltools::htmlDependencies(tag)
  if (!length(deps)) {
    return(tag)
  }
  own <- vapply(deps, function(dep) inherits(dep, "html_dependency") && startsWith(dep$name, "hovercraft"), NA)
  htmltools::attachDependencies(tag, deps[!own], append = FALSE)
}

# Takes an earlier tip off a tag, whole: its attributes, the label and the key shortcut that were made for it, its
# marker and its dependencies. What the page wrote itself (an `aria-label` or `aria-keyshortcuts` of its own) stays.
tip_strip_tip <- function(tag) {
  attrs <- tip_attributes()
  earlier <- htmltools::tagGetAttribute(tag, attrs[["text"]])
  if (!is.null(earlier) && identical(htmltools::tagGetAttribute(tag, "aria-label"), tip_plain_text(earlier))) {
    tag$attribs[names(tag$attribs) == "aria-label"] <- NULL
  }
  had_pin <- !is.null(htmltools::tagGetAttribute(tag, tip_attribute("pin")))
  more_attrs <- vapply(c("held", "held-template", "label", "state", "room"), tip_more_attribute, "")
  had_more <- any(names(tag$attribs) %in% more_attrs[c("held", "held-template")])
  replaced <- c("preset", "class", "placement", "more", "more_template", "more_on", "id", "pin")
  own <- c(attrs, vapply(replaced, tip_attribute, ""), tip_theme_attribute(), tip_shiny_attribute(), more_attrs)
  keys <- htmltools::tagGetAttribute(tag, "aria-keyshortcuts")
  tag$attribs[names(tag$attribs) %in% own] <- NULL
  if ((had_pin || had_more) && identical(keys, tip_keyshortcuts(had_pin, had_more))) {
    tag$attribs[names(tag$attribs) == "aria-keyshortcuts"] <- NULL
  }
  tip_drop_deps(tip_marker_clear(tag))
}

# `tip_on()` takes one tag. Anything else stops with the way out: a tag list is tipped tag by tag, and a tab's title is
# a tag inside `nav_panel(title = )`.
tip_check_one_tag <- function(tag, call = rlang::caller_env()) {
  if (inherits(tag, "shiny.tag")) {
    return(invisible())
  }
  what <- if (inherits(tag, "shiny.tag.list")) "a list of tags" else "{.obj_type_friendly {tag}}"
  tip_abort(
    c(
      paste0("{.arg tag} must be one HTML tag, not ", what, "."),
      i = if (is.list(tag)) "Call {.fn tip_on} on each tag, or wrap the content in one tag such as {.code span()}.",
      i = paste0(
        "For a tab, tip the tag that is its title: ",
        "{.code nav_panel(title = tip_on(span(\"Data\"), \"...\"), value = \"data\")}. ",
        "The {.arg value} is needed: without it bslib writes the tag's HTML into the tab's value."
      )
    ),
    class = "hovercraft_error_tag",
    call = call
  )
}

# A tag that has a `title` attribute shows the browser's own tooltip beside the tip. The tip keeps the title (on an icon
# it may be the only name), so this says to remove it.
tip_warn_title <- function(tag, call) {
  if (is.null(htmltools::tagGetAttribute(tag, "title"))) {
    return(invisible())
  }
  tip_warn(
    c(
      "The tag has a {.code title} attribute, so the browser shows its own tooltip beside the tip.",
      i = "Remove the {.code title}, or move its words into {.arg text}. {.fn tip_on} keeps it."
    ),
    class = "hovercraft_warning_title",
    call = call,
    .frequency = "once",
    .frequency_id = "hovercraft-title"
  )
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
tip_template_seen <- function(template, call, session = tip_session()) {
  known <- tip_known(session)
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
tip_check_use <- function(template, held, id, call, session = tip_session()) {
  if (!tip_template_seen(template, call, session)) {
    return(invisible())
  }
  known <- tip_known(session)
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
tip_more_template_dep <- function(template, prefix = "more") {
  body <- tip_blocks_plain(template)
  hash <- substr(rlang::hash(tip_json(body)), 1L, 12L)
  name <- paste0(prefix, "_", hash)
  ops <- list(list(op = "define", name = name, blocks = body))
  for (out_id in tip_output_ids(list(template))) {
    ops <- c(ops, list(list(op = "park", id = out_id)))
  }
  park_tags <- tip_output_tags(list(template))
  # The output tags are handed to the runtime script as one string, which puts them in the page's park holder once there
  # is a body: a `<div>` in a dependency's `<meta>` is data, and cannot end the head.
  if (length(park_tags)) {
    ops <- c(ops, list(list(op = "parkHtml", html = as.character(htmltools::tagList(park_tags)))))
  }
  dep <- tip_gen_dependency(paste0("hovercraft-", prefix, "-", hash), ops = ops)
  extra_deps <- if (length(park_tags)) htmltools::findDependencies(park_tags) else list()
  list(name = name, deps = c(list(dep), extra_deps))
}

# Checks fields named by an inline template (`more`, a `tip_template()` given as `template`, or `text` with a record)
# against `record`, matching tip_check_use(). `what` names the argument in the messages.
tip_check_more_fields <- function(fields, held, id, call, what = "{.arg more}") {
  engine <- unlist(tip_rules()$engine_fields)
  if ("tip_index" %in% fields && !"tip_index" %in% held) {
    tip_abort(
      c(
        paste0(what, " names {.code tip_index}, which has nothing to count here."),
        i = "Give the record a value of that name for now."
      ),
      class = "hovercraft_error_field",
      call = call
    )
  }
  if ("tip_id" %in% fields && !"tip_id" %in% held && is.null(id)) {
    tip_abort(
      c(
        paste0(what, " names {.code tip_id}, which is the tag's own id, and the tag has none."),
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
        paste0(what, " names fields the record does not hold."),
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
#' `r lifecycle::badge("stable")`
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
#' A second `tip_on()` on a tag that already has a tip replaces the old tip whole: its words, look, `id`, `events`,
#' `pin` and `when_disabled` all go, and the new call's arguments are the tip. When the old tip had an `id`, `events`
#' or a pin and the new call gives none of them, it warns once, with the class `hovercraft_warning_retip`, so a wrapper
#' that tips a tag again on purpose can silence it with `suppressWarnings(classes = )`.
#'
#' A tag that already has a `title` attribute keeps it, and the browser then shows its own tooltip beside the tip.
#' `tip_on()` warns once, with the class `hovercraft_warning_title`, and says to remove the `title`; it does not
#' remove it itself, because on an icon it may be the only name.
#'
#' @section Stable and experimental:
#' `tag`, `text`, `template`, `record`, `placement`, `id`, `pin` (one pinned tip at a time), `more` and a `style` that
#' is a preset name are `r lifecycle::badge("stable")`: their names, values and defaults do not change in 0.1.x.
#' `events` and the fields of the inputs it turns on, `when_disabled`, `marker`, a `style` that is a [tip_style()], a
#' [tip_theme()] or a bslib theme, and keeping more than one pin (`max_pins` of [use_hovercraft()]) are
#' `r lifecycle::badge("experimental")` and may change.
#'
#' @section Known limits:
#' * A tag inside [bslib::tooltip()] or [bslib::popover()], or with `data-bs-toggle`, keeps the tip that bslib draws.
#' * A pin closes (reason `"gone"`) when its item is removed, or hidden by a tab, a conditional panel or a sidebar, and
#'   it does not come back with the item. A pin whose item only scrolls out of view is hidden and returns when the item
#'   does.
#' * `tip_on()` costs about a millisecond of R time for each tag, and the page's script some time on every change of
#'   the page: for a long table, page it.
#' * Reconnecting an app (`allowReconnect`) is untested.
#'
#' @param tag `r lifecycle::badge("stable")` One HTML tag, such as `htmltools::tags$tr()` or `shiny::actionButton()`. A
#'   list of tags, a tag list or text stops with an error. To tip a tab, put the tip on the tag that is its title, and
#'   give the tab a `value`: `nav_panel(title = tip_on(span("Data"), "..."), value = "data")`. Without `value`, bslib
#'   writes the tag's HTML into the tab's value.
#' @param text `r lifecycle::badge("stable")` One string of markdown (strong, emphasis, code, links, short
#'   lists), or `NULL`. It is shown as written: build it with [paste0()] or [sprintf()]. `NA` and `""` count as
#'   none: the tag comes back without a tip. When `record` is given, the `{{ field }}` placeholders in the text are
#'   filled from it, also when a `template` is given (as in a template, with one filter at most, `number:n` or
#'   `percent:n`); without a record, braces stay as written. Words with placeholders give the tag no `aria-label`,
#'   because they depend on the record.
#' @param ... Must be empty. It makes a misspelt argument an error.
#' @param template `r lifecycle::badge("stable")` The name of a template defined with [tip_define()], a
#'   [tip_template()] made here, or `NULL`. A `tip_template()` travels with the tag, so it needs no `tip_define()`; its
#'   fields are checked against `record`. If `text` is given too, the text is the tip (see `text`).
#' @param record `r lifecycle::badge("stable")` A named list, or a data frame of one row: the values the template's
#'   placeholders read, one value each. `NA` and `NULL` show a dash. Dates are written as `YYYY-MM-DD`, times in UTC as
#'   `YYYY-MM-DDTHH:MM:SSZ` and factors as their labels. Once [tip_define()] has seen the template, only the columns it
#'   names are kept, so nothing else in a row is written into the page; for a template not seen here, every column goes,
#'   so choose them with `data[i, c("label", "n")]`.
#'
#' @param style `r lifecycle::badge("stable")` as a preset name, `r lifecycle::badge("experimental")` as a style
#'   object. How this tag's tip looks, or `NULL` (the default) to take the page's. A preset name from [tip_presets()]
#'   (`"none"` clears a preset set above), a [tip_style()], or one part of a style: a [tip_theme()] or a
#'   [bslib::bs_theme()]. The tag's style beats the page's, and a value set nearer wins, value by value. A list is not
#'   taken, and neither is a string that holds `:` or `;` (`style` takes no CSS); see [tip_style()]. A list of CSS
#'   variables (`tip_style(theme = list("--x" = ...))`) is for the whole page, so on one tag it stops with an error:
#'   give it to [use_hovercraft()], or use the roles of [tip_theme()].
#'
#' @param placement `r lifecycle::badge("stable")` Where the tip opens: `"auto"`, `"top"`, `"bottom"`, `"left"` or
#'   `"right"`, or `NULL` (the default) for the page's choice, which is `"auto"` unless a tagged ancestor sets a side. A
#'   chosen side flips to the other when there is no room, and `"left"` and `"right"` mirror in right-to-left text. On a
#'   phone the tip always chooses its own side.
#'
#' @param marker `r lifecycle::badge("experimental")` A visible cue that the tag holds a tip. `NULL` (the default) and
#'   `"none"` draw none; the others are `"underline"`, `"icon"` or `"corner"`, a [tip_marker()] to choose the line of
#'   an underline or give an icon, or a tag, which is used as the icon (`marker = fontawesome::fa("circle-info")` is
#'   `tip_marker("icon", icon = ...)`). See [tip_marker()] for what each draws. For the cells of a table, use the
#'   `marker` argument of [tip_table()].
#'
#' @param id `r lifecycle::badge("stable")` A Shiny id for this tip, or `NULL` (the default) for none: one non-empty
#'   string with no whitespace and no `:`, so `ns("baseline_tip")` works in a module. Give it a different id from every
#'   other tip source on the page. [tip_show()], [tip_hide()] and [tip_update()] find the tip by it, and `events` names
#'   the inputs after it.
#'
#' @param events `r lifecycle::badge("experimental")` Which Shiny inputs this tag's tip sends, or `NULL` (the
#'   default) for none; one or more of `"hint"`, `"pin"` and `"copy"`. All are off unless asked for, because
#'   they send a message to the server every time a reader meets the tag. They need an `id`, or a tag with an
#'   `id` of its own; in a module the input is read without the namespace. `"pin"` and `"copy"` belong to a pinned
#'   tip, so asking for either without a `pin` stops with an error. Each is a list that holds the fields below.
#'
#'   `"hint"` sends `input$<id>_hint` each time the tip opens or closes: `state` (`"shown"` or `"hidden"`), `text`
#'   (the tip's plain words, cut at 200 characters), `by` (`"reader"`, `"host"` for [tip_show()] and [tip_hide()],
#'   or `"data"` when the tag left the page), `via` (`"pointer"`, `"focus"`, `"tap"`, `"key"`, `"command"`, or
#'   `NULL`) and `reason` (why it closed, `NULL` on `"shown"`: `"leave"`, `"blur"`, `"escape"`, `"outside"`,
#'   `"pinned"` when the hint became a pin, `"replaced"`, `"gone"` or `"host"`). `"pin"` sends `input$<id>_pin` each
#'   time the tip is pinned, closed or drawn again while pinned: `state` (`"pinned"`, `"closed"` or `"updated"`),
#'   `text`, `by` (`"reader"`, `"host"` or `"data"`), `via` (`"click"`, `"key"`, `"command"`, or `NULL`) and `reason`
#'   (why it closed: `"select"`, `"escape"`, `"button"`, `"outside"`, `"replaced"`, `"gone"`, `"below"` when a layer
#'   around it closed, `"host"` or `"redraw"` when it was drawn again with other host slots (`by` is `"data"`); `NULL`
#'   on `"pinned"`). A `"state"` of `"updated"` is not a pin: an app that tests only for `"closed"` should check it.
#'   `"pin"` also keeps `input$<id>_pinned`, `TRUE` while the tip is pinned and `FALSE` after, sent in the same batch
#'   as `_pin` and as a plain value, so an equal value does not fire an observer again; read it with
#'   `isTRUE(input$run_pinned)`. The values of `state` may grow in a later version, so an app should not assume it
#'   knows them all. Every one of these inputs is `NULL` until its first event; in `shiny::testServer()` set them with
#'   `session$setInputs()`, and in `shinytest2` with `set_inputs(..., allow_no_input_binding_ = TRUE)`. On a page
#'   without Shiny (R Markdown, Quarto, a static page) `events` does nothing.
#'   `"copy"` sends `input$<id>_copy` each time the Copy button of a code block in the pinned tip is
#'   pressed: `field` (the record field the code came from), `block` (the code block's place among the tip's code
#'   blocks, from 0), `ok` (`TRUE` when the browser let the copy happen), `by` (`"reader"`) and `via` (`"click"` or
#'   `"key"`).
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
#'   `aria-disabled` alone is not covered. [tip_update()] changes the control's own tip, also while it is disabled:
#'   the new words show once the control is enabled.
#'
#'   A disabled control takes no keyboard focus and, under Bootstrap 5, no pointer, so `tip_on()` puts it in a small
#'   wrapper that does: a `<span>`, or a `<div>` for a tag that is a block, such as the `div` Shiny puts around an
#'   input. While the control is disabled the wrapper is one tab stop, and the control gets no pointer, so the
#'   reason is the wrapper's. A button's wrapper has the role of a button; any other control is a group named by the
#'   control's label, so a screen reader still says what the field is. The wrapper is not a tab stop when the
#'   control is enabled. A control that has the focus when it is switched off hands it to the wrapper, and takes it
#'   back when it is switched on. The tip's `id` and `events` stay on the control, and the wrapper answers to them
#'   while the control is disabled, so [tip_show()] and the hint input name the control either way. The value
#'   returned is the wrapper; calling `tip_on()` on it again replaces the tip whole, the reason
#'   included, and keeps one wrapper when a new `when_disabled` is given; without one you get the bare control back,
#'   with the layout it had before it was wrapped.
#'
#'   The wrapper takes the control's place in a layout: a width, a share of a flex row and a side written in its
#'   `style`, or as the classes `w-25`, `w-50`, `w-75`, `w-100`, `flex-fill`, `flex-grow-*`, `col-*`, `order-*`,
#'   `align-self-*` and `ms-auto`, move to the wrapper, and the control fills the wrapper. A `d-block` control and
#'   the cells of a `d-grid` keep their width. Not covered: a control that is a direct child of a `btn-group`,
#'   `btn-group-vertical` or `input-group`, since the wrapper then comes between the control and the joined borders;
#'   put the tip on the group instead, and do not give a control in a group a reason. A proposed fix is a reason
#'   that needs no wrapper, which asks the tip engine for a keyboard stop of its own.
#'
#' @param more `r lifecycle::badge("stable")` as words or a template, `r lifecycle::badge("experimental")` as a
#'   [tip_more()]. Extra content for the long form of the tip, or `NULL` (the default)
#'   for none: words (markdown, optionally with `{{ field }}` placeholders filled from `record`), a [tip_template()], or
#'   a [tip_more()] that also says when the long form shows. `NA` and `""` count as none. Words and a template are the
#'   same as `tip_more()` with its default: the tip is short, and a "More" button in it (and the M key) opens the long
#'   form in the same tip, with no pin and no `id`. See [tip_more()] for `"hover"`, `"pin"` and the buttons' words; with
#'   `"pin"` the tag must be able to pin and have an `id` (either `id =` or the tag's own HTML `id`), otherwise
#'   `tip_on()` stops with an error.
#'
#' @param pin `r lifecycle::badge("stable")` Whether a reader can keep this tip open, one at a time: `NULL` (the
#'   default) or `FALSE` for a tip that only shows on hover or focus (`FALSE` is "never", whatever a level above says),
#'   `TRUE` for a tip that is pinned by the small
#'   pin button in the corner of the tip, or by the pin key, and `"click"` for a tip that a click on the tagged item
#'   also pins. `"click"` is for a tag that has nothing to click on its own: on a control, or on a tag that holds
#'   one, it stops with an error, because the click would pin the tip instead of using the control. Use `TRUE`
#'   there: a plain button and a `div` around an input take it. With `TRUE`, a click, a key or a tap on the item never
#'   pins, so an item that does something when
#'   it is clicked, such as a row that selects itself, keeps doing it; a tap shows the tip. A tip that is pinned
#'   stays open, with the engine's own close button, until it is closed (the button, Escape, or a click
#'   elsewhere), and a code block with `tip_code(copy = TRUE)` shows its Copy button there. A pin needs an `id`
#'   (the `id` argument or the tag's own), which names the tip in `events`, and a tag that the engine can pin:
#'   not a link, a submit or reset button, an `input`, `select`, `textarea`, `summary`, `label` or editable tag;
#'   put the tip on a `<div>` or `<span>` around it.
#'
#'   The pin button is drawn over the corner of the tip as a separate element, so a screen reader still gets a tip
#'   that it only reads; the first block of the tip keeps room for it, so a one-sentence tip has the button at the
#'   end of its line. The pin key is `P`, which the tagged item announces as `aria-keyshortcuts`: it pins the tagged
#'   item while the focus is on the item or inside it, or while the pointer rests on it, and does nothing while the
#'   focus is in a field or while neither holds. When the focus is on one item and the pointer rests on another, the
#'   key pins the item under the pointer, the tip the reader sees, not an item around it; an item that is pinned and
#'   holds the focus keeps the key. A tip that a tap or `tip_show()` opened does not take the key from the pointer.
#'   The key works from a hover, so a page switches it off for every reader with `use_hovercraft(keys = FALSE)`;
#'   there is no switch for one reader. A screen reader that is in browse mode may take the letter for its own
#'   navigation, so check the key with the screen reader you support. `Escape` closes a pinned tip. Pointer, keyboard
#'   and tap are tested in Chrome only.
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
#' # A tip that a reader can pin, by the button in its corner or by the P key
#' tip_on(
#'   htmltools::tags$button("Run", id = "run"),
#'   "Runs the **check**",
#'   id = "run_tip",
#'   pin = TRUE,
#'   events = c("hint", "pin")
#' )
#'
#' # A short tip with a "More" button that opens the long form in the same tip
#' tip_on(
#'   htmltools::tags$button("Run"),
#'   "Runs the **check**",
#'   more = "Checks every column and prints a summary."
#' )
#'
#' # The long form shows with the short one
#' tip_on(
#'   htmltools::tags$button("Run"),
#'   "Runs the **check**",
#'   more = tip_more("Checks every column and prints a summary.", on = "hover")
#' )
#'
#' # A look for this tag only
#' tip_on(htmltools::tags$button("Run"), "Runs the **check**", style = "sober")
#' tip_on(
#'   htmltools::tags$button("Run"),
#'   "Runs the **check**",
#'   style = tip_theme(light = list(bg = "#1b3a6b", text = "#ffffff"))
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
  marker = NULL,
  id = NULL,
  events = NULL,
  when_disabled = NULL,
  more = NULL,
  pin = NULL
) {
  tip_check_one_tag(tag)
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
      "pin"
    )
  )
  # A tag that an earlier call put in a wrapper: the control inside is tipped again, and the earlier tip goes with the
  # wrapper, the reason included (a second call replaces the tip whole).
  wrapper <- NULL
  if (tip_is_disabled_wrapper(tag)) {
    wrapper <- tag
    tag <- wrapper$children[[1]]
  }
  # The old tip's wiring, read before anything is taken off: replacing it whole loses an id, events and a pin.
  wired <- any(names(tag$attribs) %in% c(tip_attribute("id"), tip_attribute("pin"), tip_shiny_attribute()))
  tip_arg_string(text, "text", min_chars = 0L, null_ok = TRUE, na_ok = TRUE)
  # A template is a name from tip_define(), or a tip_template() made here, which travels with the tag.
  inline <- NULL
  if (inherits(template, "hovercraft_template")) {
    inline <- list(template = template, fields = unique(tip_template_fields(template)), what = "The {.arg template}")
    template <- NULL
  } else {
    tip_arg_string(template, "template", null_ok = TRUE)
  }
  style <- tip_as_style(style, "style")
  tip_check_placement(placement)
  tip_check_id(id)
  events <- tip_check_events(events)
  tip_check_when_disabled(when_disabled, tag)
  # A call without a reason gives the control back without its wrapper: the earlier reason goes with the earlier tip.
  if (!is.null(wrapper) && is.null(when_disabled)) {
    tag <- tip_disabled_unwrap(wrapper, tag)
    wrapper <- NULL
  } else if (!is.null(wrapper)) {
    wrapper <- tip_drop_deps(wrapper)
  }
  pin_value <- tip_check_pin(pin)
  if (identical(pin_value, "true")) {
    tip_check_pin_click(tag)
  }
  if (!is.null(pin_value)) {
    tip_check_pin_tag(tag)
  }
  tip_check_events_pin(events, pin_value)
  more_given <- tip_more_unpack(more)
  more_info <- tip_parse_more(more_given$content, rlang::current_env())
  more_on <- if (!is.null(more_info)) more_given$on
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
  if (!is.null(pin_value)) {
    tip_check_pin_id(effective_id)
  }
  if (!is.null(more_info) && identical(more_on, "pin")) {
    # A pin already needs an id (checked above), so only a tip with no pin gets here.
    if (is.null(pin_value)) {
      why <- if (is.null(effective_id)) "the tag cannot pin and has no {.arg id}." else "the tag cannot pin."
      tip_abort(
        c(
          paste0("{.arg more} with {.code tip_more(on = \"pin\")} opens on a pin, but ", why),
          i = "Allow the tip to pin and give it an {.arg id}, or use {.code on = \"hover\"} or {.code \"button\"}."
        ),
        class = "hovercraft_error_pin"
      )
    }
  }
  marker <- tip_as_marker(marker, "marker")
  has_text <- !is.null(text) && !is.na(text) && nzchar(text)
  # `text` with a record fills its {{ field }} placeholders: the words are made into a template of their own. Without a
  # record, braces stay as written.
  text_inline <- FALSE
  if (has_text && !is.null(record) && is.null(inline)) {
    fields <- tip_check_text(text, "The words of `text`")
    if (length(fields)) {
      inline <- list(template = tip_template(tip_text(text)), fields = unique(fields), what = "{.arg text}")
      text_inline <- TRUE
      template <- NULL # the text is the tip, as it is when both are given; the named template is not used
    }
  }
  if (!has_text && is.null(template) && is.null(inline) && is.null(when_disabled)) {
    if (is.null(text)) {
      tip_abort("Give {.arg text} or {.arg template}; there is nothing to show.", class = "hovercraft_error_tip")
    }
    return(tip_strip_tip(tag)) # a missing or empty text is no tip: the earlier one, if any, goes
  }
  if (!has_text && is.null(template) && is.null(inline) && !identical(marker$type, "none")) {
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
  tip_warn_title(tag, rlang::current_env())
  held <- names(record)
  more_fields <- more_info$fields %||% character()
  short_known <- !is.null(template) && template %in% names(tip_known())
  keep <- if (!is.null(inline)) {
    unique(c(inline$fields, more_fields))
  } else if (!is.null(template)) {
    if (short_known) unique(c(tip_known()[[template]]$fields, more_fields)) else NULL
  } else if (length(more_fields)) {
    more_fields
  } else {
    NULL
  }
  if (!is.null(record)) {
    record <- tip_record(record, keep)
  }
  if (!is.null(inline)) {
    tip_check_more_fields(inline$fields, held, effective_id, rlang::current_env(), what = inline$what)
  } else if (!is.null(template)) {
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
  inline_deps <- list()
  if (!is.null(inline)) {
    built <- tip_more_template_dep(inline$template, "inline")
    template <- built$name
    inline_deps <- built$deps
  }
  if (has_text && !text_inline) {
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
  if (!is.null(pin_value)) {
    values[[tip_attribute("pin")]] <- pin_value
  }
  more_deps <- list()
  if (!is.null(more_info)) {
    built_more <- if (!is.null(more_info$template)) tip_more_template_dep(more_info$template)
    more_deps <- built_more$deps %||% list()
    values <- c(
      values,
      tip_more_values(more_on, built_more$name, more_info$text, more_given$label)
    )
  }
  keys <- tip_keyshortcuts(!is.null(pin_value), identical(more_on, "button"))
  if (!is.null(keys)) {
    values[["aria-keyshortcuts"]] <- keys
  }
  look <- tip_style_tag(style)
  # A tip made before is replaced whole: its words, id, events, pin and look. A call that names none of the wiring the
  # old tip had (an id, events, a pin) says so once.
  if (wired && is.null(id) && is.null(events) && is.null(pin)) {
    tip_warn(
      c(
        "This tag already has a tip with an id, events or a pin, and {.fn tip_on} replaced it whole.",
        i = "Give {.arg id}, {.arg events} and {.arg pin} again if the new tip still needs them.",
        i = "A wrapper that tips again on purpose can silence the class {.cls hovercraft_warning_retip}."
      ),
      class = "hovercraft_warning_retip",
      .frequency = "once",
      .frequency_id = "hovercraft-retip"
    )
  }
  tag <- tip_strip_tip(tag)
  if (has_text && !text_inline && tip_needs_label(tag)) {
    label <- tip_plain_text(text)
    if (nzchar(label)) {
      values[["aria-label"]] <- label
    }
  }
  tag <- do.call(htmltools::tagAppendAttributes, c(list(tag), values, look$attribs))
  tag <- tip_marker_apply(tag, marker, rlang::current_env())
  deps <- c(list(tip_engine_dependency()), look$deps, inline_deps, more_deps)
  if (!is.null(pin_value)) {
    deps <- c(deps, list(tip_pin_dependency()))
  }
  if (identical(more_on, "button")) {
    deps <- c(deps, list(tip_more_dependency()))
  }
  if (!is.null(when_disabled)) {
    place <- if (!is.null(placement)) stats::setNames(list(placement), tip_attribute("placement"))
    tag <- tip_disabled_wrap(tag, when_disabled, c(look$attribs, place), wrapper)
    deps <- c(deps, list(tip_disabled_dependency()))
  }
  htmltools::attachDependencies(tag, c(deps, list(tip_runtime_dependency())), append = TRUE)
}

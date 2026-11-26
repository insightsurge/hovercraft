# JavaScript-driven UI: the app's own JavaScript (www/tab-js.js) adds, moves, replaces and removes tagged elements and
# sends data, as the "JavaScript-driven UI" practice lists. One row per way; test-bench-js-browser.R tests each.

# One row of the tab: a title, what to do, and the stage the row's elements live in.
js_row <- function(id, title, steps, ...) {
  htmltools::tags$section(
    id = paste0("js-", id),
    class = "bench-row",
    htmltools::tags$h3(class = "h6", title),
    htmltools::tags$p(class = "small text-body-secondary", steps),
    htmltools::div(class = "d-flex flex-wrap gap-2 align-items-start", ...)
  )
}

# A tagged button as HTML text, for the app's JavaScript to put in the page itself.
js_tagged_html <- function(id, label, tip, events = NULL) {
  as.character(hovercraft::tip_on(
    htmltools::tags$button(id = id, type = "button", class = "btn btn-outline-primary btn-sm", label),
    tip,
    id = paste0(gsub("-", "_", id, fixed = TRUE), "_tip"),
    events = events
  ))
}

# A text input inside a tagged container, as HTML text, for JavaScript to insert and Shiny to bind.
js_input_html <- function(id, tip) {
  as.character(hovercraft::tip_on(
    htmltools::div(
      class = "form-group shiny-input-container",
      htmltools::tags$label(`for` = id, "Bound input"),
      htmltools::tags$input(id = id, type = "text", class = "form-control", value = "first")
    ),
    tip,
    id = paste0(id, "_tip")
  ))
}

tab_js_ui <- function() {
  htmltools::tagList(
    shinyjs::useShinyjs(),
    htmltools::tags$script(src = "tab-js.js"),
    # What the page's JavaScript inserts, written by R once: the scripts read these templates.
    htmltools::tags$template(
      id = "js-tpl-html",
      htmltools::HTML(js_tagged_html("js-html-btn", "By innerHTML", "Inserted by innerHTML"))
    ),
    htmltools::tags$template(
      id = "js-tpl-jq",
      htmltools::HTML(js_tagged_html("js-jq-btn", "By jQuery", "Inserted by jQuery"))
    ),
    htmltools::tags$template(id = "js-tpl-input", htmltools::HTML(js_input_html("js_bound", "A bound input's tip"))),
    htmltools::tags$template(
      id = "js-tpl-render",
      htmltools::HTML(js_tagged_html("js-render-btn", "Rendered", "Drawn by renderContent"))
    ),
    htmltools::tags$template(
      id = "js-tpl-replace",
      htmltools::HTML(js_tagged_html("js-rep-btn", "Replaced", "The new node's tip"))
    ),
    htmltools::tags$p(
      "Each row is driven by the page's own JavaScript, not by Shiny's server. ",
      "Hover each tagged button after the step; ",
      "its tip must show, and the log must show no error."
    ),
    js_row(
      "msg",
      "A custom message from the server",
      "Send: the server sends a message with a tagged button's HTML; the page's handler puts it in the slot.",
      shiny::actionButton("js_msg_send", "Send"),
      htmltools::div(id = "js-msg-slot")
    ),
    js_row(
      "set",
      "Shiny.setInputValue()",
      "Press the tagged button: the page sends an input of its own; the server writes it below. The tip still shows.",
      hovercraft::tip_on(
        htmltools::tags$button(
          id = "js-set-btn",
          type = "button",
          class = "btn btn-outline-primary btn-sm",
          "Send a value"
        ),
        "Sends js_set by setInputValue",
        id = "js_set_tip"
      ),
      shiny::textOutput("js_set_seen", inline = TRUE)
    ),
    js_row(
      "html",
      "HTML inserted by innerHTML, by jQuery .html() and .append(), then bound",
      "Insert: innerHTML, then jQuery, then an input bound with Shiny.bindAll(); its value reaches the server.",
      htmltools::tags$button(id = "js-html-add", type = "button", class = "btn btn-secondary btn-sm", "innerHTML"),
      htmltools::tags$button(id = "js-jq-add", type = "button", class = "btn btn-secondary btn-sm", "jQuery"),
      htmltools::tags$button(
        id = "js-input-add",
        type = "button",
        class = "btn btn-secondary btn-sm",
        "Input + bindAll"
      ),
      htmltools::div(id = "js-html-slot"),
      htmltools::div(id = "js-jq-slot"),
      htmltools::div(id = "js-input-slot"),
      shiny::textOutput("js_bound_seen", inline = TRUE)
    ),
    js_row(
      "render",
      "Shiny.renderContent()",
      "Render: the page renders a tagged button with its dependencies through Shiny.renderContent().",
      htmltools::tags$button(id = "js-render-add", type = "button", class = "btn btn-secondary btn-sm", "Render"),
      htmltools::div(id = "js-render-slot")
    ),
    js_row(
      "remove",
      "Shiny.unbindAll(), then removed",
      "Hover the input's tip open, then press Remove: the page unbinds and removes it; the tip closes as gone.",
      htmltools::tags$button(
        id = "js-remove",
        type = "button",
        class = "btn btn-secondary btn-sm",
        "Remove the bound input"
      )
    ),
    js_row(
      "move",
      "Moved and replaced by JavaScript",
      "Move: the tagged button moves to the other box. Replace: it is swapped for a new node with its own tip.",
      htmltools::tags$button(id = "js-move", type = "button", class = "btn btn-secondary btn-sm", "Move"),
      htmltools::tags$button(id = "js-replace", type = "button", class = "btn btn-secondary btn-sm", "Replace"),
      htmltools::div(
        id = "js-box-a",
        class = "border rounded p-2",
        hovercraft::tip_on(
          htmltools::tags$button(
            id = "js-mov-btn",
            type = "button",
            class = "btn btn-outline-primary btn-sm",
            "Moving"
          ),
          "The moving button's tip",
          id = "js_mov_tip"
        )
      ),
      htmltools::div(id = "js-box-b", class = "border rounded p-2 ms-5", "Box B")
    ),
    js_row(
      "attr",
      "Classes and attributes changed by JavaScript",
      "Change: the page toggles a class, sets aria-label and rewrites the tip's own words on the element.",
      htmltools::tags$button(id = "js-attr-change", type = "button", class = "btn btn-secondary btn-sm", "Change"),
      hovercraft::tip_on(
        htmltools::tags$button(
          id = "js-attr-btn",
          type = "button",
          class = "btn btn-outline-primary btn-sm",
          "Changing"
        ),
        "Words before",
        id = "js_attr_tip"
      )
    ),
    js_row(
      "value",
      "shiny:value and shiny:inputchanged",
      "Tick: the server redraws an output; the page tags it on shiny:value, and counts shiny:inputchanged events.",
      shiny::actionButton("js_tick", "Tick"),
      shiny::textOutput("js_value_out", inline = TRUE),
      htmltools::tags$span(id = "js-changed-count", "0")
    ),
    js_row(
      "shinyjs",
      "shinyjs show(), hide() and toggleClass()",
      "Hide, show and toggle from the server with shinyjs; the tip follows the button.",
      shiny::actionButton("js_sjs_hide", "Hide"),
      shiny::actionButton("js_sjs_show", "Show"),
      shiny::actionButton("js_sjs_toggle", "Toggle class"),
      hovercraft::tip_on(
        htmltools::tags$button(id = "js-sjs-btn", type = "button", class = "btn btn-outline-primary btn-sm", "shinyjs"),
        "The shinyjs button's tip",
        id = "js_sjs_tip"
      )
    ),
    js_row(
      "bootstrap",
      "Bootstrap's own JavaScript: collapse, modal, dropdown, its tooltip and popover",
      paste(
        "Open each with Bootstrap's own buttons and hover the tagged item inside;",
        "Bootstrap's tooltip and popover sit beside a tagged button."
      ),
      htmltools::tags$button(
        id = "js-bs-collapse-open",
        type = "button",
        class = "btn btn-secondary btn-sm",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = "#js-bs-collapse",
        "Collapse"
      ),
      htmltools::div(
        id = "js-bs-collapse",
        class = "collapse",
        hovercraft::tip_on(
          htmltools::tags$button(
            id = "js-bs-col-btn",
            type = "button",
            class = "btn btn-outline-primary btn-sm",
            "In a collapse"
          ),
          "Inside Bootstrap's collapse",
          id = "js_bs_col_tip"
        )
      ),
      htmltools::tags$button(
        id = "js-bs-modal-open",
        type = "button",
        class = "btn btn-secondary btn-sm",
        `data-bs-toggle` = "modal",
        `data-bs-target` = "#js-bs-modal",
        "Modal"
      ),
      htmltools::div(
        class = "dropdown",
        htmltools::tags$button(
          id = "js-bs-drop-open",
          type = "button",
          class = "btn btn-secondary btn-sm dropdown-toggle",
          `data-bs-toggle` = "dropdown",
          `aria-expanded` = "false",
          "Dropdown"
        ),
        htmltools::tags$ul(
          class = "dropdown-menu",
          htmltools::tags$li(hovercraft::tip_on(
            htmltools::tags$a(id = "js-bs-drop-item", class = "dropdown-item", href = "#", "Tagged item"),
            "Inside Bootstrap's dropdown",
            id = "js_bs_drop_tip"
          ))
        )
      ),
      htmltools::tags$button(
        id = "js-bs-tooltip",
        type = "button",
        class = "btn btn-outline-secondary btn-sm",
        `data-bs-toggle` = "tooltip",
        `data-bs-title` = "Bootstrap's own tooltip",
        "BS tooltip"
      ),
      htmltools::tags$button(
        id = "js-bs-popover",
        type = "button",
        class = "btn btn-outline-secondary btn-sm",
        `data-bs-toggle` = "popover",
        `data-bs-content` = "Bootstrap's own popover",
        "BS popover"
      ),
      hovercraft::tip_on(
        htmltools::tags$button(
          id = "js-bs-near",
          type = "button",
          class = "btn btn-outline-primary btn-sm",
          "Beside them"
        ),
        "A tip beside Bootstrap's",
        id = "js_bs_near_tip"
      ),
      htmltools::div(
        id = "js-bs-modal",
        class = "modal fade",
        tabindex = "-1",
        htmltools::div(
          class = "modal-dialog",
          htmltools::div(
            class = "modal-content",
            htmltools::div(
              class = "modal-body",
              hovercraft::tip_on(
                htmltools::tags$button(
                  id = "js-bs-modal-btn",
                  type = "button",
                  class = "btn btn-outline-primary btn-sm",
                  "In a modal"
                ),
                "Inside Bootstrap's modal",
                id = "js_bs_modal_tip"
              ),
              htmltools::tags$button(
                type = "button",
                class = "btn btn-secondary btn-sm",
                `data-bs-dismiss` = "modal",
                "Close"
              )
            )
          )
        )
      )
    ),
    js_row(
      "widget",
      "A widget's render callback",
      "The table's own callback (DT's callback, run when the widget renders) tags the first cell; hover it.",
      DT::DTOutput("js_dt")
    ),
    js_row(
      "names",
      "The app's own message and event names",
      paste(
        "The page registers handlers named show, hide and tip-show and sends app:hint events;",
        "Show runs both the app's own show and tip_show()."
      ),
      shiny::actionButton("js_names_show", "Show"),
      hovercraft::tip_on(
        htmltools::tags$button(id = "js-names-btn", type = "button", class = "btn btn-outline-primary btn-sm", "Named"),
        "Shown by tip_show()",
        id = "js_names_tip",
        events = "hint"
      ),
      htmltools::tags$span(id = "js-names-seen", "")
    )
  )
}

tab_js_server <- function(input, output, session) {
  shiny::observeEvent(input$js_msg_send, {
    session$sendCustomMessage(
      "bench-js-insert",
      list(
        slot = "js-msg-slot",
        html = js_tagged_html("js-msg-btn", "From a message", "Sent by a custom message")
      )
    )
  })
  output$js_set_seen <- shiny::renderText(paste("js_set:", input$js_set %||% "nothing yet"))
  output$js_bound_seen <- shiny::renderText(paste("js_bound:", input$js_bound %||% "not bound"))

  ticks <- shiny::reactiveVal(0L)
  shiny::observeEvent(input$js_tick, ticks(ticks() + 1L))
  output$js_value_out <- shiny::renderText(paste("Tick", ticks()))

  shiny::observeEvent(input$js_sjs_hide, shinyjs::hide("js-sjs-btn"))
  shiny::observeEvent(input$js_sjs_show, shinyjs::show("js-sjs-btn"))
  shiny::observeEvent(input$js_sjs_toggle, shinyjs::toggleClass("js-sjs-btn", "btn-warning"))

  output$js_dt <- DT::renderDT(
    DT::datatable(
      data.frame(site = c("Site 01", "Site 02", "Site 03"), patients = c(45L, 82L, 119L)),
      rownames = FALSE,
      options = list(dom = "t"),
      callback = DT::JS(sprintf(
        "const cell = table.cell(0, 0).node();
        cell.setAttribute('%s', 'Tagged by the render callback');
        cell.id = 'js-dt-cell';",
        bench_contract()$attributes$tip
      ))
    ),
    server = FALSE
  )

  shiny::observeEvent(input$js_names_show, {
    session$sendCustomMessage("show", list(text = "the app's own show"))
    hovercraft::tip_show("js_names_tip")
  })
}

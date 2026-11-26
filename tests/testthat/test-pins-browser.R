skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if_not_installed("DT")
skip_if_not_installed("reactable")
skip_if_not_installed("bslib")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# Several pins at once: use_hovercraft(max_pins =) for the app, tip_table(max_pins =) for a table of its own, on real DT and
# reactable tables in a running Shiny app. A pin is opened by the engine's own door, as the pin key and the pin button do.
describe("several pins in a Shiny app", {
  scores <- data.frame(
    team = c("Owls", "Foxes", "Hares", "Lynx", "Voles", "Moles"),
    wins = c(7, 5, 2, 9, 1, 4)
  )
  app <- start_app(
    function(scores) {
      rows <- function(...) tip_spec("**{{ team }}**: {{ wins | number:0 }} wins", pin = TRUE, events = "pin")
      dt <- function(id, ...) {
        DT::renderDT(
          tip_table(
            DT::datatable(scores, rownames = FALSE, options = list(pageLength = 6)),
            rows = rows(),
            id = id,
            ...
          ),
          server = FALSE
        )
      }
      tag <- function(id) tip_on(shiny::actionButton(id, id), paste("Tag", id), id = id, pin = TRUE)
      ui <- bslib::page_fluid(
        use_hovercraft(max_pins = 3),
        tip_define(
          shared = tip_template(tip_title("{{ label }}"), tip_output("shared_out"))
        ),
        shiny::textOutput("shared_out"),
        tag("x"),
        tag("y"),
        # A tag pinned by its own HTML id: it has no tip id of its own.
        tip_on(shiny::actionButton("own", "own"), "Pinned by its HTML id", pin = TRUE),
        shiny::actionButton("modal", "Open a modal"),
        tip_on(
          shiny::actionButton("s1", "s1"),
          template = "shared",
          record = list(label = "S1"),
          id = "s1",
          pin = TRUE
        ),
        tip_on(
          shiny::actionButton("s2", "s2"),
          template = "shared",
          record = list(label = "S2"),
          id = "s2",
          pin = TRUE
        ),
        DT::DTOutput("a"),
        DT::DTOutput("b"),
        DT::DTOutput("c"),
        DT::DTOutput("t"),
        DT::DTOutput("t2"),
        DT::DTOutput("m-mt"),
        reactable::reactableOutput("r"),
        shiny::textOutput("seen_a")
      )
      server <- function(input, output, session) {
        output$t <- dt("t", max_pins = 1)
        output$t2 <- dt("t2", max_pins = 2)
        shiny::moduleServer("m", function(input, output, session) {
          output$mt <- DT::renderDT(
            tip_table(
              DT::datatable(scores, rownames = FALSE, options = list(pageLength = 6)),
              rows = rows(),
              id = "tips",
              max_pins = 1
            ),
            server = FALSE
          )
        })
        output$a <- dt("a")
        output$b <- dt("b", max_pins = 2)
        output$c <- dt("c", max_pins = 1)
        output$r <- reactable::renderReactable(
          tip_table(reactable::reactable(scores, defaultPageSize = 6), rows = rows(), id = "r", max_pins = 2)
        )
        output$shared_out <- shiny::renderText("The shared output")
        output$seen_a <- shiny::renderText(paste(vapply(input$a_pinned, function(p) p$row, 1L), collapse = ","))
        shiny::observeEvent(input$unmodal, shiny::removeModal())
        shiny::observeEvent(input$modal, {
          shiny::showModal(shiny::modalDialog(
            tip_on(shiny::actionButton("m1", "m1"), "Modal one", id = "m1", pin = TRUE),
            tip_on(shiny::actionButton("m2", "m2"), "Modal two", id = "m2", pin = TRUE),
            easyClose = TRUE
          ))
        })
      }
      shiny::shinyApp(ui, server)
    },
    scores = scores
  )
  page <- local_page(app$url, environment())
  id_attribute <- tip_attribute("id")
  open_attribute <- tip_contract()$attributes$open
  open_ids <- function() {
    got <- page$evaluate(sprintf(
      "JSON.stringify([...document.querySelectorAll('[%s=\"pin\"]')].map((n) => n.getAttribute('%s') ?? n.id).sort())",
      open_attribute,
      id_attribute
    ))
    as.character(jsonlite::fromJSON(got))
  }
  # Pins an item by the engine's own door, as the pin key and the pin button do.
  pin <- function(id) {
    page$evaluate(sprintf(
      paste0(
        "(() => { const n = document.querySelector('[%s=\"%s\"]') ?? document.getElementById('%s'); ",
        "return window.Hovercraft.pin(n, { by: 'reader', via: 'key' }); })()"
      ),
      id_attribute,
      id,
      id
    ))
  }
  close_all <- function() {
    page$evaluate("window.Hovercraft.close(); 0")
    expect_true(page$wait_until(sprintf("document.querySelectorAll('[%s=\"pin\"]').length === 0", open_attribute)))
  }
  # Waits for exactly these pins to be open, then says so.
  pins_are <- function(ids) {
    want <- jsonlite::toJSON(sort(ids))
    page$wait_until(sprintf(
      "JSON.stringify([...document.querySelectorAll('[%s=\"pin\"]')].map((n) => n.getAttribute('%s') ?? n.id).sort()) === %s",
      open_attribute,
      id_attribute,
      jsonlite::toJSON(as.character(want), auto_unbox = TRUE)
    ))
  }

  it("starts and connects, with the page's limit of three", {
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    for (table in c("a", "b", "c", "r")) {
      expect_true(
        page$wait_until(sprintf(
          "document.querySelector('[%s^=\"%s/row/\"]') !== null",
          id_attribute,
          table
        )),
        info = table
      )
    }
    expect_identical(
      page$evaluate(sprintf("document.documentElement.getAttribute('%s')", tip_contract()$attributes$pins)),
      "10"
    )
  })

  it("keeps the app's limit for tags and for a table with no count of its own, and closes the oldest", {
    pin("x")
    pin("y")
    pin("a/row/1")
    expect_true(pins_are(c("x", "y", "a/row/1")))
    pin("a/row/2")
    expect_true(pins_are(c("y", "a/row/1", "a/row/2")))
    # `_pinned` lists the table's rows, oldest first, with the pin that closed gone.
    expect_true(page$wait_until("document.getElementById('seen_a').textContent === '1,2'"))
    pin("a/row/3")
    expect_true(pins_are(c("a/row/1", "a/row/2", "a/row/3")))
    expect_true(page$wait_until("document.getElementById('seen_a').textContent === '1,2,3'"))
  })

  it("gives a table its own count, closing that table's oldest and no other", {
    pin("b/row/1")
    pin("b/row/2")
    expect_true(pins_are(c("a/row/1", "a/row/2", "a/row/3", "b/row/1", "b/row/2")))
    pin("b/row/3")
    expect_true(pins_are(c("a/row/1", "a/row/2", "a/row/3", "b/row/2", "b/row/3")))
  })

  it("closes no row of a table with its own count when a pin elsewhere reaches the app's limit", {
    pin("x")
    # The app's three are now a/row/2, a/row/3 and x: a/row/1 was the oldest. The table b keeps both of its pins.
    expect_true(pins_are(c("a/row/2", "a/row/3", "x", "b/row/2", "b/row/3")))
  })

  it("keeps separate counts for two tables, each in its own pool", {
    pin("c/row/1")
    expect_true(pins_are(c("a/row/2", "a/row/3", "x", "b/row/2", "b/row/3", "c/row/1")))
    pin("c/row/2")
    expect_true(pins_are(c("a/row/2", "a/row/3", "x", "b/row/2", "b/row/3", "c/row/2")))
  })

  it("counts a reactable's rows the same way", {
    pin("r/row/1")
    pin("r/row/2")
    pin("r/row/3")
    expect_true(pins_are(c("a/row/2", "a/row/3", "x", "b/row/2", "b/row/3", "c/row/2", "r/row/2", "r/row/3")))
  })

  it("keeps several pins open inside a modal", {
    close_all()
    page$click_now("#modal")
    expect_true(page$wait_until(
      "document.querySelector('.modal.show') !== null && document.getElementById('m2')?.getClientRects().length > 0"
    ))
    pin("m1")
    pin("m2")
    expect_true(pins_are(c("m1", "m2")))
  })

  it("lets two pins share one output: it shows in one tip and the other says where it went", {
    close_all()
    page$evaluate("Shiny.setInputValue('unmodal', 1, { priority: 'event' }); 0")
    expect_true(page$wait_until("document.querySelector('.modal.show') === null"))
    pin("s1")
    pin("s2")
    expect_true(pins_are(c("s1", "s2")))
    moved <- "[...document.querySelectorAll('.hc-output-moved')].filter((n) => !n.hidden && n.offsetParent !== null).map((n) => n.textContent)"
    expect_true(page$wait_until(sprintf("JSON.stringify(%s) === '[\"Shown in another tip.\"]'", moved)))
    expect_true(page$wait_until(
      "document.querySelectorAll('.hc-tip-pin:not([hidden]) #shared_out').length === 1"
    ))
  })

  it("closes the oldest of the app's pins when it was pinned by the tag's own HTML id", {
    close_all()
    pin("own")
    pin("x")
    pin("y")
    expect_true(pins_are(c("own", "x", "y")))
    pin("a/row/1")
    expect_true(pins_are(c("x", "y", "a/row/1")))
  })

  it("reports a pin that a table's own count closes as the engine's limit does, and never lists too many", {
    close_all()
    page$evaluate(paste(
      "window.seen = { pin: [], pinned: [] };",
      "$(document).on('shiny:inputchanged', (e) => {",
      "  if (e.name === 'b_pin') window.seen.pin.push(e.value);",
      "  if (e.name.startsWith('b_pinned')) window.seen.pinned.push(e.value.length);",
      "}); 0"
    ))
    pin("b/row/1")
    pin("b/row/2")
    pin("b/row/3")
    expect_true(pins_are(c("b/row/2", "b/row/3")))
    expect_true(page$wait_until("window.seen.pin.some((v) => v.state === 'closed')"))
    closed <- jsonlite::fromJSON(page$evaluate(
      "JSON.stringify(window.seen.pin.filter((v) => v.state === 'closed')[0])"
    ))
    expect_identical(closed$reason, "replaced")
    expect_identical(closed$by, "reader")
    expect_identical(closed$via, "key")
    expect_identical(closed$row, 1L)
    expect_lte(max(unlist(jsonlite::fromJSON(page$evaluate("JSON.stringify(window.seen.pinned)")))), 2L)
  })

  it("keeps tables whose ids start alike apart, and counts a table in a module with its plain id", {
    close_all()
    pin("t/row/1")
    pin("t2/row/1")
    pin("t2/row/2")
    expect_true(pins_are(c("t/row/1", "t2/row/1", "t2/row/2")))
    pin("t/row/2")
    expect_true(pins_are(c("t/row/2", "t2/row/1", "t2/row/2")))
    pin("m-tips/row/1")
    pin("m-tips/row/2")
    expect_true(pins_are(c("t/row/2", "t2/row/1", "t2/row/2", "m-tips/row/2")))
  })
})

# The page's own settings, on a static page: a second, different number of pins, and the letter keys.
describe("the page's pin settings", {
  collected <- "window.warned = []; const w = console.warn; console.warn = (...a) => { window.warned.push(a.join(' ')); w(...a); };"
  page_of <- function(...) {
    file <- withr::local_tempfile(fileext = ".html", .local_envir = parent.frame(2))
    htmltools::save_html(htmltools::tags$html(htmltools::tags$head(), htmltools::tags$body(...)), file)
    file
  }
  pinnable <- function(id) {
    tip_on(
      htmltools::tags$button(id, id = id, class = "target"),
      paste("Tip", id),
      id = id,
      pin = TRUE
    )
  }
  pin_attribute <- tip_contract()$attributes$pins
  open_count <- function(page) {
    page$evaluate(sprintf("document.querySelectorAll('[%s=\"pin\"]').length", tip_contract()$attributes$open))
  }

  it("warns on the console for a second, different number of pins and keeps the first", {
    file <- page_of(use_hovercraft(max_pins = 2), use_hovercraft(max_pins = 3), pinnable("one"))
    page <- local_page(file, environment(), init = collected)
    expect_true(page$wait_until(sprintf("document.documentElement.getAttribute('%s') === '2'", pin_attribute)))
    expect_true(page$wait_until("window.warned.some((x) => x.includes('max_pins'))"))
    expect_identical(page$evaluate(sprintf("document.documentElement.getAttribute('%s')", pin_attribute)), "2")
  })

  it("does not warn for the same number twice", {
    file <- page_of(use_hovercraft(max_pins = 2), use_hovercraft(max_pins = 2), pinnable("one"))
    page <- local_page(file, environment(), init = collected)
    expect_true(page$wait_until(sprintf("document.documentElement.getAttribute('%s') === '2'", pin_attribute)))
    expect_false(isTRUE(page$evaluate("window.warned.some((x) => x.includes('max_pins'))")))
  })

  it("keeps the engine's one pin when max_pins is left out", {
    file <- page_of(use_hovercraft(), pinnable("one"), pinnable("two"))
    page <- local_page(file, environment())
    expect_true(page$wait_until("window.Hovercraft !== undefined"))
    for (id in c("one", "two")) {
      page$evaluate(sprintf(
        "window.Hovercraft.pin(document.getElementById('%s'), { by: 'reader', via: 'key' }); 0",
        id
      ))
    }
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('[%s=\"pin\"]').length === 1",
      tip_contract()$attributes$open
    )))
  })

  it("pins by the letter key by default, and by the button when keys are off", {
    on <- local_page(page_of(use_hovercraft(), pinnable("one")), environment())
    expect_true(on$wait_until("window.HovercraftPin !== undefined"))
    on$evaluate("document.getElementById('one').focus(); 0")
    on$press("p")
    expect_true(on$wait_until(sprintf(
      "document.querySelectorAll('[%s=\"pin\"]').length === 1",
      tip_contract()$attributes$open
    )))
    expect_identical(on$evaluate("document.getElementById('one').getAttribute('aria-keyshortcuts')"), "P")

    off <- local_page(page_of(use_hovercraft(keys = FALSE), pinnable("one")), environment())
    expect_true(off$wait_until("window.HovercraftPin !== undefined"))
    off$evaluate("document.getElementById('one').focus(); 0")
    off$press("p")
    expect_identical(open_count(off), 0L)
    expect_true(off$wait_until("document.getElementById('one').getAttribute('aria-keyshortcuts') === null"))
    # The engine's own Escape stays, and the pin is still there for a pointer through the door the button uses.
    off$evaluate("window.Hovercraft.pin(document.getElementById('one'), { by: 'reader', via: 'click' }); 0")
    expect_true(off$wait_until(sprintf(
      "document.querySelectorAll('[%s=\"pin\"]').length === 1",
      tip_contract()$attributes$open
    )))
    off$press("Escape")
    expect_true(off$wait_until(sprintf(
      "document.querySelectorAll('[%s=\"pin\"]').length === 0",
      tip_contract()$attributes$open
    )))
  })

  # The pin key also acts while the pointer hovers the item and its hint is on show; the hint's item wins over the focus.
  pinned_ids <- function(page) {
    page$evaluate(sprintf(
      "JSON.stringify([...document.querySelectorAll('[%s=\"pin\"]')].map((n) => n.id))",
      tip_contract()$attributes$open
    ))
  }

  it("pins by the letter key while the pointer hovers the item", {
    page <- local_page(page_of(use_hovercraft(), pinnable("one")), environment())
    expect_true(page$wait_until("window.HovercraftPin !== undefined"))
    expect_length(page$hover("#one"), 1L)
    page$press("p")
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('[%s=\"pin\"]').length === 1",
      tip_contract()$attributes$open
    )))
  })

  # A synthetic key press at the element that holds the focus, also inside a shadow root: was it taken (prevented)?
  taken <- function(page, key = "p", repeated = FALSE) {
    page$evaluate(sprintf(
      paste(
        "(() => { const e = new KeyboardEvent('keydown', { key: '%s', repeat: %s, bubbles: true, composed: true, cancelable: true });",
        "let at = document.activeElement; while (at.shadowRoot?.activeElement) at = at.shadowRoot.activeElement;",
        "at.dispatchEvent(e); return e.defaultPrevented; })()"
      ),
      key,
      tolower(as.character(repeated))
    ))
  }

  it("does not pin by the letter key from a hover while the focus is in a field", {
    file <- page_of(use_hovercraft(), pinnable("one"), htmltools::tags$input(id = "field", type = "text"))
    page <- local_page(file, environment())
    expect_true(page$wait_until("window.HovercraftPin !== undefined"))
    # The field holds the focus first; the hover then shows the item's tip, which the letter must not pin.
    page$evaluate("document.getElementById('field').focus(); 0")
    expect_length(page$hover("#one"), 1L)
    expect_identical(page$evaluate("document.activeElement.id"), "field")
    expect_false(taken(page))
    page$hold(0.3) # the key must pin nothing: nothing happening has no signal
    expect_identical(open_count(page), 0L)
  })

  it("does not pin by the letter key from a hover while the focus is in a shadow root's field or an ARIA text widget", {
    shadow <- htmltools::tagList(
      htmltools::tags$div(id = "host"),
      htmltools::tags$div(id = "combo", role = "combobox", tabindex = "0", "combo"),
      htmltools::tags$script(htmltools::HTML(
        "document.getElementById('host').attachShadow({ mode: 'open' }).innerHTML = '<input id=\"inner\" type=\"text\">';"
      ))
    )
    page <- local_page(page_of(use_hovercraft(), pinnable("one"), shadow), environment())
    expect_true(page$wait_until("window.HovercraftPin !== undefined"))
    page$evaluate("document.getElementById('host').shadowRoot.getElementById('inner').focus(); 0")
    expect_length(page$hover("#one"), 1L)
    expect_identical(page$evaluate("document.activeElement.id"), "host")
    expect_false(taken(page))
    page$evaluate("document.getElementById('combo').focus(); 0")
    expect_length(page$hover("#one"), 1L)
    expect_false(taken(page))
    page$hold(0.3) # the key must pin nothing: nothing happening has no signal
    expect_identical(open_count(page), 0L)
  })

  it("pins the focused item when the tip on show is another item's and the pointer is not on it", {
    page <- local_page(page_of(use_hovercraft(), pinnable("one"), pinnable("two")), environment())
    expect_true(page$wait_until("window.HovercraftPin !== undefined"))
    expect_length(page$hover("#one"), 1L)
    # The pointer leaves and the focus moves on at once, as a reader does, while the tip of "one" is still on show.
    page$mouse(5, 5, FALSE)
    page$evaluate("document.getElementById('two').focus(); 0")
    page$press("p", wait = 0.2)
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('[%s=\"pin\"]').length === 1",
      tip_contract()$attributes$open
    )))
    expect_identical(jsonlite::fromJSON(pinned_ids(page)), "two")
    page$press("Escape")
    # A tip that the page opens (tip_show() does it) is not under the pointer: the key stays with the focused item.
    page$mouse(5, 5, FALSE)
    page$evaluate("document.getElementById('two').focus(); window.Hovercraft.show(document.getElementById('one')); 0")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not(.%s-pin):not([hidden])') !== null",
      tip_names()$css,
      tip_names()$css
    )))
    page$press("p", wait = 0.2)
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('[%s=\"pin\"]').length === 1",
      tip_contract()$attributes$open
    )))
    expect_identical(jsonlite::fromJSON(pinned_ids(page)), "two")
  })

  it("pins once for a key held down, on the item whose tip was shown", {
    page <- local_page(page_of(use_hovercraft(), pinnable("one"), pinnable("two")), environment())
    expect_true(page$wait_until("window.HovercraftPin !== undefined"))
    page$evaluate("document.getElementById('one').focus(); 0")
    expect_length(page$hover("#two"), 1L)
    page$press("p")
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('[%s=\"pin\"]').length === 1",
      tip_contract()$attributes$open
    )))
    # The first press closed the hint; the auto-repeat of the held key must not fall back to the focused item.
    expect_false(taken(page, repeated = TRUE))
    page$hold(0.3) # a repeat must pin nothing: nothing happening has no signal
    expect_identical(jsonlite::fromJSON(pinned_ids(page)), "two")
  })

  it("pins the item whose tip is shown, not an item around it", {
    inner <- tip_on(
      htmltools::tags$span("inner", id = "inner", class = "px-1", tabindex = "0"),
      "The inner tip",
      id = "inner_tip"
    )
    outer <- tip_on(
      htmltools::tags$div(id = "outer", tabindex = "0", style = "padding: 2rem", "outer ", inner),
      "The outer tip",
      id = "outer_tip",
      pin = TRUE
    )
    page <- local_page(page_of(use_hovercraft(), outer), environment())
    expect_true(page$wait_until("window.HovercraftPin !== undefined"))
    expect_match(paste(page$hover("#inner"), collapse = " "), "The inner tip")
    page$press("p")
    page$hold(0.3) # the inner tip cannot pin, and the key must not pin the item around it: nothing happening has no signal
    expect_identical(open_count(page), 0L)
  })

  it("pins by the letter key from a hover while the focus is on a check box, which takes no letters", {
    file <- page_of(use_hovercraft(), pinnable("one"), htmltools::tags$input(id = "ck", type = "checkbox"))
    page <- local_page(file, environment())
    expect_true(page$wait_until("window.HovercraftPin !== undefined"))
    page$evaluate("document.getElementById('ck').focus(); 0")
    expect_length(page$hover("#one"), 1L)
    expect_identical(page$evaluate("document.activeElement.id"), "ck")
    expect_true(taken(page))
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('[%s=\"pin\"]').length === 1",
      tip_contract()$attributes$open
    )))
  })

  it("does not pin by the letter key from a hover when the keys are off", {
    page <- local_page(page_of(use_hovercraft(keys = FALSE), pinnable("one")), environment())
    expect_true(page$wait_until("window.HovercraftPin !== undefined"))
    expect_length(page$hover("#one"), 1L)
    page$press("p")
    expect_identical(open_count(page), 0L)
  })

  it("pins the item whose hint is on show when the focus is on another, and none when that hint cannot pin", {
    plain <- tip_on(htmltools::tags$button("plain", id = "plain", class = "target"), "A plain tip", id = "plain")
    page <- local_page(page_of(use_hovercraft(), pinnable("one"), pinnable("two"), plain), environment())
    expect_true(page$wait_until("window.HovercraftPin !== undefined"))
    page$evaluate("document.getElementById('one').focus(); 0")
    expect_length(page$hover("#two"), 1L)
    page$press("p")
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('[%s=\"pin\"]').length === 1",
      tip_contract()$attributes$open
    )))
    expect_identical(jsonlite::fromJSON(pinned_ids(page)), "two")
    page$press("Escape")
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('[%s=\"pin\"]').length === 0",
      tip_contract()$attributes$open
    )))
    page$evaluate("document.getElementById('one').focus(); 0")
    expect_length(page$hover("#plain"), 1L)
    page$press("p")
    page$hold(0.3) # the key must pin nothing: nothing happening has no signal
    expect_identical(open_count(page), 0L)
  })

  it("switches one key off and leaves the other on", {
    page <- local_page(page_of(use_hovercraft(keys = c(more = FALSE)), pinnable("one")), environment())
    expect_true(page$wait_until("window.HovercraftPin !== undefined"))
    page$evaluate("document.getElementById('one').focus(); 0")
    page$press("p")
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('[%s=\"pin\"]').length === 1",
      tip_contract()$attributes$open
    )))
    skip <- local_page(page_of(use_hovercraft(keys = c(pin = FALSE)), pinnable("one")), environment())
    expect_true(skip$wait_until("window.HovercraftPin !== undefined"))
    skip$evaluate("document.getElementById('one').focus(); 0")
    skip$press("p")
    expect_identical(open_count(skip), 0L)
  })

  it("warns for a number that is written twice differently, also when the first is the default one", {
    file <- page_of(use_hovercraft(max_pins = 1), use_hovercraft(max_pins = 3), pinnable("one"))
    page <- local_page(file, environment(), init = collected)
    expect_true(page$wait_until("window.warned.some((x) => x.includes('max_pins'))"))
    expect_identical(page$evaluate(sprintf("document.documentElement.getAttribute('%s')", pin_attribute)), "1")
  })

  it("warns for keys that differ and keeps the first", {
    file <- page_of(use_hovercraft(keys = FALSE), use_hovercraft(keys = c(more = FALSE)), pinnable("one"))
    page <- local_page(file, environment(), init = collected)
    expect_true(page$wait_until("window.warned.some((x) => x.includes('keys'))"))
    expect_true(page$wait_until(sprintf(
      "document.documentElement.getAttribute('%s') === 'pin more'",
      tip_names()$prefix |> (\(p) paste0("data-", p, "-keys-off"))()
    )))
  })

  it("takes the pin key's announcement off the rows of a reactable that writes it on a frame, when keys are off", {
    skip_if_not_installed("reactable")
    scores <- data.frame(team = c("Owls", "Foxes"), wins = c(7, 5))
    widget <- tip_table(
      reactable::reactable(scores),
      rows = tip_spec("{{ team }}", pin = TRUE),
      id = "r"
    )
    shortcuts <- "document.querySelectorAll('[aria-keyshortcuts]').length"
    rows_ready <- sprintf("document.querySelectorAll('[%s^=\"r/row/\"]').length >= 2", tip_attribute("id"))
    on <- local_page(page_of(use_hovercraft(), widget), environment())
    expect_true(on$wait_until(rows_ready))
    expect_true(on$wait_until(paste(shortcuts, "> 0")))
    off <- local_page(page_of(use_hovercraft(keys = FALSE), widget), environment())
    expect_true(off$wait_until(rows_ready))
    # The rows are marked on a frame, so give it a few before saying nothing was written.
    off$evaluate("new Promise((resolve) => setTimeout(resolve, 600))")
    expect_identical(off$evaluate(shortcuts), 0L)
  })
})

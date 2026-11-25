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
        reactable::reactableOutput("r"),
        shiny::textOutput("seen_a")
      )
      server <- function(input, output, session) {
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
      "JSON.stringify([...document.querySelectorAll('[%s=\"pin\"]')].map((n) => n.getAttribute('%s')).sort())",
      open_attribute,
      id_attribute
    ))
    as.character(jsonlite::fromJSON(got))
  }
  # Pins an item by the engine's own door, as the pin key and the pin button do.
  pin <- function(id) {
    page$evaluate(sprintf(
      "(() => { const n = document.querySelector('[%s=\"%s\"]'); return window.Hovercraft.pin(n, { by: 'reader', via: 'key' }); })()",
      id_attribute,
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
      "JSON.stringify([...document.querySelectorAll('[%s=\"pin\"]')].map((n) => n.getAttribute('%s')).sort()) === %s",
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
    page$evaluate("document.getElementById('modal').click(); 0")
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
})

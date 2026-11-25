skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# The page scripts of the interim branch (hovercraft #149): the key announcements as a list of keys, and a More button
# that does not stay on a plain hint. Static pages, no Shiny.
describe("the page scripts on a static page", {
  page_file <- function(..., .env = parent.frame()) {
    file <- withr::local_tempfile(fileext = ".html", .local_envir = .env)
    htmltools::save_html(
      htmltools::tags$html(htmltools::tags$head(), htmltools::tags$body(style = "min-height: 700px;", ...)),
      file
    )
    file
  }
  place <- function(x, style) htmltools::div(style = paste("position: absolute;", style), x)
  keys_of <- function(page, id) {
    page$evaluate(sprintf("document.getElementById('%s').getAttribute('aria-keyshortcuts')", id))
  }

  it("takes the pin key out of a list of keys and leaves the others", {
    page <- local_page(
      page_file(
        use_hovercraft(keys = c(pin = FALSE)),
        tip_on(htmltools::tags$button("Both", id = "both"), "Text", more = "Long", pin = TRUE, id = "both"),
        tip_on(htmltools::tags$button("Pin", id = "pin_only"), "Text", pin = TRUE, id = "pin_only")
      ),
      environment()
    )
    expect_true(page$wait_until("document.getElementById('both').getAttribute('aria-keyshortcuts') === 'M'"))
    expect_true(page$wait_until("document.getElementById('pin_only').getAttribute('aria-keyshortcuts') === null"))
  })

  it("takes the M key out of a list of keys, and the attribute goes when no key is left", {
    page <- local_page(
      page_file(
        use_hovercraft(keys = c(more = FALSE)),
        tip_on(htmltools::tags$button("Both", id = "both"), "Text", more = "Long", pin = TRUE, id = "both"),
        tip_on(htmltools::tags$button("More", id = "more_only"), "Text", more = "Long", id = "more_only")
      ),
      environment()
    )
    expect_true(page$wait_until("document.getElementById('both').getAttribute('aria-keyshortcuts') === 'P'"))
    expect_true(page$wait_until("document.getElementById('more_only').getAttribute('aria-keyshortcuts') === null"))
  })

  it("leaves a key of the page's own, and takes the pin key out of a list it shares with one", {
    page <- local_page(
      page_file(
        use_hovercraft(keys = c(pin = FALSE)),
        tip_on(htmltools::tags$button("Own", id = "own"), "Text", pin = TRUE, id = "own")
      ),
      environment()
    )
    expect_true(page$wait_until("window.HovercraftRuntime !== undefined"))
    expect_true(page$wait_until("document.getElementById('own').getAttribute('aria-keyshortcuts') === null"))
    # The page gives the item a key of its own next to the pin key: the attribute is watched, and only P goes.
    page$evaluate("document.getElementById('own').setAttribute('aria-keyshortcuts', 'Alt+R P'); 0")
    expect_true(page$wait_until("document.getElementById('own').getAttribute('aria-keyshortcuts') === 'Alt+R'"))
    # A key of the page's own that is no hovercraft key stays whole.
    page$evaluate("document.getElementById('own').setAttribute('aria-keyshortcuts', 'Alt+R'); 0")
    expect_true(page$wait_until("document.getElementById('own').getAttribute('aria-keyshortcuts') === 'Alt+R'"))
  })

  it("takes the keys off an item that is inserted later", {
    page <- local_page(
      page_file(
        use_hovercraft(keys = FALSE),
        tip_on(htmltools::tags$button("First", id = "first"), "Text", more = "Long", pin = TRUE, id = "first")
      ),
      environment()
    )
    expect_true(page$wait_until("document.getElementById('first').getAttribute('aria-keyshortcuts') === null"))
    page$evaluate(sprintf(
      "(() => { const b = document.createElement('button'); b.id = 'late'; b.setAttribute('%s', 'Late'); b.setAttribute('%s', ''); b.setAttribute('%s', 'late'); b.setAttribute('aria-keyshortcuts', 'P M'); document.body.append(b); })()",
      tip_attribute("tip"),
      tip_attribute("pin"),
      tip_attribute("id")
    ))
    expect_true(page$wait_until("document.getElementById('late').getAttribute('aria-keyshortcuts') === null"))
  })

  it("takes the M key out of an item that has More and no tip id or pin", {
    for (keys in list(c(more = FALSE), FALSE)) {
      page <- local_page(
        page_file(
          use_hovercraft(keys = keys),
          tip_on(htmltools::tags$button("Bare", id = "bare"), "Text", more = "Long")
        ),
        environment()
      )
      expect_true(page$wait_until("window.HovercraftRuntime !== undefined"))
      expect_true(page$wait_until("document.getElementById('bare').getAttribute('aria-keyshortcuts') === null"))
    }
  })

  it("opens a hint short that is shown again while the last one is still fading", {
    page <- local_page(
      page_file(
        use_hovercraft(),
        place(
          tip_on(htmltools::tags$button("Long", id = "lg"), "Short words", more = "Long form words", id = "lg"),
          "left: 60px; top: 100px;"
        )
      ),
      environment()
    )
    expect_true(page$wait_until("window.HovercraftMore !== undefined && window.Hovercraft !== undefined"))
    page$hover("#lg")
    expect_true(page$wait_until("document.querySelectorAll('.hovercraft-more-strip-button').length === 1"))
    page$evaluate("document.querySelector('.hovercraft-more-strip-button').click(); 0")
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip:not(.hc-tip-pin):not([hidden])')?.textContent.includes('Long form words') === true"
    ))
    # The pointer leaves; as soon as the engine has closed the hint, the page shows it again (its button still fades).
    page$mouse(890, 690)
    expect_true(page$wait_until("document.getElementById('lg').getAttribute('data-hovercraft-tip-open') === null"))
    page$evaluate("window.Hovercraft.show(document.getElementById('lg')); 0")
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip:not(.hc-tip-pin):not([hidden])')?.textContent.includes('Short words') === true"
    ))
    expect_identical(
      page$evaluate(
        "document.querySelector('.hc-tip:not(.hc-tip-pin):not([hidden])').textContent.includes('Long form words')"
      ),
      FALSE
    )
    expect_identical(page$evaluate("document.querySelector('.hovercraft-more-strip-button').textContent"), "More")
  })

  it("closes the pin whose More button has the focus on Escape, not the newest pin", {
    page <- local_page(
      page_file(
        use_hovercraft(max_pins = 3),
        place(
          tip_on(htmltools::tags$button("One", id = "one"), "Short one", more = "Long one", id = "one", pin = TRUE),
          "left: 40px; top: 60px;"
        ),
        place(
          tip_on(htmltools::tags$button("Two", id = "two"), "Short two", more = "Long two", id = "two", pin = TRUE),
          "left: 400px; top: 60px;"
        )
      ),
      environment()
    )
    expect_true(page$wait_until("window.HovercraftMore !== undefined && window.Hovercraft !== undefined"))
    for (id in c("one", "two")) {
      page$evaluate(sprintf(
        "window.Hovercraft.pin(document.getElementById('%s'), { by: 'reader', via: 'key' }); 0",
        id
      ))
    }
    expect_true(page$wait_until("document.querySelectorAll('.hovercraft-more-strip-button').length === 2"))
    # The button that sits on the box of the first (older) pin takes the focus.
    page$evaluate(
      "(() => { const box = [...document.querySelectorAll('.hc-tip-pin:not([hidden])')].find((b) => b.textContent.includes('Short one')); const r = box.getBoundingClientRect(); [...document.querySelectorAll('.hovercraft-more-strip-button')].find((b) => { const q = b.getBoundingClientRect(); return q.left >= r.left && q.right <= r.right && q.top >= r.top && q.bottom <= r.bottom; }).focus(); })()"
    )
    page$press("Escape", wait = 0.2)
    expect_true(page$wait_until("document.querySelectorAll('.hc-tip-pin:not([hidden])').length === 1"))
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip-pin:not([hidden])').textContent.includes('Short two')"
    ))
    # The first pin is closed: the item is not pinned. (The focus goes back to it, which may show its hint, so the item
    # can carry the hint's mark a moment later; the test used to read it before that.)
    expect_identical(
      page$evaluate("document.getElementById('one').getAttribute('data-hovercraft-tip-open') === 'pin'"),
      FALSE
    )
  })

  it("keeps the room for the button under the last line of a bottom sheet", {
    page <- local_page(
      page_file(
        use_hovercraft(),
        place(
          tip_on(
            htmltools::tags$button("Sheet", id = "sh"),
            "Short words for the sheet that are a few lines long when the window is narrow.",
            more = "Long form words for the sheet that are a few lines long when the window is narrow.",
            id = "sh",
            pin = TRUE
          ),
          "left: 20px; top: 100px;"
        )
      ),
      environment()
    )
    expect_true(page$wait_until("window.HovercraftMore !== undefined && window.Hovercraft !== undefined"))
    page$resize(375, 700)
    page$evaluate("window.Hovercraft.pin(document.getElementById('sh'), { by: 'reader', via: 'key' }); 0")
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin[data-side=\"sheet\"]:not([hidden])') !== null"))
    expect_true(page$wait_until("document.querySelectorAll('.hovercraft-more-strip-button').length === 1"))
    # The button starts below the last text of the sheet.
    expect_true(page$wait_until(
      "(() => { const box = document.querySelector('.hc-tip-pin[data-side=\"sheet\"]'); const texts = box.querySelectorAll('.hc-tip-text'); const b = document.querySelector('.hovercraft-more-strip-button').getBoundingClientRect(); return b.top >= texts[texts.length - 1].getBoundingClientRect().bottom; })()"
    ))
  })

  it("closes the More button of a hint at once when the next hint has no long form", {
    page <- local_page(
      page_file(
        use_hovercraft(),
        place(
          tip_on(htmltools::tags$button("Long", id = "lg"), "Short words", more = "Long form words", id = "lg"),
          "left: 60px; top: 100px;"
        ),
        place(tip_on(htmltools::tags$button("Plain", id = "pl"), "Plain words", id = "pl"), "left: 200px; top: 100px;")
      ),
      environment()
    )
    expect_true(page$wait_until("window.HovercraftMore !== undefined && window.Hovercraft !== undefined"))
    centre <- function(id) {
      jsonlite::fromJSON(page$evaluate(sprintf(
        "(() => { const r = document.getElementById('%s').getBoundingClientRect(); return JSON.stringify([r.x + r.width / 2, r.y + r.height / 2]); })()",
        id
      )))
    }
    page$hover("#lg")
    expect_true(page$wait_until("document.querySelectorAll('.hovercraft-more-strip-button').length === 1"))
    # Straight onto the neighbour, the pointer never leaving the page's items for long.
    to <- centre("pl")
    page$mouse(to[[1]], to[[2]])
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip:not(.hc-tip-pin):not([hidden])')?.textContent.includes('Plain words') === true"
    ))
    # The button of the first hint stays no longer than the hint, not for the 600 ms it lingers when a hint fades.
    expect_identical(page$evaluate("document.querySelectorAll('.hovercraft-more-strip-button').length"), 0L)
  })
})

# The page scripts in a Shiny app: a disabled input's tip id and events, the tab stops of a pinned tip, the order of the
# start script when renderUI() adds a template, and the pool limit of a table next to a plain pin.
describe("the page scripts in a Shiny app", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("callr")
  skip_if_not_installed("httpuv")
  skip_if_not_installed("pkgload")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  scores <- data.frame(team = c("Owls", "Foxes", "Hares"), wins = c(7, 5, 2))
  app <- start_app(
    function(scores) {
      ui <- bslib::page_fluid(
        use_hovercraft(max_pins = 2),
        # The inline template is defined by the dependency of the tag renderUI() draws: it arrives with the element.
        shiny::actionButton("draw", "Draw"),
        shiny::uiOutput("late"),
        tip_on(shiny::textInput("tx", "Name"), "Help text", id = "x_tip", events = "hint", when_disabled = "Locked"),
        shiny::verbatimTextOutput("hint_seen"),
        tip_on(
          htmltools::tags$button("Out", id = "out", type = "button"),
          template = tip_template(tip_text("See [the docs](https://example.com/)."), tip_output("live")),
          more = "The long form.",
          pin = TRUE,
          id = "out_tip"
        ),
        shiny::textOutput("live"),
        DT::DTOutput("dt"),
        DT::DTOutput("dtp"),
        tip_on(htmltools::tags$button("Plain", id = "pl", type = "button"), "Plain pinned tip", pin = TRUE, id = "pl")
      )
      server <- function(input, output, session) {
        output$live <- shiny::renderText("Live text")
        output$hint_seen <- shiny::renderText(paste(unlist(input$x_tip_hint[c("state")]), collapse = " "))
        output$late <- shiny::renderUI({
          shiny::req(input$draw)
          tip_on(
            htmltools::tags$span("Late item", id = "late_item"),
            "Count: {{ n }}",
            record = list(n = 1),
            id = "late_tip"
          )
        })
        output$dtp <- DT::renderDT(
          tip_table(
            DT::datatable(scores, rownames = FALSE, options = list(dom = "t")),
            rows = tip_spec("**{{ team }}**", more = "The long form of {{ team }}.", pin = TRUE, to = "in_place"),
            id = "dtp",
            style = tip_style(theme = tip_theme(light = list(bg = "#1b3a6b", text = "#ffffff"), mode = "light"))
          ),
          server = FALSE
        )
        output$dt <- DT::renderDT(
          tip_table(
            DT::datatable(scores, rownames = FALSE, options = list(dom = "t")),
            rows = tip_spec("**{{ team }}**", more = "The long form of {{ team }}.", pin = TRUE),
            id = "dt",
            max_pins = 1
          ),
          server = FALSE
        )
      }
      shiny::shinyApp(ui, server)
    },
    scores = scores
  )
  page <- local_page(
    app$url,
    environment(),
    init = "window.__warns = []; const w = console.warn; console.warn = (...a) => { window.__warns.push(a.join(' ')); w.apply(console, a); };"
  )
  open_pins <- "[...document.querySelectorAll('.hc-tip-pin:not([hidden])')]"
  pin_key <- function(selector) {
    expect_true(page$wait_until(sprintf("document.querySelector('%s')?.hasAttribute('tabindex') === true", selector)))
    page$evaluate(sprintf("document.querySelector('%s').focus()", selector))
    page$press("p", wait = 0.2)
  }
  pin_tag <- function(id) {
    page$evaluate(sprintf("window.Hovercraft.pin(document.getElementById('%s'), { by: 'reader', via: 'key' }); 0", id))
  }
  close_all <- function() {
    page$evaluate("window.Hovercraft.close(); 0")
    expect_true(page$wait_until(sprintf("%s.length === 0", open_pins)))
  }

  it("starts", {
    expect_true(app$proc$is_alive(), info = paste(app$proc$read_error(), collapse = "\n"))
    expect_true(page$wait_until("document.querySelectorAll('#dt tbody tr[data-hovercraft-tip-id]').length === 3"))
  })

  it("does not say a template is not defined when renderUI() draws a tag with an inline template", {
    expect_identical(page$evaluate("window.__warns.filter((w) => /not defined/.test(w)).length"), 0L)
    page$click_now("#draw")
    expect_true(page$wait_until("document.getElementById('late_item') !== null"))
    page$settle()
    expect_identical(page$evaluate("window.__warns.filter((w) => /not defined/.test(w)).length"), 0L)
  })

  it("gives the wrapper of a disabled input the tip id and events of the tagged element", {
    page$evaluate("document.getElementById('tx').disabled = true; 0")
    wrapper <- sprintf("document.getElementById('tx').closest('.%s')", tip_disabled_class())
    expect_true(page$wait_until(sprintf("%s.getAttribute('%s') === 'x_tip'", wrapper, tip_attribute("id"))))
    expect_identical(
      page$evaluate(sprintf("%s.getAttribute('%s')", wrapper, tip_shiny_attribute())),
      "hint"
    )
    # The hint input is the tip's own, by the tip id the page gave.
    page$hover(sprintf(".%s", tip_disabled_class()))
    expect_true(page$wait_until("document.getElementById('hint_seen').textContent.includes('shown')"))
    page$evaluate("document.getElementById('tx').disabled = false; 0")
  })

  it("sends Shift+Tab from More to the tip's last visible control", {
    pin_tag("out")
    expect_true(page$wait_until(sprintf(
      "%s.some((b) => b.textContent.includes('the docs')) && document.querySelectorAll('.hovercraft-more-strip-button').length === 1",
      open_pins
    )))
    # The output's Retry button is in the box, hidden: it is no tab stop.
    expect_identical(
      page$evaluate("document.querySelector('.hc-tip-pin .hc-output-retry')?.closest('[hidden]') !== null"),
      TRUE
    )
    # Back from More to the link, not to a button nobody can see (the Retry button is the last of the tip's buttons).
    page$evaluate("document.querySelector('.hovercraft-more-strip-button').focus(); 0")
    page$evaluate(
      "document.activeElement.dispatchEvent(new KeyboardEvent('keydown', { key: 'Tab', shiftKey: true, bubbles: true, cancelable: true })); 0"
    )
    expect_true(page$wait_until("document.activeElement?.matches('.hc-tip-pin a[href]')"))
    close_all()
  })

  it("puts a table row's More button on the row's own box when its pool closes an older row", {
    # The table's pool allows one row; the app's allows two. Row 1, then the plain tag, then row 2: row 1 closes while
    # row 2 opens, and the button of row 2 must land on row 2's box, not on the plain tag's.
    pin_key("#dt tbody tr[data-hovercraft-tip-id=\"dt/row/1\"]")
    expect_true(page$wait_until(sprintf("%s.some((b) => b.textContent.includes('Owls'))", open_pins)))
    pin_tag("pl")
    expect_true(page$wait_until(sprintf("%s.some((b) => b.textContent.includes('Plain pinned'))", open_pins)))
    pin_key("#dt tbody tr[data-hovercraft-tip-id=\"dt/row/2\"]")
    expect_true(page$wait_until(sprintf(
      "%s.some((b) => b.textContent.includes('Foxes')) && !%s.some((b) => b.textContent.includes('Owls'))",
      open_pins,
      open_pins
    )))
    on_box <- paste0(
      "(text) => { const box = ",
      open_pins,
      ".find((b) => b.textContent.includes(text)); const r = box.getBoundingClientRect();",
      " return [...document.querySelectorAll('.hovercraft-more-strip-button')].filter((b) => { const q = b.getBoundingClientRect();",
      " const x = q.left + q.width / 2; const y = q.top + q.height / 2; return x >= r.left && x <= r.right && y >= r.top && y <= r.bottom; }).length; }"
    )
    expect_true(page$wait_until(sprintf("(%s)('Foxes') === 1", on_box)))
    expect_identical(page$evaluate(sprintf("(%s)('Plain pinned')", on_box)), 0L)
    close_all()
  })

  it("gives the More button of an in-place panel the colours of the panel's box", {
    pin_key("#dtp tbody tr[data-hovercraft-tip-id=\"dtp/row/1\"]")
    expect_true(page$wait_until(
      "document.querySelector('#dtp [data-hovercraft-panel] .hovercraft-more-strip-button') !== null"
    ))
    colours <- "
      (() => {
        const box = document.querySelector('#dtp [data-hovercraft-panel] .hc-tip');
        const button = document.querySelector('#dtp [data-hovercraft-panel] .hovercraft-more-strip-button');
        return JSON.stringify([getComputedStyle(box).color, getComputedStyle(box).backgroundColor, getComputedStyle(button).color, getComputedStyle(button).backgroundColor]);
      })()"
    expect_true(page$wait_until(sprintf(
      "(() => { const c = JSON.parse(%s); return c[0] === c[2] && c[1] === c[3]; })()",
      colours
    )))
    close_all()
  })
})

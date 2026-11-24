skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# tip_on(when_disabled = ) in a running bslib/Shiny app: a control disabled when the page is built, one switched off and
# on by Shiny's updateActionButton(), by script and by a disabled fieldset, inside a sidebar where the wrapper must not
# change a width. The reason shows by pointer and by keyboard, goes when the control is enabled, and has a lock.
describe("tip_on(when_disabled = ) in a Shiny app", {
  path <- find.package("hovercraft")
  port <- httpuv::randomPort()
  app <- callr::r_bg(
    function(port, path) {
      if (dir.exists(file.path(path, "inst"))) {
        pkgload::load_all(path, quiet = TRUE)
      } else {
        library(hovercraft)
      }
      off <- function(x) htmltools::tagAppendAttributes(x, disabled = "disabled")
      ui <- bslib::page_sidebar(
        title = "when_disabled",
        sidebar = bslib::sidebar(
          id = "side",
          open = TRUE,
          width = 300,
          shiny::actionButton("ref", "Reference", width = "100%"),
          tip_on(off(shiny::actionButton("wide", "Wide", width = "100%")), when_disabled = "Wide reason")
        ),
        shiny::actionButton("before", "Before"),
        tip_on(
          shiny::actionButton("up", "Update"),
          "Runs the **check**",
          when_disabled = "Pick a dataset first",
          id = "up_tip",
          events = "hint"
        ),
        tip_on(
          off(shiny::actionButton("built", "Built")),
          "Built tip",
          when_disabled = "Disabled at build",
          id = "built_tip"
        ),
        tip_on(shiny::actionButton("js", "Script"), when_disabled = "Script reason"),
        htmltools::tags$fieldset(
          id = "fs",
          tip_on(shiny::actionButton("in_fs", "Fieldset"), when_disabled = "Fieldset reason")
        ),
        shiny::actionButton("after", "After"),
        tip_on(shiny::textInput("txt", "Name"), when_disabled = "Sign in first"),
        tip_on(
          htmltools::tags$div(
            htmltools::tags$label(`for` = "sel", "Pick"),
            htmltools::tags$select(id = "sel", htmltools::tags$option("a"))
          ),
          when_disabled = "Select reason"
        ),
        tip_on(
          htmltools::tags$fieldset(id = "grp", htmltools::tags$legend("Group"), htmltools::tags$input(id = "in_grp")),
          when_disabled = "Group reason"
        ),
        tip_on(htmltools::tags$button(id = "aria", `aria-disabled` = "true", "Aria"), when_disabled = "Aria reason"),
        tip_on(shiny::actionButton("self", "Self"), "Self tip", when_disabled = "Already running"),
        htmltools::tags$div(
          tip_on(
            off(shiny::actionButton("styled", "Styled")),
            "Enabled tip",
            style = tip_style(theme = list("--hovercraft-tip-radius" = "13px")),
            placement = "right",
            when_disabled = "Styled reason"
          )
        ),
        htmltools::tags$div(
          id = "box400",
          style = "width: 400px",
          tip_on(off(shiny::actionButton("half", "Half", width = "50%")), when_disabled = "Half reason"),
          tip_on(off(htmltools::tags$button("W50", id = "w50", class = "btn w-50")), when_disabled = "W50 reason"),
          htmltools::tags$div(
            id = "grid",
            class = "d-grid",
            tip_on(off(shiny::actionButton("grid_btn", "Grid")), when_disabled = "Grid reason")
          )
        ),
        htmltools::tags$div(
          id = "flexrow",
          style = "display: flex; width: 400px; gap: 10px",
          tip_on(off(htmltools::tags$button("F1", id = "f1", class = "btn flex-fill")), when_disabled = "F1 reason"),
          htmltools::tags$button("F2", id = "f2", class = "btn flex-fill")
        ),
        shiny::actionButton("show_built", "Show built"),
        shiny::actionButton("enable", "Enable"),
        shiny::actionButton("disable", "Disable"),
        shiny::verbatimTextOutput("clicks"),
        shiny::verbatimTextOutput("hint")
      )
      server <- function(input, output, session) {
        n <- shiny::reactiveVal(0L)
        shiny::observeEvent(input$up, n(n() + 1L))
        shiny::observeEvent(input$self, shiny::updateActionButton(session, "self", disabled = TRUE))
        shiny::observeEvent(input$show_built, tip_show("built_tip"))
        shiny::observeEvent(input$enable, shiny::updateActionButton(session, "up", disabled = FALSE))
        shiny::observeEvent(input$disable, shiny::updateActionButton(session, "up", disabled = TRUE))
        output$clicks <- shiny::renderText(paste("clicks:", n()))
        output$hint <- shiny::renderText(paste(unlist(input$up_tip_hint[c("state", "text")]), collapse = " | "))
      }
      shiny::runApp(shiny::shinyApp(ui, server), port = port, host = "127.0.0.1", launch.browser = FALSE)
    },
    args = list(port = port, path = path),
    supervise = TRUE
  )
  withr::defer(app$kill())
  url <- paste0("http://127.0.0.1:", port)
  up <- wait_for_app(app, url)
  page <- local_page(url, environment())
  reason <- tip_attribute("reason")
  wrapper <- function(id) {
    sprintf("document.getElementById('%s').closest('.%s')", id, tip_disabled_class())
  }
  # The wrapper's state as the page sees it: the engine's reason attribute, the tab stop and the role.
  state <- function(id) {
    jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const w = %s; return JSON.stringify([w.getAttribute('%s'), w.getAttribute('tabindex'), w.getAttribute('role')]); })()",
      wrapper(id),
      reason
    )))
  }
  press_button <- function(id) page$evaluate(sprintf("document.getElementById('%s').click()", id))
  seen <- function(id, pattern) {
    page$wait_until(sprintf(
      "new RegExp(%s).test(document.getElementById(%s)?.textContent ?? '')",
      jsonlite::toJSON(pattern, auto_unbox = TRUE),
      jsonlite::toJSON(id, auto_unbox = TRUE)
    ))
  }
  # The tab stop the keyboard reaches, after pressing Tab from the button before the one under test.
  tab_to <- function(from) {
    page$evaluate(sprintf("document.getElementById('%s').focus()", from))
    page$press("Tab")
  }

  it("starts and connects", {
    expect_true(up, info = paste(app$read_error(), collapse = "\n"))
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === 'button'", wrapper("built"))))
  })

  it("shows the reason of a control that was disabled when the page was built, by the pointer", {
    expect_identical(page$hover("#built"), "Not available: Disabled at build")
  })

  it("reaches the reason by keyboard: the wrapper is a tab stop while the control is disabled", {
    expect_identical(state("built"), c("Disabled at build", "0", "button"))
    expect_identical(tab_to("up"), "Not available: Disabled at build")
    expect_identical(page$evaluate("document.activeElement.firstElementChild.id"), "built")
  })

  it("gives the reason a lock that adds no words, in the words' colour", {
    page$hover("#built")
    lock <- jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const line = document.querySelector('.%s-reason'); const s = getComputedStyle(line, '::before'); return JSON.stringify({ text: line.textContent, content: s.content, mask: s.maskImage || s.webkitMaskImage, width: parseFloat(s.width), colour: s.backgroundColor === getComputedStyle(line).color }); })()",
      tip_names()$css
    )))
    expect_identical(lock$text, "Not available: Disabled at build")
    expect_match(lock$content, "^\"?\"?$")
    expect_match(lock$mask, "^url\\(")
    expect_gt(lock$width, 4)
    expect_true(lock$colour)
  })

  it("does not change the width of a control that fills its column", {
    expect_equal(
      page$evaluate("document.getElementById('wide').getBoundingClientRect().width"),
      page$evaluate("document.getElementById('ref').getBoundingClientRect().width")
    )
    expect_equal(
      page$evaluate("document.getElementById('wide').parentElement.getBoundingClientRect().width"),
      page$evaluate("document.getElementById('ref').getBoundingClientRect().width")
    )
  })

  it("has no reason and no tab stop while the control is enabled, and its own tip shows", {
    expect_equal(state("up"), rep(NA, 3L))
    expect_identical(page$hover("#up"), "Runs the check")
    expect_identical(tab_to("before"), "Runs the check")
    expect_identical(page$evaluate("document.activeElement.id"), "up")
  })

  it("shows the reason when Shiny switches the control off, and takes it away when it is switched on", {
    press_button("disable")
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === 'button'", wrapper("up"))))
    expect_identical(state("up"), c("Pick a dataset first", "0", "button"))
    expect_identical(page$hover("#up"), "Not available: Pick a dataset first")
    expect_true(seen("hint", "^shown \\| Not available: Pick a dataset first"))
    press_button("enable")
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === null", wrapper("up"))))
    expect_equal(state("up"), rep(NA, 3L))
    expect_identical(page$hover("#up"), "Runs the check")
  })

  it("lets an enabled control be clicked again: the engine blocks the clicks of a control that still has a reason", {
    press_button("up")
    expect_true(seen("clicks", "clicks: 1"))
    page$click("#up")
    expect_true(seen("clicks", "clicks: 2"))
  })

  it("follows a control that a script switches off and on", {
    page$evaluate("document.getElementById('js').disabled = true")
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === 'button'", wrapper("js"))))
    expect_identical(page$hover("#js"), "Not available: Script reason")
    page$evaluate("document.getElementById('js').disabled = false")
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === null", wrapper("js"))))
    expect_equal(state("js"), rep(NA, 3L))
    expect_length(page$hover("#js"), 0L)
  })

  it("follows a control that a disabled fieldset switches off and on", {
    page$evaluate("document.getElementById('fs').disabled = true")
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === 'button'", wrapper("in_fs"))))
    expect_identical(page$hover("#in_fs"), "Not available: Fieldset reason")
    page$evaluate("document.getElementById('fs').disabled = false")
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === null", wrapper("in_fs"))))
  })

  # Brings the element into view, so the pointer can reach it on a page that is taller than the window.
  view <- function(id) {
    page$evaluate(sprintf("document.getElementById('%s').scrollIntoView({block: 'center'})", id))
    page$settle(0.1)
  }
  aria <- function(id) {
    jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const w = %s; return JSON.stringify([w.getAttribute('role'), w.getAttribute('aria-label'), w.getAttribute('tabindex')]); })()",
      wrapper(id)
    )))
  }
  width_of <- function(id) page$evaluate(sprintf("document.getElementById('%s').getBoundingClientRect().width", id))

  it("makes a disabled field a named group, never a button, so its own meaning is kept", {
    for (field in list(c("txt", "Name"), c("sel", "Pick"), c("grp", "Group"))) {
      view(field[[1]])
      page$evaluate(sprintf("document.getElementById('%s').disabled = true", field[[1]]))
      expect_true(page$wait_until(sprintf("%s.getAttribute('tabindex') === '0'", wrapper(field[[1]]))))
      expect_identical(aria(field[[1]]), c("group", field[[2]], "0"), info = field[[1]])
      expect_match(page$hover(sprintf("#%s", field[[1]])), "^Not available: ", info = field[[1]])
      page$evaluate(sprintf("document.getElementById('%s').disabled = false", field[[1]]))
      expect_true(page$wait_until(sprintf("%s.getAttribute('tabindex') === null", wrapper(field[[1]]))))
      expect_equal(aria(field[[1]]), rep(NA, 3L), info = field[[1]])
    }
  })

  it("does not cover a control that only has aria-disabled", {
    expect_equal(state("aria"), rep(NA, 3L))
    expect_equal(aria("aria"), rep(NA, 3L))
  })

  it("keeps the reason open on Enter and Space, which do nothing on the wrapper, and closes it on Escape", {
    view("built")
    page$evaluate(sprintf("%s.focus()", wrapper("built")))
    expect_true(page$wait_open())
    expect_identical(page$shown(), "Not available: Disabled at build")
    expect_identical(page$press("Enter"), "Not available: Disabled at build")
    expect_identical(page$press(" "), "Not available: Disabled at build")
    expect_length(page$press("Escape"), 0L)
    expect_identical(page$problems(), character())
  })

  it("shows the reason for tip_show() while the control is disabled", {
    view("show_built")
    press_button("show_built")
    expect_true(page$wait_until(sprintf(
      "[...document.querySelectorAll('.%s:not([hidden])')].some((t) => t.textContent === 'Not available: Disabled at build')",
      tip_names()$css
    )))
  })

  it("keeps the focus with a control that disables itself, and gives it back when the control is enabled", {
    view("self")
    page$evaluate("document.getElementById('self').focus()")
    page$evaluate("document.getElementById('self').click()")
    expect_true(page$wait_until(sprintf("%s.getAttribute('tabindex') === '0'", wrapper("self"))))
    expect_true(page$wait_until(sprintf("document.activeElement === %s", wrapper("self"))))
    page$evaluate("document.getElementById('self').disabled = false")
    expect_true(page$wait_until("document.activeElement === document.getElementById('self')"))
    expect_equal(aria("self"), rep(NA, 3L))
  })

  it("gives the reason the tip's look and placement, which the engine reads from the item upwards", {
    view("styled")
    page$hover("#styled")
    box <- jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const t = document.querySelector('.%s:not([hidden])'); const c = document.getElementById('styled').getBoundingClientRect(); const r = t.getBoundingClientRect(); return JSON.stringify({ text: t.textContent, right: r.left >= c.right - 1, radius: getComputedStyle(t).borderRadius }); })()",
      tip_names()$css
    )))
    expect_identical(box$text, "Not available: Styled reason")
    expect_true(box$right)
    expect_identical(box$radius, "13px")
  })

  it("keeps the width a control had in its column, a share of a row and a grid cell", {
    expect_equal(width_of("half"), 200)
    expect_equal(width_of("w50"), 200)
    expect_equal(page$evaluate("document.getElementById('half').parentElement.getBoundingClientRect().width"), 200)
    expect_equal(width_of("grid_btn"), page$evaluate("document.getElementById('grid').getBoundingClientRect().width"))
    expect_equal(width_of("f1"), width_of("f2"), tolerance = 0.01)
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

# The same on the page Shiny builds with no bslib: `fluidPage()` is Bootstrap 3, where a disabled `button` keeps its
# pointer events, so the engine could reach it on its own; the wrapper must still work, and give the keyboard a stop.
describe("tip_on(when_disabled = ) on a Bootstrap 3 page", {
  path <- find.package("hovercraft")
  port <- httpuv::randomPort()
  app <- callr::r_bg(
    function(port, path) {
      if (dir.exists(file.path(path, "inst"))) {
        pkgload::load_all(path, quiet = TRUE)
      } else {
        library(hovercraft)
      }
      ui <- shiny::fluidPage(
        shiny::actionButton("before", "Before"),
        tip_on(
          htmltools::tagAppendAttributes(shiny::actionButton("built", "Built"), disabled = "disabled"),
          "Enabled tip",
          when_disabled = "Disabled at build"
        ),
        tip_on(shiny::actionButton("later", "Later"), "Enabled tip", when_disabled = "Switched off"),
        shiny::actionButton("off", "Switch off"),
        shiny::actionButton("on", "Switch on")
      )
      server <- function(input, output, session) {
        shiny::observeEvent(input$off, shiny::updateActionButton(session, "later", disabled = TRUE))
        shiny::observeEvent(input$on, shiny::updateActionButton(session, "later", disabled = FALSE))
      }
      shiny::runApp(shiny::shinyApp(ui, server), port = port, host = "127.0.0.1", launch.browser = FALSE)
    },
    args = list(port = port, path = path),
    supervise = TRUE
  )
  withr::defer(app$kill())
  url <- paste0("http://127.0.0.1:", port)
  up <- wait_for_app(app, url)
  page <- local_page(url, environment())
  tabbed <- function(js) page$evaluate(js)

  it("starts, on Bootstrap 3, with no bslib", {
    expect_true(up, info = paste(app$read_error(), collapse = "\n"))
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    expect_false(page$evaluate("!!document.querySelector('.bslib-page-fill, [data-bs-theme]')"))
    expect_true(page$wait_until("document.getElementById('built').parentElement.getAttribute('role') === 'button'"))
  })

  it("shows the reason of a control disabled when the page was built, by the pointer and by the keyboard", {
    expect_identical(page$hover("#built"), "Not available: Disabled at build")
    expect_identical(tabbed("document.getElementById('before').focus()"), NULL)
    expect_identical(page$press("Tab"), "Not available: Disabled at build")
    expect_identical(page$evaluate("document.activeElement.firstElementChild.id"), "built")
  })

  it("shows the reason when Shiny switches the control off, and the control's own tip when it is on again", {
    page$evaluate("document.getElementById('off').click()")
    expect_true(page$wait_until("document.getElementById('later').parentElement.getAttribute('role') === 'button'"))
    expect_identical(page$hover("#later"), "Not available: Switched off")
    page$evaluate("document.getElementById('on').click()")
    expect_true(page$wait_until("document.getElementById('later').parentElement.getAttribute('role') === null"))
    expect_identical(page$hover("#later"), "Enabled tip")
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

# tip_update() on a control that has `when_disabled` (#85): while the control is off the page's tip item is its wrapper, but
# the new words, template, record and placement belong to the control's own tip. The reason stays alone while the control is
# off, the new values show once it is on, and the wrapper never holds a tip of its own.
describe("tip_update() on a control with when_disabled", {
  app <- start_app(function() {
    off <- function(x) htmltools::tagAppendAttributes(x, disabled = "disabled")
    row <- function(id, label, ...) {
      htmltools::tags$div(
        style = "margin: 160px 0 0 220px",
        tip_on(
          off(shiny::actionButton(id, label)),
          ...,
          when_disabled = paste("Reason", label),
          id = paste0(id, "_tip")
        )
      )
    }
    ui <- shiny::fluidPage(
      tip_define(card = tip_template(body = tip_text("Hello {{ name }}"))),
      row("tx", "Text", "Old text"),
      row("tp", "Template", template = "card", record = list(name = "Ada")),
      row("pl", "Place", "Place tip", placement = "top"),
      row("nt", "Plain", "Plain tip"),
      htmltools::tags$div(
        style = "margin: 160px 0 0 20px",
        shiny::actionButton("upd", "Update"),
        shiny::actionButton("en", "Enable"),
        shiny::actionButton("clear", "Clear")
      )
    )
    server <- function(input, output, session) {
      shiny::observeEvent(input$upd, {
        tip_update("tx_tip", text = "New **text**")
        tip_update("tp_tip", template = "card", record = list(name = "Grace"))
        tip_update("pl_tip", placement = "bottom")
      })
      shiny::observeEvent(
        input$en,
        for (id in c("tx", "tp", "pl", "nt")) {
          shiny::updateActionButton(session, id, disabled = FALSE)
        }
      )
      shiny::observeEvent(input$clear, tip_update("tx_tip", text = NULL))
    }
    shiny::shinyApp(ui, server)
  })
  page <- local_page(app$url, environment())
  tip_attrs <- c(tip_attribute("text"), tip_attribute("template"), tip_attribute("record"))
  wrapper <- function(id) sprintf("document.getElementById('%s').closest('.%s')", id, tip_disabled_class())
  press_button <- function(id) page$evaluate(sprintf("document.getElementById('%s').click()", id))
  # Whether the wrapper holds any attribute that would make it a tip of its own.
  ghost <- function(id) {
    page$evaluate(sprintf(
      "%s.hasAttribute('%s') || %s.hasAttribute('%s') || %s.hasAttribute('%s')",
      wrapper(id),
      tip_attrs[[1]],
      wrapper(id),
      tip_attrs[[2]],
      wrapper(id),
      tip_attrs[[3]]
    ))
  }
  # The tip's place against the control: the tip's top edge minus the control's bottom edge ("below" when positive), and
  # the control's top edge minus the tip's bottom edge ("above" when positive).
  gap <- function(id) {
    jsonlite::fromJSON(page$evaluate(sprintf(
      "(() => { const b = document.getElementById('%s').getBoundingClientRect(); const t = [...document.querySelectorAll('.%s')].find((e) => !e.hidden).getBoundingClientRect(); return JSON.stringify({ below: t.top - b.bottom, above: b.top - t.bottom }); })()",
      id,
      tip_names()$css
    )))
  }

  it("starts and the wrappers have no tip of their own", {
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === 'button'", wrapper("tx"))))
    for (id in c("tx", "tp", "pl", "nt")) {
      expect_false(ghost(id), info = id)
    }
  })

  it("shows the placement it was built with, above, while the control is off", {
    page$hover("#pl")
    expect_gt(gap("pl")$above, 0)
  })

  it("keeps the reason alone while the control is off, after an update of text, template, record and placement", {
    press_button("upd")
    expect_true(page$wait_until(sprintf(
      "document.getElementById('tx').getAttribute('%s') === 'New **text**'",
      tip_attrs[[1]]
    )))
    expect_true(page$wait_until(sprintf(
      "(document.getElementById('tp').getAttribute('%s') ?? '').includes('Grace')",
      tip_attrs[[3]]
    )))
    expect_true(page$wait_until(sprintf(
      "document.getElementById('pl').closest('[%s]').getAttribute('%s') === 'bottom'",
      tip_attribute("placement"),
      tip_attribute("placement")
    )))
    for (id in c("tx", "tp", "pl")) {
      expect_false(ghost(id), info = id)
    }
    expect_identical(page$hover("#tx"), "Not available: Reason Text")
    expect_identical(page$hover("#tp"), "Not available: Reason Template")
    expect_identical(page$hover("#pl"), "Not available: Reason Place")
  })

  it("puts the reason where the new placement says, below", {
    page$hover("#pl")
    expect_gt(gap("pl")$below, 0)
  })

  it("shows the updated tip once the control is on, and leaves nothing on the wrapper", {
    press_button("en")
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === null", wrapper("tx"))))
    expect_true(page$wait_until(sprintf("%s.getAttribute('role') === null", wrapper("pl"))))
    for (id in c("tx", "tp", "pl", "nt")) {
      expect_false(ghost(id), info = id)
    }
    expect_identical(page$hover("#tx"), "New text")
    expect_match(page$hover("#tp"), "Hello Grace")
    expect_identical(page$hover("#pl"), "Place tip")
    expect_gt(gap("pl")$below, 0)
  })

  it("takes the text away on a control that is on, and the control then has no tip of its own", {
    press_button("clear")
    expect_true(page$wait_until(sprintf("!document.getElementById('tx').hasAttribute('%s')", tip_attrs[[1]])))
    expect_length(page$hover("#tx"), 0L)
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

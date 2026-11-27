skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if_not_installed("bslib")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# One tip_output() shared by two items, with two pins open: the output lives in the newest pinned tip, and that tip must
# wait for the server (the loading mask) as an output drawn for another item does, not show what the first item's tip
# held. The output block finds the item it is drawn for from its own record (`tip_id`), not from "the first pinned tag in
# the page".
describe("one output shared by two pinned tips", {
  app <- start_app(function() {
    ui <- bslib::page_fluid(
      use_hovercraft(max_pins = 2),
      tip_define(shared = tip_template(tip_title("{{ label }}"), tip_output(shiny::textOutput("shared_out")))),
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
      )
    )
    server <- function(input, output, session) {
      # Answers only when the test says so: pinning the second item changes nothing this output reads.
      output$shared_out <- shiny::renderText(paste("Answer", input$gate %||% 0))
    }
    shiny::shinyApp(ui, server)
  })
  page <- local_page(app$url, environment())
  id_attribute <- tip_attribute("id")
  pin <- function(id) {
    page$evaluate(sprintf(
      paste0(
        "(() => { const n = document.querySelector('[%s=\"%s\"]'); ",
        "return window.Hovercraft.pin(n, { by: 'reader', via: 'key' }); })()"
      ),
      id_attribute,
      id
    ))
  }
  # The state of the output's slot inside the pinned tip that holds `label` in its title.
  slot_state <- function(label) {
    page$evaluate(sprintf(
      paste0(
        "[...document.querySelectorAll('.hc-tip-pin')].filter((t) => t.textContent.includes('%s'))",
        ".map((t) => t.querySelector('.hc-output')?.dataset.state ?? 'none')[0] ?? 'no tip'"
      ),
      label
    ))
  }

  it("starts and connects", {
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    expect_true(page$wait_until("document.getElementById('shared_out') !== null"))
  })

  it("shows the loading mask in the second pinned tip until the server answers for it", {
    pin("s1")
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin .hc-output[data-state=\"ready\"]') !== null"))
    expect_identical(slot_state("S1"), "ready")
    pin("s2")
    expect_true(page$wait_until("document.querySelectorAll('.hc-tip-pin').length === 2"))
    # The output moved to the second tip, which the server has not answered for.
    expect_true(
      page$wait_until(
        paste0(
          "[...document.querySelectorAll('.hc-tip-pin')].some((t) => t.textContent.includes('S2') && ",
          "t.querySelector('.hc-output')?.dataset.state === 'loading')"
        ),
        seconds = 1.2
      ),
      info = paste("state in the S2 tip:", slot_state("S2"))
    )
    # An answer from the server ends the wait.
    page$evaluate("Shiny.setInputValue('gate', 1, { priority: 'event' }); 0")
    expect_true(page$wait_until(
      "[...document.querySelectorAll('.hc-tip-pin')].some((t) => t.textContent.includes('S2') && t.querySelector('.hc-output')?.dataset.state === 'ready')"
    ))
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

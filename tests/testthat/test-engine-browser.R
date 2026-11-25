skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# The engine in a page that marks its own controls again on every change (engine 0.56.0, "E0"): such a page used to
# freeze once the engine's hidden descriptions were on it, because the engine made them again for a write that kept the
# value, which changed the panel the page watches, and the two never settled.
describe("the engine on a page that marks its controls again", {
  app <- start_app(function() {
    marking <- paste(
      "window.rounds = 0;",
      "const panel = document.getElementById('panel');",
      # The page's own observer stops itself at 300 rounds, so a loop fails the test and does not hang the browser.
      "const mark = () => {",
      "  if (window.rounds >= 300) return;",
      "  window.rounds += 1;",
      "  for (const b of panel.querySelectorAll('button')) b.setAttribute('aria-disabled', 'false');",
      "};",
      "new MutationObserver(mark).observe(panel, { childList: true, subtree: true });",
      "mark();",
      sep = "\n"
    )
    ui <- bslib::page_fluid(
      htmltools::div(
        id = "panel",
        tip_on(shiny::actionButton("one", "One"), "The **first** tip", id = "one_tip"),
        tip_on(shiny::actionButton("two", "Two"), "The second tip", id = "two_tip"),
        tip_on(shiny::actionButton("three", "Three"), "The third tip", id = "three_tip")
      ),
      htmltools::tags$script(htmltools::HTML(marking))
    )
    shiny::shinyApp(ui, function(input, output, session) NULL)
  })
  page <- local_page(app$url, environment())

  it("settles, and the page and the tips keep working", {
    expect_true(app$proc$is_alive(), info = paste(app$proc$read_error(), collapse = "\n"))
    expect_true(page$wait_until("document.querySelectorAll('[data-hovercraft-tip-id]').length === 3"))
    Sys.sleep(1)
    # A few rounds of the page's own observer, and then no more: it was at its cap of 300 when the engine fed it.
    expect_lt(page$evaluate("window.rounds"), 30)
    # The main thread answers, and a tip shows on the pointer.
    expect_identical(page$evaluate("1 + 1"), 2L)
    expect_identical(page$hover("#two"), "The second tip")
    expect_identical(page$problems(), character())
  })
})

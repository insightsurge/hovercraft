skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# labels = on use_hovercraft() and tip_on(): the words the tip shows itself change on the page, on one tip and on
# what is inside a tagged scope, and the nearest words win, key by key. The words of the engine ("Close", "More when
# pinned") and of hovercraft's own scripts ("Pin this tip", "More", "Less") are one set of keys.
describe("labels = changes the words a tip shows itself", {
  app <- start_app(function() {
    row <- function(id, label) {
      htmltools::tags$div(label, id = id, tabindex = "0", class = "p-2 border", style = "width: 18rem")
    }
    long <- tip_more("Long words.", on = "pin")
    ui <- bslib::page_fluid(
      use_hovercraft(
        labels = c(close = "Schliessen (page)", more_pinned = "Mehr, wenn angeheftet (page)", pin = "Anheften (page)")
      ),
      htmltools::tags$div(
        style = "padding: 30px 20px; display: flex; flex-direction: column; gap: 22px; align-items: flex-start",
        tip_on(row("r_page", "Page words"), "Short.", id = "t_page", pin = TRUE, more = long),
        tip_on(
          row("r_own", "Own words"),
          "Short.",
          id = "t_own",
          pin = TRUE,
          more = long,
          labels = c(close = "Zu (own)", more_pinned = "Mehr (own)", pin = "Fix (own)")
        ),
        tip_on(
          htmltools::tags$div(
            id = "scope",
            tabindex = "0",
            class = "p-2 border",
            style = "width: 20rem",
            htmltools::tags$div("Scope"),
            htmltools::tags$div(
              style = "display: flex; flex-direction: column; gap: 14px",
              tip_on(row("r_in", "Inside the scope"), "Short.", id = "t_in", pin = TRUE, more = long),
              tip_on(
                row("r_deep", "Deeper scope"),
                "Short.",
                id = "t_deep",
                pin = TRUE,
                more = long,
                labels = c(close = "Tief zu (deep)")
              )
            )
          ),
          "The scope's own tip.",
          labels = c(close = "Aussen zu (scope)", pin = "Aussen fix (scope)")
        ),
        tip_on(
          row("r_btn", "Buttons"),
          "Short.",
          more = "Long words.",
          labels = c(more = "Mehr (tag)", less = "Weniger (tag)")
        ),
        tip_on(
          row("r_btn_own", "Buttons, own"),
          "Short.",
          more = tip_more("Long words.", label = c(more = "Show (more)")),
          labels = c(more = "Mehr (tag)", less = "Weniger (tag)")
        )
      )
    )
    shiny::shinyApp(ui, function(input, output, session) NULL)
  })
  page <- local_page(app$url, environment())
  css <- tip_names()$css
  hint_box <- sprintf(".%s:not(.%s-pin):not([hidden])", css, css)
  pinned_box <- sprintf(".%s.%s-pin:not([hidden])", css, css)
  # What the reader meets on an item: the name of the pin button and the cue under the short tip, with the keyboard on
  # the item, then the name of the close button of the pinned tip.
  met <- function(id) {
    page$evaluate("document.activeElement?.blur()")
    page$evaluate(sprintf("document.getElementById('%s').focus(); 0", id))
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", hint_box)))
    expect_true(page$wait_until("document.querySelector('.hovercraft-pin-strip-pin') !== null"))
    page$settle()
    hint <- jsonlite::fromJSON(page$evaluate(sprintf(
      "JSON.stringify({ pin: document.querySelector('.hovercraft-pin-strip-pin').getAttribute('aria-label'), cue: document.querySelector('%s .%s-cue')?.textContent ?? '' })",
      hint_box,
      css
    )))
    page$press("p")
    expect_true(page$wait_until(sprintf("document.querySelector('%s') !== null", pinned_box)))
    close <- page$evaluate(sprintf(
      "document.querySelector('%s .%s-close').getAttribute('aria-label')",
      pinned_box,
      css
    ))
    page$press("Escape", wait = 0.3)
    expect_true(page$wait_until(sprintf("document.querySelector('%s') === null", pinned_box)))
    page$evaluate("document.activeElement?.blur()")
    page$settle()
    list(pin = hint$pin, cue = hint$cue, close = close)
  }
  button <- function(id) {
    page$evaluate("document.activeElement?.blur()")
    page$evaluate(sprintf("document.getElementById('%s').focus(); 0", id))
    expect_true(page$wait_until("document.querySelector('.hovercraft-more-strip-button') !== null"))
    page$settle()
    before <- page$evaluate("document.querySelector('.hovercraft-more-strip-button').textContent")
    page$press("m")
    expect_true(page$wait_until(
      "document.querySelector('.hovercraft-more-strip-button')?.getAttribute('aria-expanded') === 'true'"
    ))
    after <- page$evaluate("document.querySelector('.hovercraft-more-strip-button').textContent")
    page$press("Escape", wait = 0.3)
    page$evaluate("document.activeElement?.blur()")
    expect_true(page$wait_closed())
    list(before = before, after = after)
  }

  it("starts and connects", {
    expect_true(app$proc$is_alive(), info = paste(app$proc$read_error(), collapse = "\n"))
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
  })

  it("shows the page's words for More when pinned, Pin this tip and Close", {
    got <- met("r_page")
    expect_identical(got$pin, "Anheften (page)")
    expect_identical(got$cue, "Mehr, wenn angeheftet (page)")
    expect_identical(got$close, "Schliessen (page)")
  })

  it("shows a tip's own words over the page's", {
    got <- met("r_own")
    expect_identical(got$pin, "Fix (own)")
    expect_identical(got$cue, "Mehr (own)")
    expect_identical(got$close, "Zu (own)")
  })

  it("lets a tip inside a scope take the scope's words, key by key, over the page's", {
    got <- met("r_in")
    expect_identical(got$pin, "Aussen fix (scope)")
    expect_identical(got$close, "Aussen zu (scope)")
    # A key the scope leaves out comes from the page.
    expect_identical(got$cue, "Mehr, wenn angeheftet (page)")
  })

  it("lets the nearer scope win, and the farther one fill the rest", {
    got <- met("r_deep")
    expect_identical(got$close, "Tief zu (deep)")
    expect_identical(got$pin, "Aussen fix (scope)")
    expect_identical(got$cue, "Mehr, wenn angeheftet (page)")
  })

  it("changes the words of the More and Less buttons, and tip_more(label =) beats labels =", {
    words <- button("r_btn")
    expect_identical(words$before, "Mehr (tag)")
    expect_identical(words$after, "Weniger (tag)")
    words <- button("r_btn_own")
    expect_identical(words$before, "Show (more)")
    expect_identical(words$after, "Weniger (tag)")
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

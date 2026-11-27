skip_if_no_browser("DT")

# hovercraft book in Chrome, one session from start to end: real pointer and keys, the server's answers, the ticks,
# the look of the page, Copy right after a change, the search, the checklist, and a phone's layout.
describe("hovercraft book in a browser", {
  app <- start_app(function() hovercraft_book(run = FALSE))
  page <- local_page(app$url)
  page$resize(1440, 900)
  js <- function(code) page$evaluate(code)
  tip <- function(id) sprintf("[%s-id=\"%s\"]", tip_names()$attribute, id)
  ticked <- function(plate, i) {
    sprintf("!!document.querySelector('[data-steps=\"%s\"] > li[data-i=\"%d\"].ck-done')", plate, i)
  }
  logged <- function(plate, words) {
    sprintf(
      "[...document.querySelectorAll('[data-log=\"%s\"] li')].some((li) => li.textContent.includes(%s))",
      plate,
      jsonlite::toJSON(words, auto_unbox = TRUE)
    )
  }
  scroll_to <- function(id) js(sprintf("document.getElementById('%s').scrollIntoView({ behavior: 'instant' })", id))
  focus <- function(selector) {
    js(sprintf("document.querySelector(%s).focus()", jsonlite::toJSON(selector, auto_unbox = TRUE)))
  }

  it("starts with every section's code from the server and nothing in the console", {
    expect_true(page$wait_until(
      "!!window.Shiny?.shinyapp?.isConnected() && [...document.querySelectorAll('[data-code]')].every((p) => p.dataset.raw)",
      30
    ))
    expect_identical(js("document.querySelectorAll('.plate').length"), length(book_section_ids()))
    # The page's colours reach the server with no click: the dark-mode steps can tick on a dark system.
    expect_true(page$wait_until("['light', 'dark'].includes(window.Shiny.shinyapp.$inputValues.page_mode)"))
    expect_identical(page$problems(), character())
  })

  it("folds long code to its first lines, and opens it in place", {
    more <- "document.querySelector('#start .js-code-more')"
    folded <- "document.getElementById('start-code').classList.contains('is-folded')"
    expect_match(js(paste0(more, ".textContent")), "^Show all [0-9]+ lines$")
    expect_true(js(folded))
    # Short code shows whole, with no button.
    expect_true(js("document.querySelector('#size .js-code-more').hidden"))
    js(paste0(more, ".click()"))
    expect_false(js(folded))
    expect_identical(js(paste0(more, ".getAttribute('aria-expanded')")), "true")
    js(paste0(more, ".click()"))
    expect_true(js(folded))
  })

  it("logs a real hint and ticks the step it proves", {
    # The figures are drawn by the server a moment after the page.
    expect_true(page$wait_until(
      sprintf("!!document.querySelector(%s)", jsonlite::toJSON(tip("visits"), auto_unbox = TRUE)),
      20
    ))
    expect_length(page$hover(tip("visits")), 1L)
    expect_true(page$wait_until(logged("start", "input$visits_hint")))
    expect_true(page$wait_until(ticked("start", 2L)))
    expect_match(js("document.querySelector('#start .ck-said').textContent"), "ticked itself")
  })

  it("opens the title's tip in its own look, and logs it", {
    js("window.scrollTo({ top: 0, behavior: 'instant' })")
    expect_length(page$hover(tip("hero")), 1L)
    expect_true(page$wait_until(logged("start", "input$hero_hint")))
    expect_true(page$wait_until(ticked("start", 0L)))
    page$mouse(5, 5)
  })

  it("finds sections by the functions and arguments they show, and Escape clears the search", {
    visible <- "[...document.querySelectorAll('#toc a[data-plate]')].filter((a) => a.offsetParent).map((a) => a.dataset.plate)"
    focus("#toc-q")
    page$type("keep_alive")
    # A search opens every part with a match, not only the part being read.
    expect_true(page$wait_until(sprintf("JSON.stringify(%s) === '[\"output\"]'", visible)))
    page$type(" zzz")
    expect_true(page$wait_until("document.querySelector('.index .toc-none').textContent === 'Nothing matches.'"))
    page$press("Escape", wait = 0)
    # Every section is back in the index, and only the part being read is open again.
    expect_true(page$wait_until("[...document.querySelectorAll('#toc li')].every((li) => !li.hidden)"))
    expect_false(js("document.documentElement.classList.contains('toc-searching')"))
    expect_identical(js("document.querySelector('.index .toc-none').textContent"), "")
    js("document.activeElement.blur()")
  })

  it("changes the look of every tip on the page, from the server, without a reload", {
    js("window.bookLoaded = true; document.querySelector('#looks_preset input[value=editorial]').click()")
    attribute <- paste0(tip_names()$attribute, "-preset")
    expect_true(page$wait_until(sprintf("document.documentElement.getAttribute('%s') === 'editorial'", attribute)))
    expect_true(js("window.bookLoaded === true"))
    expect_match(
      js("document.querySelector('[data-code=\"looks\"]').dataset.raw"),
      "tip_style(\"editorial\"",
      fixed = TRUE
    )
  })

  it("copies the code for a choice made just before Copy", {
    js(paste(
      "window.copied = null;",
      "navigator.clipboard.writeText = async (text) => { window.copied = text; };",
      "document.querySelector('#placement_side input[value=top]').click();",
      "document.querySelector('#placement .js-copy').click();"
    ))
    expect_true(page$wait_until("typeof window.copied === 'string'"))
    expect_match(js("window.copied"), "placement = \"top\"", fixed = TRUE)
    # The page colours in the looks sentence are not a choice of its code: Copy right after one still copies.
    js(paste(
      "window.copied = null;",
      "document.querySelector('#looks .theme-set input[value=dark]').click();",
      "document.querySelector('#looks .js-copy').click();"
    ))
    expect_true(page$wait_until("typeof window.copied === 'string'"))
    expect_match(js("window.copied"), "use_hovercraft(", fixed = TRUE)
    expect_identical(js("document.documentElement.getAttribute('data-bs-theme')"), "dark")
    expect_true(page$wait_until("window.Shiny.shinyapp.$inputValues.page_mode === 'dark'"))
    js("document.querySelector('#looks .theme-set input[value=light]').click()")
  })

  it("draws a plot in a pinned tip, keeps it when the tip closes, and says so", {
    scroll_to("output")
    focus(tip("region_north"))
    page$press("p", wait = 0)
    expect_true(page$wait_until(logged("output", "drawn for North")))
    expect_true(page$wait_until(ticked("output", 0L)))
    page$press("Escape", wait = 0)
    expect_true(page$wait_unpinned())
    focus(tip("region_north"))
    page$press("p", wait = 0)
    expect_true(page$wait_until(logged("output", "kept for North")))
    expect_true(page$wait_until(ticked("output", 1L)))
    drawn <- "[...document.querySelectorAll('[data-log=\"output\"] li')].filter((li) => li.textContent.includes('drawn for North')).length"
    expect_identical(js(drawn), 1L)
    page$press("Escape", wait = 0)
    expect_true(page$wait_unpinned())
  })

  it("says drawn, not kept, when the plot was drawn for another region in between", {
    scroll_to("output")
    # Every log line the server sends from now on (the page keeps only the newest 14, so they are counted here).
    js(paste(
      "window.bookSeen = [];",
      "window.jQuery(document).on('shiny:message', (e) => {",
      "  const m = e.message?.custom?.['book-log'];",
      "  if (m) window.bookSeen.push(m.value);",
      "})"
    ))
    count <- function(words) sprintf("window.bookSeen.filter((v) => v.includes('%s')).length", words)
    # Coast first, so North's plot is drawn anew when North is pinned.
    focus(tip("region_coast"))
    page$press("p", wait = 0)
    expect_true(page$wait_until(sprintf("%s > 0", count("drawn for Coast"))))
    page$press("Escape", wait = 0)
    expect_true(page$wait_unpinned())
    drawn_before <- js(count("drawn for North"))
    kept_before <- js(count("kept for North"))
    focus(tip("region_north"))
    page$press("p", wait = 0)
    expect_true(page$wait_until(sprintf("%s > %d", count("drawn for North"), drawn_before)))
    page$press("Escape", wait = 0)
    expect_true(page$wait_unpinned())
    coast_before <- js(count("drawn for Coast"))
    # Pinning Coast draws the shared plot for Coast; pinning North again must draw it again, not keep it.
    focus(tip("region_coast"))
    page$press("p", wait = 0)
    expect_true(page$wait_until(sprintf("%s > %d", count("drawn for Coast"), coast_before)))
    page$press("Escape", wait = 0)
    expect_true(page$wait_unpinned())
    focus(tip("region_north"))
    page$press("p", wait = 0)
    expect_true(page$wait_until(sprintf("%s > %d", count("drawn for North"), drawn_before + 1L)))
    page$hold(2) # North must not be drawn again: nothing happening has no signal
    expect_identical(js(count("kept for North")), kept_before)
    page$press("Escape", wait = 0)
    expect_true(page$wait_unpinned())
    # Resting on Coast in between may or may not draw the plot for Coast, as Shiny decides; either way the log tells
    # the truth about North when it is pinned again: drawn, or kept, never both and never neither.
    drawn_now <- js(count("drawn for North"))
    kept_now <- js(count("kept for North"))
    page$hover(tip("region_coast"))
    page$hold(1) # Shiny may or may not draw the plot for Coast: either way, let it decide before moving on
    page$mouse(5, 5)
    focus(tip("region_north"))
    page$press("p", wait = 0)
    expect_true(page$wait_until(sprintf(
      "%s > %d || %s > %d",
      count("drawn for North"),
      drawn_now,
      count("kept for North"),
      kept_now
    )))
    # Let the other counter move too, were it going to: "drawn and kept" must be caught, not only the first signal. No
    # signal says that nothing else comes, so this is a wait for time to pass, not for an event.
    page$hold(2)
    drew <- js(count("drawn for North")) > drawn_now
    kept <- js(count("kept for North")) > kept_now
    expect_true(xor(drew, kept))
    page$press("Escape", wait = 0)
    expect_true(page$wait_unpinned())
  })

  it("loads every section in light and dark, at 1440 and 375 px, with nothing in the console", {
    figure <- function(id) {
      sprintf(
        "(() => { const f = document.getElementById('%s_figure'); return !!f && f.innerHTML.includes('%s'); })()",
        id,
        tip_names()$attribute
      )
    }
    # The page's colours on Auto, so they follow the system's.
    js("document.querySelector('.index .theme-set input[value=auto]').click()")
    for (width in c(1440L, 375L)) {
      page$resize(width, if (width > 1000L) 900L else 812L)
      for (scheme in c("light", "dark")) {
        page$emulate("prefers-color-scheme", scheme)
        expect_true(page$wait_until(sprintf("document.documentElement.getAttribute('data-bs-theme') === '%s'", scheme)))
        for (id in book_section_ids()) {
          scroll_to(id)
          expect_true(page$wait_until(figure(id)), info = paste(id, width, scheme))
          expect_lte(js("document.documentElement.scrollWidth"), width)
        }
      }
    }
    page$emulate("prefers-color-scheme", "light")
    page$resize(1440, 900)
    expect_identical(page$problems(), character())
  })

  it("does not tick a step when the page pins a tip again", {
    scroll_to("pins")
    focus(tip("col_wait_min"))
    page$press("p", wait = 0)
    expect_true(page$wait_until(ticked("pins", 1L)))
    # A new look closes the pinned tip and pins it again, both by the page: no close step ticks.
    js("document.querySelector('#looks_preset input[value=dense]').click()")
    expect_true(page$wait_until(logged("pins", "\"host\"")))
    expect_false(js(ticked("pins", 4L)))
    expect_false(js(ticked("pins", 2L)))
    page$press("Escape", wait = 0)
    expect_true(page$wait_until(ticked("pins", 4L)))
  })

  it("opens a tip from the server, and the hint input says the host did it", {
    scroll_to("server")
    # A hint the pointer opens and closes is not the step "stays open until Hide, Escape or another tip".
    page$hover(tip("onboarding"))
    page$mouse(5, 5)
    expect_true(page$wait_until(logged("server", "\"leave\"")))
    expect_false(js(ticked("server", 1L)))
    js("document.getElementById('srv_show').click()")
    expect_true(page$wait_until(logged("server", "by = \"host\"")))
    expect_true(page$wait_until(ticked("server", 0L)))
    js("document.getElementById('srv_hide').click()")
    expect_true(page$wait_until(ticked("server", 1L)))
  })

  it("shows the checklist one step at a time, with keys", {
    scroll_to("pins")
    # The letter keys act while the focus is on the page, not on a control or a tip.
    js("document.activeElement.blur()")
    page$press("c", wait = 0)
    expect_true(page$wait_until("document.documentElement.classList.contains('checklist-on')"))
    expect_false(js("document.getElementById('ck-leader').hasAttribute('hidden')"))
    expect_true(js("!!document.querySelector('#pins .ck-target')"))
    before <- js("document.querySelector('#pins .tests > li.ck-current').dataset.i")
    page$press("n", wait = 0)
    expect_true(page$wait_until(sprintf(
      "document.querySelector('#pins .tests > li.ck-current').dataset.i !== %s",
      jsonlite::toJSON(before, auto_unbox = TRUE)
    )))
    page$press("c", wait = 0)
    expect_true(page$wait_until("!document.documentElement.classList.contains('checklist-on')"))
  })

  it("opens only the part being read in the index, and a click on a part's title goes to its first section", {
    open_parts <- "[...document.querySelectorAll('#toc .part.is-open .part-t .pn')].map((n) => n.textContent).join()"
    scroll_to("pins")
    expect_true(page$wait_until(sprintf("%s === '3'", open_parts)))
    expect_false(js("!!document.querySelector('#toc a[data-plate=\"start\"]').offsetParent"))
    js("document.querySelector('#toc .part:nth-child(6) .part-t').click()")
    expect_true(page$wait_until(sprintf("%s === '6'", open_parts), 10))
    expect_true(page$wait_until("Math.abs(document.getElementById('tables').getBoundingClientRect().top) < 60", 10))
  })

  it("fits a phone: no sideways scroll, the margin opens as a drawer and Escape closes it", {
    page$resize(375, 812)
    expect_lte(js("document.documentElement.scrollWidth"), 375L)
    js("document.getElementById('pull').click()")
    expect_true(js("document.documentElement.classList.contains('drawer-open')"))
    page$press("Escape", wait = 0)
    expect_true(page$wait_until("!document.documentElement.classList.contains('drawer-open')"))
    expect_identical(js("document.activeElement.id"), "pull")
    # Focus that leaves the drawer closes it too.
    js("document.getElementById('pull').click()")
    expect_true(js("document.documentElement.classList.contains('drawer-open')"))
    js("document.querySelector('.plate.is-current .specimen [tabindex]').focus()")
    expect_false(js("document.documentElement.classList.contains('drawer-open')"))
    page$resize(1440, 900)
    expect_identical(page$problems(), character())
  })

  it("opens at the section its address names, once the server has drawn the figures above it", {
    js("location.search = '?section=table_pins'")
    expect_true(page$wait_until("location.search === '?section=table_pins'", 10))
    expect_true(page$wait_until("!!window.Shiny?.shinyapp?.isConnected()", 30))
    Sys.sleep(3)
    expect_lt(abs(js("document.getElementById('table_pins').getBoundingClientRect().top")), 60)
    expect_identical(js("document.querySelector('#toc a[aria-current=\"true\"]').dataset.plate"), "table_pins")
  })
})

# The five sections added for the 0.1.0 API, in a book of their own: the last test reloads the page with a page-wide
# setting, which the tests above must not see.
describe("hovercraft book's newer sections in a browser", {
  app <- start_app(function() hovercraft_book(run = FALSE))
  page <- local_page(app$url)
  page$resize(1440, 900)
  js <- function(code) page$evaluate(code)
  json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE)
  tip <- function(id) sprintf("[%s-id=\"%s\"]", tip_names()$attribute, id)
  shown <- sprintf("document.querySelector('.%s:not([hidden])')", tip_names()$css)
  # The More control is drawn beside the tip's box, not in it: its words are the button's text.
  control <- "document.querySelector('.hovercraft-more-strip:not([hidden]) .hovercraft-more-strip-button')"
  says <- function(words) sprintf("(%s?.textContent ?? '').includes(%s)", control, json(words))
  ticked <- function(plate, i) {
    sprintf("!!document.querySelector('[data-steps=\"%s\"] > li[data-i=\"%d\"].ck-done')", plate, i)
  }
  logged <- function(plate, words) {
    sprintf(
      "[...document.querySelectorAll('[data-log=\"%s\"] li')].some((li) => li.textContent.includes(%s))",
      plate,
      json(words)
    )
  }
  scroll_to <- function(id) js(sprintf("document.getElementById('%s').scrollIntoView({ behavior: 'instant' })", id))
  focus <- function(selector) js(sprintf("document.querySelector(%s).focus()", json(selector)))
  value <- function(name) sprintf("window.Shiny.shinyapp.$inputValues[%s]", json(name))
  ready <- paste(
    "!!window.Shiny?.shinyapp?.isConnected() &&",
    "[...document.querySelectorAll('[data-code]')].every((p) => p.dataset.raw)"
  )
  expect_true(page$wait_until(ready, 30))

  it("opens a table row's tip from the server, and closes it again (Table tips from the server)", {
    scroll_to("table_server")
    expect_true(page$wait_until("!!document.querySelector('#table_server tbody tr')", 20))
    page$click_now("#tsrv_show")
    expect_true(page$wait_until(logged("table_server", "input$branch_tips_hint")))
    expect_true(page$wait_until(ticked("table_server", 0L)))
    expect_true(page$wait_open())
    page$click_now("#tsrv_hide")
    expect_true(page$wait_until(ticked("table_server", 1L)))
    expect_true(page$wait_closed())
  })

  it("draws a row's pinned tip as a card under the row, which a click away leaves open (A pinned tip ...)", {
    scroll_to("in_place")
    row <- "#in_place tbody tr"
    expect_true(page$wait_until(sprintf("!!document.querySelector(%s)", json(row)), 20))
    focus(row)
    page$press("p", wait = 0)
    card <- sprintf(
      "document.querySelector(%s)?.nextElementSibling?.hasAttribute('data-hovercraft-panel') === true",
      json(row)
    )
    expect_true(page$wait_until(card))
    expect_true(page$wait_until(logged("in_place", "input$queue_cards_pin")))
    expect_true(page$wait_until(ticked("in_place", 0L)))
    page$mouse(5, 5)
    page$click(".colophon")
    # The engine's close delay: a card that a click away closed would be gone by now.
    page$hold(0.4)
    expect_true(js(card))
    page$press("Escape", wait = 0)
    expect_true(page$wait_until("document.querySelectorAll('[data-hovercraft-panel]').length === 0"))
  })

  it("grows a tip in place with More, says Less, and takes the words chosen (The More button)", {
    scroll_to("more_button")
    focus(tip("more_button_open"))
    expect_true(page$wait_open())
    expect_true(page$wait_until(says("More")))
    page$press("m", wait = 0)
    expect_true(page$wait_until(sprintf("%s?.textContent.includes('How it is counted')", shown)))
    expect_true(page$wait_until(says("Less")))
    page$press("Escape", wait = 0)
    js("document.activeElement.blur()")
    expect_true(page$wait_closed())
    js("document.querySelector('#more_button_label input:not(:checked)').click()")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('[data-code=\"more_button\"]').dataset.raw.includes(%s)",
      json("label =")
    )))
    focus(tip("more_button_open"))
    expect_true(page$wait_until(says("Show all")))
    page$press("Escape", wait = 0)
    js("document.activeElement.blur()")
    expect_true(page$wait_closed())
  })

  it("gives one tag's tip its own words, and the tag next to it keeps the page's (The tip's built-in words)", {
    scroll_to("words")
    js("document.querySelector('#words_words input[value=english]').click()")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('[data-code=\"words\"]').dataset.raw.includes(%s)",
      json("Dismiss")
    )))
    expect_true(page$wait_until(sprintf("!!document.querySelector(%s)", json(tip("words_wait_min")))))
    focus(tip("words_wait_min"))
    page$press("p", wait = 0)
    expect_true(page$wait_until(logged("words", "input$words_wait_min_pin")))
    # The close button says its word to a screen reader, as its name.
    expect_true(page$wait_until(sprintf(
      "[...%s?.querySelectorAll('button') ?? []].some((b) => b.getAttribute('aria-label') === 'Dismiss')",
      shown
    )))
    page$press("Escape", wait = 0)
    expect_true(page$wait_unpinned())
    js("document.activeElement.blur()")
    focus(tip("words_region"))
    expect_true(page$wait_open())
    expect_true(page$wait_until(says("More")))
    js("document.activeElement.blur()")
  })

  it("reloads the book with max_pins = 3, at the same section, and keeps two pins open (Several pinned tips)", {
    scroll_to("pin_limits")
    js("document.querySelector('#pin_limits_pins input[value=\"3\"]').click()")
    expect_true(page$wait_until(
      "location.search.includes('max_pins=3') && location.search.includes('section=pin_limits')"
    ))
    expect_true(page$wait_until(ready, 30))
    expect_true(js("document.querySelector('#pin_limits_pins input[value=\"3\"]').checked"))
    for (id in c("pin_limits_visit_date", "pin_limits_region")) {
      expect_true(page$wait_until(sprintf("!!document.querySelector(%s)", json(tip(id))), 20))
      focus(tip(id))
      page$press("p", wait = 0)
      expect_true(page$wait_until(sprintf("%s === true", value(paste0(id, "_pinned")))))
    }
    # With room for three, pinning the second leaves the first pinned.
    expect_true(js(sprintf("%s === true", value("pin_limits_visit_date_pinned"))))
    expect_identical(page$problems(), character())
  })
})

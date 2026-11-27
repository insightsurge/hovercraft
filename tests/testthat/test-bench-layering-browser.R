skip_if_no_browser(bench_packages)
skip_if_no_bench()

# The bench's layering scenarios that had no test (#158): a tip in a native dialog and in an offcanvas (HC-004), and a
# Shiny modal opening over pinned tips (HC-052, whose spot is on the host tab; the layering tab's modal has the
# pinnable item inside it that the scenario needs).
describe("the bench's layering scenarios", {
  bench <- start_bench(environment())

  it("draws a hint and a pin in a native dialog and in an offcanvas above them (HC-004)", {
    page <- bench_page(bench, "layering")
    bench_scroll_to(page, "#spot-HC-004")
    page$click_now("#lay-dialog-open")
    expect_true(page$wait_until("document.getElementById('lay-dialog').open === true"))
    expect_identical(page$hover("#lay-dialog-btn"), "A tip in the Top Layer")
    tips <- bench_tips(page)
    expect_true(tips$drawn && tips$on_top)
    page$press("p", wait = 0)
    expect_true(bench_wait_tips(page, "A tip in the Top Layer"))
    tips <- bench_tips(page)
    expect_true(tips$pinned && tips$drawn && tips$on_top)
    # The pin lives in the dialog, so it is in the Top Layer with it and goes when the dialog closes.
    expect_true(page$evaluate(sprintf(
      "document.querySelector('#lay-dialog .%s-pin:not([hidden])') !== null",
      tip_names()$css
    )))
    since <- page$mark()
    page$evaluate("document.getElementById('lay-dialog').close(); 0")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s-pin:not([hidden])') === null",
      tip_names()$css
    )))
    closed <- grep("^closed lay_dialog_btn_tip", bench_tip_events(page, since), value = TRUE)
    expect_length(closed, 1L)
    expect_false(grepl("escape", closed, fixed = TRUE))

    page$click_now("#lay-offcanvas-open")
    expect_true(page$wait_until("document.getElementById('lay-offcanvas').classList.contains('show')"))
    page$settle()
    expect_identical(page$hover("#lay-offcanvas-btn"), "Above it")
    tips <- bench_tips(page)
    expect_true(tips$drawn && tips$on_top)
    page$press("p", wait = 0)
    expect_true(bench_wait_tips(page, "Above it"))
    tips <- bench_tips(page)
    expect_true(tips$pinned && tips$drawn && tips$on_top)
    expect_true(bench_close_tips(page))
    page$evaluate("bootstrap.Offcanvas.getInstance(document.getElementById('lay-offcanvas')).hide(); 0")
    expect_true(page$wait_until("document.querySelector('.offcanvas.show, .offcanvas-backdrop') === null"))
    expect_bench_clean(page)
  })

  it("keeps the page's pins through a modal and closes the modal's own pin with it (HC-052)", {
    page <- bench_page(bench, "layering", list(max_pins = 2))
    bench_scroll_to(page, "#spot-HC-001")
    page$hover("#lay-a")
    page$press("p", wait = 0)
    expect_true(bench_wait_tips(page, "A's tip"))
    since <- page$mark()

    # The server opens the modal (as an observer or a timer would): a reader's click on the page's button would be a
    # click away, which closes a pin by design (reason "outside").
    page$evaluate("Shiny.setInputValue('lay_modal', 1, { priority: 'event' }); 0")
    expect_true(page$wait_until("document.querySelector('.modal.show #lay-modal-btn') !== null"))
    page$settle()
    # The page's pin stays open, but under the backdrop (#165): nothing of it is on top at its own centre, so a click
    # there reaches the backdrop and not the pin's buttons.
    expect_identical(bench_tips(page)$text, "A's tip")
    expect_false(bench_tips(page)$on_top)
    expect_true(page$evaluate(
      "(() => { const t = document.querySelector('.hc-tip-pin:not([hidden])'); const r = t.getBoundingClientRect();
        return t.contains(document.elementFromPoint(r.right - 8, r.top + 8)) === false; })()"
    ))

    expect_true("Above the backdrop" %in% page$hover("#lay-modal-btn"))
    page$press("p", wait = 0)
    expect_true(bench_wait_tips(page, c("A's tip", "Above the backdrop")))
    tips <- bench_tips(page)
    modal_tip <- tips[tips$text == "Above the backdrop", ]
    expect_true(modal_tip$pinned && modal_tip$on_top)
    expect_false(tips$on_top[tips$text == "A's tip"])

    page$evaluate("bootstrap.Modal.getInstance(document.getElementById('shiny-modal')).hide(); 0")
    expect_true(page$wait_until("document.getElementById('shiny-modal') === null"))
    expect_true(bench_wait_tips(page, "A's tip"))
    page$settle()
    tips <- bench_tips(page)
    expect_true(tips$pinned && tips$on_top)
    expect_false(any(grepl("lay_a_tip", bench_tip_events(page, since), fixed = TRUE)))
    expect_bench_clean(page)
  })
})

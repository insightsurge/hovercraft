skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if_not_installed("DT")
skip_if_not_installed("reactable")
skip_if_not_installed("shinyjs")
skip_if_not_installed("promises")
skip_if_not_installed("later")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")
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
    page$press("p")
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
    page$press("p")
    expect_true(bench_wait_tips(page, "Above it"))
    tips <- bench_tips(page)
    expect_true(tips$pinned && tips$drawn && tips$on_top)
    expect_true(bench_close_tips(page))
    page$evaluate("bootstrap.Offcanvas.getInstance(document.getElementById('lay-offcanvas')).hide(); 0")
    expect_true(page$wait_until("document.querySelector('.offcanvas.show, .offcanvas-backdrop') === null"))
    expect_bench_clean(page)
  })

  it("keeps the page's pins through a modal and closes the modal's own pin with it (HC-052, partly)", {
    page <- bench_page(bench, "layering", list(max_pins = 2))
    bench_scroll_to(page, "#spot-HC-001")
    page$hover("#lay-a")
    page$press("p")
    expect_true(bench_wait_tips(page, "A's tip"))
    since <- page$mark()

    # The server opens the modal (as an observer or a timer would): a reader's click on the page's button would be a
    # click away, which closes a pin by design (reason "outside").
    page$evaluate("Shiny.setInputValue('lay_modal', 1, { priority: 'event' }); 0")
    expect_true(page$wait_until("document.querySelector('.modal.show #lay-modal-btn') !== null"))
    page$settle()
    # The page's pin stays open. The scenario also wants it under the backdrop; today it draws above the modal and
    # stays clickable (the tip layer's z-index is above Bootstrap's modal), a finding reported in #158, so that step is
    # left out here.
    expect_identical(bench_tips(page)$text, "A's tip")

    expect_true("Above the backdrop" %in% page$hover("#lay-modal-btn"))
    page$press("p")
    expect_true(bench_wait_tips(page, c("A's tip", "Above the backdrop")))
    tips <- bench_tips(page)
    modal_tip <- tips[tips$text == "Above the backdrop", ]
    expect_true(modal_tip$pinned && modal_tip$on_top)

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

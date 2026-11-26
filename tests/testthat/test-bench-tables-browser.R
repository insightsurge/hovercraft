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

# The bench's table scenarios that had no test (#158): the pointer coming back to a pinned row (HC-020), a pinned row
# in a tab that hides and in a modal that closes (HC-047), and a hint on a row while the table is searched and scrolled
# (HC-048).
describe("the bench's table scenarios", {
  bench <- start_bench(environment())

  css <- tip_names()$css
  tip_nodes <- function(page) page$evaluate(sprintf("document.querySelectorAll('.%s').length", css))
  none_open <- sprintf("document.querySelector('.%s:not([hidden])') === null", css)
  # No pin on show (a hint may open where the resting pointer lands once the layout changes; that is not this step's).
  no_pin <- sprintf("document.querySelector('.%s-pin:not([hidden])') === null", css)
  widths <- function(page, id) {
    page$evaluate(sprintf(
      "JSON.stringify([...document.querySelectorAll('#%s thead th')].map((t) => Math.round(t.getBoundingClientRect().width)))",
      id
    ))
  }

  it("keeps one tip for a pinned row when the pointer comes back and rests, and P leaves it pinned (HC-020)", {
    page <- bench_page(bench, "tables")
    expect_true(page$wait_until("document.querySelector('#tbl_client tbody tr') !== null"))
    page$settle()
    cell <- "#tbl_client tbody tr:nth-child(2) td:nth-child(1)"
    bench_scroll_to(page, "#tbl_client tbody tr:nth-child(2)")
    page$hover(cell)
    page$press("p")
    expect_true(page$wait_until(sprintf("document.querySelector('.%s-pin:not([hidden])') !== null", css)))
    pinned <- bench_tips(page)$text
    nodes <- tip_nodes(page)
    since <- page$mark()

    page$mouse(890, 690)
    page$hover(cell)
    page$hold(2) # the scenario's two-second rest: no second tip must appear, which has no signal
    expect_identical(bench_tips(page)$text, pinned)
    expect_true(bench_tips(page)$pinned)
    page$press("p")
    expect_identical(bench_tips(page)$text, pinned)
    expect_identical(tip_nodes(page), nodes)
    expect_identical(bench_tip_events(page, since), character())
    expect_bench_clean(page)
  })

  it("closes a pinned row when its tab hides or its modal closes, and leaves nothing stale on return (HC-047)", {
    page <- bench_page(bench, "tables")
    # Tabs switch through Bootstrap's own API: a reader's click on a tab is a click away, which closes the pin first.
    show_tab <- function(value) {
      page$evaluate(sprintf(
        "bootstrap.Tab.getOrCreateInstance(document.querySelector('#tbl_tabs [data-value=\"%s\"]')).show(); 0",
        value
      ))
      page$settle()
    }
    bench_scroll_to(page, "#tbl_tabs")
    show_tab("table")
    expect_true(page$wait_until("document.querySelector('#tbl_hidden tbody tr') !== null"))
    page$settle()
    first <- widths(page, "tbl_hidden")
    bench_scroll_to(page, "#tbl_hidden tbody tr:nth-child(1)")
    since <- page$mark()
    page$hover("#tbl_hidden tbody tr:nth-child(1) td:nth-child(1)")
    page$press("p")
    expect_true(page$wait_until(sprintf("document.querySelector('.%s-pin:not([hidden])') !== null", css)))
    show_tab("open")
    expect_true(page$wait_until(no_pin))
    expect_true("closed tbl_hidden/row/1 gone" %in% bench_tip_events(page, since))
    nodes <- tip_nodes(page)
    show_tab("table")
    expect_true(page$wait_until(no_pin))
    expect_identical(widths(page, "tbl_hidden"), first)
    expect_lte(tip_nodes(page), nodes)

    open_modal <- function(n) {
      page$evaluate(sprintf("Shiny.setInputValue('tbl_modal', %d, { priority: 'event' }); 0", n))
      expect_true(page$wait_until("document.querySelector('.modal.show #tbl_in_modal tbody tr') !== null"))
      page$settle()
    }
    close_modal <- function() {
      page$evaluate("bootstrap.Modal.getInstance(document.getElementById('shiny-modal')).hide(); 0")
      expect_true(page$wait_until("document.getElementById('shiny-modal') === null"))
      page$settle()
    }
    open_modal(1L)
    first <- widths(page, "tbl_in_modal")
    since <- page$mark()
    page$hover("#tbl_in_modal tbody tr:nth-child(1) td:nth-child(1)")
    page$press("p")
    expect_true(page$wait_until(sprintf("document.querySelector('.%s-pin:not([hidden])') !== null", css)))
    close_modal()
    expect_true(page$wait_until(no_pin))
    expect_true(any(startsWith(bench_tip_events(page, since), "closed tbl_in_modal/row/1")))
    open_modal(2L)
    expect_true(page$evaluate(no_pin))
    expect_identical(widths(page, "tbl_in_modal"), first)
    expect_identical(
      page$evaluate(
        "document.querySelectorAll('#tbl_in_modal tr.hovercraft-panel-row, #tbl_in_modal .hovercraft-panel').length"
      ),
      0L
    )
    close_modal()
    expect_bench_clean(page)
  })

  it("shows a hint only for the row under the pointer while the table is searched and scrolled (HC-048, partly)", {
    page <- bench_page(bench, "tables")
    expect_true(page$wait_until("document.querySelector('#tbl_scroll tbody tr') !== null"))
    page$settle()
    bench_scroll_to(page, "#tbl_scroll")
    # At rest, a hint on show is for the row under the pointer, and says that row's site: never a hint left over empty
    # space or over a row it is not for.
    honest <- function() {
      page$hold(0.6) # the engine's open and close delay, after the rows moved under a still pointer
      page$evaluate(sprintf(
        "(() => { const hint = document.querySelector('.%s:not(.%s-pin):not([hidden])');
          if (!hint) return true;
          const row = document.querySelector('#tbl_scroll tbody tr:hover');
          const site = row?.querySelector('td').textContent.match(/^Site \\d+/)?.[0];
          return site !== undefined && hint.textContent.startsWith(site + ' '); })()",
        css,
        css
      ))
    }
    since <- page$mark()
    page$hover("#tbl_scroll tbody tr:nth-child(2) td:nth-child(1)")
    expect_match(bench_tips(page)$text, "^Site 02 ")

    page$evaluate("document.querySelector('#tbl_scroll input[type=search]').focus(); 0")
    page$session$Input$insertText(text = "Peru")
    expect_true(page$wait_until("document.querySelectorAll('#tbl_scroll tbody tr').length === 10"))
    page$settle()
    expect_true(honest())
    expect_true(any(startsWith(bench_tip_events(page, since), "hidden tbl_scroll/row/2")))

    page$evaluate("$('#tbl_scroll table.dataTable').DataTable().search('').draw(); document.activeElement.blur(); 0")
    expect_true(page$wait_until("document.querySelectorAll('#tbl_scroll tbody tr').length === 60"))
    page$hover("#tbl_scroll tbody tr:nth-child(2) td:nth-child(1)")
    page$evaluate(
      "document.querySelector('#tbl_scroll .dataTables_scrollBody, #tbl_scroll .dt-scroll-body').scrollTop += 400; 0"
    )
    page$settle()
    expect_true(honest())
    page$evaluate("window.scrollBy(0, 300); 0")
    page$settle()
    expect_true(honest())
    expect_bench_clean(page)
  })
})

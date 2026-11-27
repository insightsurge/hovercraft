skip_if_no_browser(bench_packages)
skip_if_no_bench()

# The bench's performance and cleanup scenarios that had no test (#158): a fast sweep across thirty items (HC-072), a
# page with a thousand tagged cells (HC-073), and every tab loading with nothing on the console (HC-077, which every
# test-bench-*-browser.R scenario also ends with, through expect_bench_clean()).
describe("the bench's scale scenarios", {
  bench <- start_bench(environment())
  css <- tip_names()$css
  tip_nodes <- function(page) page$evaluate(sprintf("document.querySelectorAll('.%s').length", css))
  none_open <- sprintf("document.querySelector('.%s:not([hidden])') === null", css)

  it("shows exactly one hint at rest after a fast sweep across thirty items, and none once off them (HC-072)", {
    page <- bench_page(bench, "scale")
    bench_scroll_to(page, "#spot-HC-072")
    centres <- jsonlite::fromJSON(page$evaluate(
      "JSON.stringify([...Array(30).keys()].map((i) => {
        const r = document.getElementById('scale-thrash-' + String(i + 1).padStart(2, '0')).getBoundingClientRect();
        return [r.left + r.width / 2, r.top + r.height / 2]; }))"
    ))
    # Ten sweeps, there and back, with no pause between moves: as fast as the browser takes them.
    for (sweep in 1:10) {
      for (i in if (sweep %% 2L == 1L) 1:30 else 30:1) {
        page$session$Input$dispatchMouseEvent(type = "mouseMoved", x = centres[i, 1], y = centres[i, 2])
      }
    }
    page$mouse(centres[7, 1], centres[7, 2])
    expect_true(bench_wait_tips(page, "Item 07"))
    page$hold(0.6) # the engine's open delay: no other hint may follow the one at rest
    expect_identical(bench_tips(page)$text, "Item 07")
    expect_lte(tip_nodes(page), 1L)
    page$mouse(5, 5)
    expect_true(page$wait_until(none_open))
    expect_lte(tip_nodes(page), 1L)
    expect_bench_clean(page)
  })

  it("shows a cell's tip quickly among a thousand tagged cells and keeps one tip node after fifty hovers (HC-073, partly)", {
    page <- bench_page(bench, "scale")
    page$evaluate("Shiny.setInputValue('scale_cells', 1, { priority: 'event' }); 0")
    expect_true(page$wait_until("document.querySelectorAll('.scale-cell').length === 1000"))
    page$settle()
    # Times the first show in the page: from the pointer reaching the cell to its tip being on show.
    page$evaluate(sprintf(
      "window.benchTiming = { cell: null, want: null, moved: null, shown: null };
      document.addEventListener('pointermove', (e) => {
        const t = window.benchTiming;
        if (t.cell && t.moved === null && t.cell.contains(e.target)) t.moved = performance.now();
      }, true);
      new MutationObserver(() => {
        const t = window.benchTiming;
        const tip = document.querySelector('.%s:not([hidden])');
        if (t.moved !== null && t.shown === null && tip?.textContent === t.want) t.shown = performance.now();
      }).observe(document.body, { subtree: true, childList: true, attributes: true, characterData: true }); 0",
      css
    ))
    attribute <- tip_names()$attribute
    show_time <- function(index) {
      page$mouse(5, 5)
      expect_true(page$wait_until(none_open))
      at <- jsonlite::fromJSON(page$evaluate(sprintf(
        "(() => { const c = document.querySelectorAll('.scale-cell')[%d];
          c.scrollIntoView({ block: 'center', behavior: 'instant' });
          window.benchTiming = { ...window.benchTiming, cell: c, want: c.getAttribute('%s'), moved: null, shown: null };
          const r = c.getBoundingClientRect(); return JSON.stringify([r.left + r.width / 2, r.top + r.height / 2]); })()",
        index,
        attribute
      )))
      page$settle()
      page$mouse(at[[1]], at[[2]])
      expect_true(page$wait_until("window.benchTiming.shown !== null"))
      page$evaluate("window.benchTiming.shown - window.benchTiming.moved")
    }
    first <- show_time(500L)
    expect_gt(first, 0)
    # The open delay (about 300 ms) plus a generous 500 ms, so a busy machine does not fail the test.
    expect_lt(first, 800)

    nodes <- NULL
    for (index in seq(3L, 997L, length.out = 50L)) {
      expect_lt(show_time(as.integer(index)), 800)
      nodes <- c(nodes, tip_nodes(page))
    }
    expect_identical(unique(nodes), 1L)
    expect_bench_clean(page)
  })

  it("loads every tab with no error on the console, and no warning outside the table tabs (HC-077, partly)", {
    # The layering and tables tabs warn while a DT table with tips renders (a template named before it is defined),
    # reported in #158; their errors are still checked.
    warn_ok <- c("layering", "tables")
    for (tab in c("practice", "js", "layering", "placement", "input", "tables", "host", "a11y", "theme", "scale")) {
      # Each tab is opened and closed in its own scope, so one Shiny session is open at a time.
      local({
        page <- bench_page(bench, tab)
        expect_true(page$wait_until(sprintf("document.querySelector('.tab-pane[data-value=\"%s\"]') !== null", tab)))
        page$settle()
        expect_bench_clean(page)
        if (!tab %in% warn_ok) {
          warnings <- Filter(function(row) identical(row$kind, "warn"), page$log(0))
          expect_length(warnings, 0L)
        }
      })
    }
  })
})

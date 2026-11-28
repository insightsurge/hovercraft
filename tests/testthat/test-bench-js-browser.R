skip_if_no_browser(c("DT", "shinyjs"))
skip_if_no_bench()

# The JavaScript-driven UI practice, one test per way an app's own JavaScript changes the page: the test bench's js
# tab (apps/testbench/R/tab-js.R and www/tab-js.js). Each test hovers the tagged element after the page's script ran,
# reads the tip, and checks the bench's log where the engine's event says more than the page does.
describe("tips on a page whose own JavaScript changes it", {
  bench <- start_bench(environment())
  page <- bench_page(bench, "js")
  exists_js <- function(id) sprintf("document.getElementById('%s') !== null", id)
  no_tip_open <- function() {
    page$wait_until(sprintf("document.querySelectorAll('.%s:not([hidden])').length === 0", tip_names()$css))
  }

  it("draws the tip of a tagged element a custom message put in the page", {
    page$click_now("#js_msg_send")
    expect_true(page$wait_until(exists_js("js-msg-btn")))
    expect_identical(page$hover_in_view("#js-msg-btn"), "Sent by a custom message")
    expect_true(page$log_has("hint", "js_msg_btn_tip", state = "shown"))
  })

  it("keeps a tip working on a button whose click sends Shiny.setInputValue()", {
    page$click_now("#js-set-btn")
    page$click_now("#js-set-btn")
    expect_true(page$wait_until("document.getElementById('js_set_seen')?.textContent === 'js_set: value 2'"))
    expect_true(page$log_has("input", "js_set"))
    expect_identical(page$hover_in_view("#js-set-btn"), "Sends js_set by setInputValue")
  })

  it("draws tips on HTML put in by innerHTML and by jQuery .html() and .append()", {
    page$click_now("#js-html-add")
    page$click_now("#js-jq-add")
    expect_true(page$wait_until(exists_js("js-html-btn")))
    expect_true(page$wait_until(exists_js("js-jq-btn")))
    expect_identical(page$hover_in_view("#js-html-btn"), "Inserted by innerHTML")
    expect_identical(page$hover_in_view("#js-jq-btn"), "Inserted by jQuery")
  })

  it("draws the tip of an input bound by Shiny.bindAll(), whose value reaches the server", {
    page$click_now("#js-input-add")
    expect_true(page$wait_until("document.getElementById('js_bound_seen')?.textContent === 'js_bound: first'"))
    expect_identical(page$hover_in_view("#js_bound"), "A bound input's tip")
  })

  it("draws the tip of a button drawn by Shiny.renderContent()", {
    page$click_now("#js-render-add")
    expect_true(page$wait_until(exists_js("js-render-btn")))
    expect_identical(page$hover_in_view("#js-render-btn"), "Drawn by renderContent")
  })

  it("closes the open tip as gone when the page unbinds and removes its element", {
    expect_identical(page$hover_in_view("#js_bound"), "A bound input's tip")
    since <- page$mark()
    page$click_now("#js-remove")
    expect_true(page$wait_until("document.getElementById('js_bound') === null"))
    expect_true(no_tip_open())
    expect_true(page$log_has("hint", "js_bound_tip", state = "hidden", since = since))
  })

  it("follows a tagged element the page moves, and draws the tip of the node that replaced it", {
    page$click_now("#js-move")
    expect_true(page$wait_until("document.getElementById('js-mov-btn')?.parentElement?.id === 'js-box-b'"))
    expect_identical(page$hover_in_view("#js-mov-btn"), "The moving button's tip")
    tip_beside <- page$evaluate(sprintf(
      "(() => {
        const tip = document.querySelector('.%s:not([hidden])')?.getBoundingClientRect();
        const item = document.getElementById('js-mov-btn').getBoundingClientRect();
        return !!tip && Math.abs((tip.left + tip.right) / 2 - (item.left + item.right) / 2) < item.width + tip.width;
      })()",
      tip_names()$css
    ))
    expect_true(tip_beside)
    page$click_now("#js-replace")
    expect_true(page$wait_until(exists_js("js-rep-btn")))
    expect_identical(page$hover_in_view("#js-rep-btn"), "The new node's tip")
  })

  it("shows the new words when the page changes the tip's attribute, a class and aria-label", {
    expect_identical(page$hover_in_view("#js-attr-btn"), "Words before")
    page$click_now("#js-attr-change")
    expect_true(page$wait_until("document.getElementById('js-attr-btn')?.classList.contains('btn-warning')"))
    expect_identical(page$hover_in_view("#js-attr-btn"), "Words after")
  })

  it("draws a tip the page adds on shiny:value, and the page still hears shiny:inputchanged", {
    page$click_now("#js_tick")
    expect_true(page$wait_until("document.getElementById('js_value_out')?.textContent === 'Tick 1'"))
    expect_true(page$wait_until(sprintf(
      "document.getElementById('js_value_out')?.getAttribute('%s') === 'Tagged on shiny:value: Tick 1'",
      tip_contract()$attributes$tip
    )))
    expect_identical(page$hover_in_view("#js_value_out"), "Tagged on shiny:value: Tick 1")
    expect_true(page$wait_until("Number(document.getElementById('js-changed-count')?.textContent) > 0"))
  })

  it("follows shinyjs: no tip on a hidden button, the tip again once shown, and after a class change", {
    expect_identical(page$hover_in_view("#js-sjs-btn"), "The shinyjs button's tip")
    page$click_now("#js_sjs_hide")
    expect_true(page$wait_until("getComputedStyle(document.getElementById('js-sjs-btn')).display === 'none'"))
    expect_true(no_tip_open())
    page$click_now("#js_sjs_show")
    page$click_now("#js_sjs_toggle")
    expect_true(page$wait_until("document.getElementById('js-sjs-btn')?.classList.contains('btn-warning')"))
    expect_identical(page$hover_in_view("#js-sjs-btn"), "The shinyjs button's tip")
  })

  it("draws tips inside Bootstrap's own collapse, dropdown and modal, and beside its tooltip and popover", {
    page$click_now("#js-bs-collapse-open")
    expect_true(page$wait_until(
      "document.querySelector('#js-bs-collapse.show') !== null &&
        document.querySelector('#js-bs-collapse.collapsing') === null"
    ))
    expect_identical(page$hover_in_view("#js-bs-col-btn"), "Inside Bootstrap's collapse")

    page$click_now("#js-bs-drop-open")
    expect_true(page$wait_until("document.querySelector('.dropdown-menu.show #js-bs-drop-item') !== null"))
    expect_identical(page$hover("#js-bs-drop-item"), "Inside Bootstrap's dropdown")
    page$press("Escape", wait = 0)
    expect_true(page$wait_closed())

    expect_length(page$hover_in_view("#js-bs-tooltip", opens = FALSE), 0L)
    expect_true(page$wait_until("document.querySelector('.tooltip.show') !== null"))
    expect_identical(page$hover_in_view("#js-bs-near"), "A tip beside Bootstrap's")

    page$click_now("#js-bs-modal-open")
    expect_true(page$wait_until("document.querySelector('#js-bs-modal.show') !== null"))
    page$settle()
    expect_identical(page$hover("#js-bs-modal-btn"), "Inside Bootstrap's modal")
    page$evaluate("document.querySelector('#js-bs-modal [data-bs-dismiss]').click()")
    expect_true(page$wait_until("document.querySelector('#js-bs-modal.show') === null"))
  })

  it("draws the tip a widget's render callback put on a table cell", {
    expect_true(page$wait_until(exists_js("js-dt-cell")))
    expect_identical(page$hover_in_view("#js-dt-cell"), "Tagged by the render callback")
  })

  it("keeps its own message and event names apart from the app's", {
    page$evaluate("window.scrollTo(0, 0); 0")
    since <- page$mark()
    page$click_now("#js_names_show")
    expect_true(page$wait_until(
      "document.getElementById('js-names-seen')?.textContent === \"show: the app's own show\""
    ))
    expect_true(page$log_has("hint", "js_names_tip", state = "shown", by = "host", since = since))
    expect_true(page$log_has("input", "js_names_tip_hint", since = since))
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")
skip_if_no_bench()

# Shiny practices in a running bslib/Shiny app: renderUI() re-render, insertUI()/removeUI(), modules (two copies,
# nested, dynamic), hidden containers (tabs, accordion, conditionalPanel, collapsed sidebar), modals and full-screen
# cards, input updates and disabled state, dynamic tabs, bindCache() across two sessions, multi-session isolation,
# and embedding in an iframe. The app is the test bench's practice tab (apps/testbench).
describe("Shiny practices in a running app", {
  bench <- start_bench(environment())
  app <- bench$proc
  url <- bench_url(bench, "practice")
  up <- wait_for_app(app, url)
  page <- local_page(url, environment())
  press_button <- function(id) page$click_now(paste0("#", id))
  hover_in_view <- function(selector) {
    page$evaluate(sprintf(
      "document.querySelector(%s)?.scrollIntoView({ block: 'center', behavior: 'instant' }); 0",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    ))
    page$settle(0.1)
    page$hover(selector)
  }
  seen <- function(id, pattern) {
    page$wait_until(sprintf(
      "new RegExp(%s).test(document.getElementById(%s)?.textContent ?? '')",
      jsonlite::toJSON(pattern, auto_unbox = TRUE),
      jsonlite::toJSON(id, auto_unbox = TRUE)
    ))
  }

  it("starts and connects", {
    expect_true(up, info = paste(app$read_error(), collapse = "\n"))
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
  })

  it("closes an open tip on renderUI() re-render and shows the new tip without reload", {
    expect_true(page$wait_until("document.getElementById('dyn_btn') !== null"))
    expect_identical(hover_in_view("#dyn_btn"), "Render tip 1")
    press_button("rerender")
    expect_true(page$wait_until("document.getElementById('dyn_btn')?.textContent?.includes('Version 2')"))
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('.%s:not([hidden])').length === 0",
      tip_names()$css
    )))
    expect_identical(hover_in_view("#dyn_btn"), "Render tip 2")
  })

  it("shows a tip on an insertUI() tag and closes it cleanly on removeUI()", {
    press_button("ins_add")
    expect_true(page$wait_until("document.getElementById('ins_btn') !== null"))
    expect_identical(page$hover("#ins_btn"), "Inserted tip")
    expect_true(seen("ins_seen", "^shown reader"))
    press_button("ins_rm")
    expect_true(page$wait_until("document.getElementById('ins_btn') === null"))
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('.%s:not([hidden])').length === 0",
      tip_names()$css
    )))
    expect_true(seen("ins_seen", "^hidden data"))
  })

  it("isolates two module copies, nested modules, and a dynamically inserted module", {
    expect_identical(page$hover("#mod_a-btn"), "Alpha: Alpha")
    expect_identical(page$hover("#mod_b-btn"), "Beta: Beta")
    press_button("mod_a-show")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden])')?.textContent === 'Alpha: Alpha'",
      tip_names()$css
    )))
    expect_identical(page$hover("#outer-inner-btn"), "Nested: Nested")
    press_button("outer-inner-show")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden])')?.textContent === 'Nested: Nested'",
      tip_names()$css
    )))
    press_button("add_dyn_mod")
    expect_true(page$wait_until("document.getElementById('dyn_mod-btn') !== null"))
    expect_identical(page$hover("#dyn_mod-btn"), "Dynamic: Dynamic")
    press_button("dyn_mod-show")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden])')?.textContent === 'Dynamic: Dynamic'",
      tip_names()$css
    )))
  })

  it("shows tips inside containers that start hidden (tab, accordion, conditionalPanel, collapsed sidebar)", {
    # Second tab
    page$evaluate("document.querySelector('#tabs a[data-value=\"Second\"]').click()")
    expect_true(page$wait_until(
      "document.getElementById('tab2_btn')?.getBoundingClientRect().height > 0"
    ))
    expect_identical(hover_in_view("#tab2_btn"), "Hidden tab tip")

    # Accordion: wait for Bootstrap's collapse animation to finish (.show and no .collapsing).
    page$evaluate("document.querySelector('#acc .accordion-button').click()")
    expect_true(page$wait_until(
      "document.querySelector('#acc .accordion-collapse.show') !== null && document.querySelector('#acc .accordion-collapse.collapsing') === null"
    ))
    expect_identical(hover_in_view("#acc_btn"), "Accordion tip")

    # conditionalPanel
    page$click_now("#show_cond")
    expect_true(page$wait_until(
      "document.getElementById('cond_btn')?.getBoundingClientRect().height > 0"
    ))
    expect_identical(hover_in_view("#cond_btn"), "Conditional tip")

    # Collapsed bslib sidebar
    page$evaluate("document.querySelector('.bslib-sidebar-layout .collapse-toggle').click()")
    expect_true(page$wait_until(
      "document.getElementById('sb_btn')?.getBoundingClientRect().width > 0"
    ))
    page$settle()
    expect_identical(hover_in_view("#sb_btn"), "Sidebar tip")
  })

  it("shows a tip inside showModal() above the modal backdrop", {
    press_button("open_modal")
    expect_true(page$wait_until("document.getElementById('modal_btn') !== null"))
    page$settle()
    expect_identical(page$hover("#modal_btn"), "Modal tip")
    tip_above <- page$evaluate(sprintf(
      "(() => {
        const tip = document.querySelector('.%s:not([hidden])');
        const modal = document.querySelector('.modal');
        if (!tip || !modal) return false;
        const tz = Number(getComputedStyle(tip).zIndex) || 0;
        const mz = Number(getComputedStyle(modal).zIndex) || 0;
        return tz >= mz && tip.getBoundingClientRect().height > 0;
      })()",
      tip_names()$css
    ))
    expect_true(tip_above)
    page$evaluate("document.querySelector('.modal .btn-close, .modal [data-bs-dismiss=\"modal\"]')?.click()")
    expect_true(page$wait_until("document.getElementById('modal_btn') === null"))
  })

  it("shows a tip inside a bslib full-screen card", {
    page$evaluate("document.querySelector('#fs_card .bslib-full-screen-enter').click()")
    expect_true(page$wait_until("document.getElementById('fs_card')?.dataset.fullScreen === 'true'"))
    page$settle()
    expect_identical(page$hover("#fs_btn"), "Card tip")
    tip_above <- page$evaluate(sprintf(
      "(() => {
        const tip = document.querySelector('.%s:not([hidden])');
        const card = document.getElementById('fs_card');
        if (!tip || !card) return false;
        const tz = Number(getComputedStyle(tip).zIndex) || 0;
        const cz = Number(getComputedStyle(card).zIndex) || 0;
        return tz >= cz && tip.getBoundingClientRect().height > 0;
      })()",
      tip_names()$css
    ))
    expect_true(tip_above)
    page$evaluate("document.querySelector('.bslib-full-screen-exit')?.click()")
    expect_true(page$wait_until("document.getElementById('fs_card')?.dataset.fullScreen !== 'true'"))
  })

  it("keeps tips working after updateActionButton() and updateTextInput()", {
    press_button("upd_inputs")
    expect_true(page$wait_until("document.getElementById('upd_btn')?.textContent?.includes('After')"))
    expect_true(page$wait_until("document.getElementById('upd_txt')?.value === 'v2'"))
    expect_identical(hover_in_view("#upd_btn"), "Updated button tip")
    expect_identical(hover_in_view("#upd_txt"), "Updated input tip")
  })

  it("handles a disabled button cleanly without console errors", {
    # Native disabled buttons suppress pointer events in browsers, so no tip opens and no error is raised.
    shown_disabled <- hover_in_view("#dis_btn")
    expect_length(shown_disabled, 0L)
    expect_identical(page$problems(), character())
  })

  it("works with appendTab() and closes an open tip on removeTab()", {
    press_button("add_tab")
    expect_true(page$wait_until("document.getElementById('dyn_tab_btn')?.getBoundingClientRect().height > 0"))
    expect_identical(hover_in_view("#dyn_tab_btn"), "Dynamic tab tip")
    press_button("rm_tab")
    expect_true(page$wait_until("document.getElementById('dyn_tab_btn') === null"))
    expect_true(page$wait_until(sprintf(
      "document.querySelectorAll('.%s:not([hidden])').length === 0",
      tip_names()$css
    )))
  })

  it("serves identical bindCache() renderUI() markup across two sessions and isolates server calls", {
    page2 <- local_page(url, environment())
    expect_true(page2$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    expect_true(page$wait_until("document.getElementById('cached_btn') !== null"))
    expect_true(page2$wait_until("document.getElementById('cached_btn') !== null"))

    # Compare the server-rendered cached markup (strip client-side engine aria-describedby and .hc-tip-desc).
    server_html <- function(p) {
      p$evaluate(sprintf(
        "(() => {
          const clone = document.getElementById('cached_ui').cloneNode(true);
          clone.querySelectorAll('.%s-desc').forEach((n) => n.remove());
          clone.querySelectorAll('[aria-describedby]').forEach((n) => n.removeAttribute('aria-describedby'));
          return clone.innerHTML;
        })()",
        tip_names()$css
      ))
    }
    expect_identical(server_html(page), server_html(page2))
    expect_identical(hover_in_view("#cached_btn"), "Cached tip")
    page2$evaluate("document.querySelector('#cached_btn')?.scrollIntoView({ block: 'center', behavior: 'instant' }); 0")
    page2$settle(0.1)
    expect_identical(page2$hover("#cached_btn"), "Cached tip")

    # Clear tip in page2, trigger tip_show in page1, verify page2 stays closed.
    page2$press("Escape", wait = 0.3)
    expect_length(page2$shown(), 0L)
    press_button("iso_show")
    expect_true(page$wait_until(sprintf(
      "document.querySelector('.%s:not([hidden])')?.textContent === 'Isolated tip'",
      tip_names()$css
    )))
    page2$hold(0.3) # nothing may open in the other page: no signal for that
    expect_length(page2$shown(), 0L)
    expect_identical(page2$problems(), character())
  })

  it("works when the Shiny app is embedded inside an iframe", {
    outer <- local_page(bench_url(bench, "practice", list(host = 1)), environment())
    expect_true(outer$wait_until(
      "document.getElementById('f')?.contentWindow?.Shiny?.shinyapp?.isConnected?.() === true"
    ))
    expect_true(outer$wait_until(
      "document.getElementById('f')?.contentDocument?.getElementById('iso_btn') !== null"
    ))
    outer$evaluate("document.getElementById('f').contentDocument.getElementById('iso_show').click()")
    expect_true(outer$wait_until(sprintf(
      "document.getElementById('f')?.contentDocument?.querySelector('.%s:not([hidden])')?.textContent === 'Isolated tip'",
      tip_names()$css
    )))
    expect_identical(outer$problems(), character())
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

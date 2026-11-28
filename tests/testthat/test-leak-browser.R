skip_if_no_browser(bench_packages)
skip_if_no_bench()

# What the page script makes and must let go of when Shiny draws or removes its host (#131): watchers
# (MutationObserver, ResizeObserver), event listeners, animation frames, timers, helper boxes (pin and More strips,
# parked outputs), duplicate ids and the table script's panels. (Animation frames and timers are not counted: at rest
# they are zero in every run, so a count of them proves nothing; a leaked one would still show as a growing observer or
# listener.) Each scenario draws the same thing once, counts what is
# alive, draws it 30 more times and counts again: a count that grew is a leak.
#
# The counts come from a shim that the page loads before its own scripts (`leak_shim`): it wraps the browser's
# observers and `addEventListener`, and counts only what still matters: an observer
# or a listener on an element that is no longer in the page is garbage, not a leak.
leak_draws <- 30L

leak_shim <- "
(() => {
  const observers = new Set();
  const wrapObserver = (name) => {
    const Native = window[name];
    if (typeof Native !== 'function') return;
    window[name] = class extends Native {
      constructor(callback) {
        super(callback);
        this.__targets = new Set();
        observers.add(this);
      }
      observe(target, options) {
        this.__targets.add(target);
        return super.observe(target, options);
      }
      unobserve(target) {
        this.__targets.delete(target);
        return super.unobserve(target);
      }
      disconnect() {
        this.__targets.clear();
        return super.disconnect();
      }
    };
    window[name].__kind = name;
  };
  wrapObserver('MutationObserver');
  wrapObserver('ResizeObserver');

  const listeners = [];
  const nativeAdd = EventTarget.prototype.addEventListener;
  const nativeRemove = EventTarget.prototype.removeEventListener;
  const same = (e, target, type, fn, capture) =>
    e.target === target && e.type === type && e.fn === fn && e.capture === capture;
  EventTarget.prototype.addEventListener = function (type, fn, options) {
    const capture = typeof options === 'boolean' ? options : Boolean(options?.capture);
    if (fn && !listeners.some((e) => same(e, this, type, fn, capture))) {
      listeners.push({ target: this, type, fn, capture, once: Boolean(options?.once), signal: options?.signal ?? null });
    }
    return nativeAdd.call(this, type, fn, options);
  };
  EventTarget.prototype.removeEventListener = function (type, fn, options) {
    const capture = typeof options === 'boolean' ? options : Boolean(options?.capture);
    const at = listeners.findIndex((e) => same(e, this, type, fn, capture));
    if (at >= 0) listeners.splice(at, 1);
    return nativeRemove.call(this, type, fn, options);
  };

  const inPage = (node) => !(node instanceof Node) || node.isConnected;
  const alive = (kind) =>
    [...observers].filter((o) => o.constructor.__kind === kind && [...o.__targets].some(inPage)).length;
  const duplicates = () => {
    const seen = new Map();
    for (const el of document.querySelectorAll('[id]')) seen.set(el.id, (seen.get(el.id) ?? 0) + 1);
    return [...seen.values()].filter((n) => n > 1).length;
  };
  window.__leak = {
    snapshot: () => ({
      mutation_observers: alive('MutationObserver'),
      resize_observers: alive('ResizeObserver'),
      listeners: listeners.filter((e) => !e.once && !e.signal?.aborted && inPage(e.target)).length,
      strips: document.querySelectorAll('.hovercraft-pin-strip, .hovercraft-more-strip').length,
      parked: document.querySelectorAll('body > .hc-output-park > *').length,
      duplicate_ids: duplicates(),
      panels: window.HovercraftTable?.stats().panels ?? 0,
      table_observers: window.HovercraftTable?.stats().observers ?? 0,
    }),
  };
})();
"

describe("a host that Shiny draws again or removes", {
  # The bench's leak tab (apps/testbench/R/tab-leaks.R), alone and with two pins open.
  app <- start_bench(environment())
  leaks_url <- bench_url(app, "leaks", list(max_pins = 2))
  page <- local_page(leaks_url, environment(), init = leak_shim)
  first_template <- NULL

  snapshot <- function() {
    page$mouse(890, 690)
    page$settle(1)
    unlist(jsonlite::fromJSON(page$evaluate("JSON.stringify(window.__leak.snapshot())")))
  }
  set_input <- function(name, value) {
    page$evaluate(sprintf("Shiny.setInputValue('%s', %d, { priority: 'event' })", name, value))
  }
  # Draws again `leak_draws` times: sets the input to 1, 2, ... and waits for the page to show that version.
  redraw <- function(input, ready, hover = NULL) {
    for (i in seq_len(leak_draws)) {
      set_input(input, i)
      expect_true(page$wait_until(sprintf(ready, i)), info = paste(input, "draw", i))
      if (!is.null(hover)) page$hover(hover)
    }
  }
  expect_no_growth <- function(before, after) {
    grew <- names(after)[after > before[names(after)]]
    info <- paste(sprintf("%s: %d -> %d", names(after), before[names(after)], after), collapse = "\n")
    expect_length(grew, 0L)
    if (length(grew)) message("Counts after ", leak_draws, " draws (before -> after):\n", info)
  }

  count_id <- function(id) page$evaluate(sprintf("document.querySelectorAll('[id=\"%s\"]').length", id))
  bound_once <- function(id) {
    page$evaluate(sprintf("window.Shiny.shinyapp.$bindings['%s']?.el === document.getElementById('%s')", id, id))
  }
  # Pins an item by the engine's own door, as the pin key does, and closes every tip.
  pin_item <- function(id) {
    page$evaluate(sprintf("window.Hovercraft.pin(document.getElementById('%s'), { by: 'reader', via: 'key' }); 0", id))
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin') !== null"))
  }
  close_tips <- function() {
    page$evaluate("window.Hovercraft.close(); 0")
    expect_true(page$wait_unpinned())
    page$settle()
  }
  held_stats <- function() unlist(jsonlite::fromJSON(page$evaluate("JSON.stringify(window.HovercraftShiny.stats())")))
  # What the first row of the reactable says its template is: a name made from the words, so it changes with them.
  row_template <- function(p) {
    p$evaluate(sprintf(
      "document.querySelector('#leaks_rt [class*=\"hovercraft-row-\"]')?.getAttribute('%s')",
      tip_attribute("template")
    ))
  }

  it("starts and connects", {
    expect_true(app$proc$is_alive())
    expect_true(page$wait_connected())
    expect_true(page$wait_until("document.getElementById('leaks_ru_btn') !== null"))
    expect_true(page$wait_until("document.querySelector('#leaks_dt td') !== null"))
    expect_true(page$wait_until("document.querySelector('#leaks_rt .rt-td') !== null"))
    expect_true(page$wait_until(sprintf(
      "document.querySelector('#leaks_rt [class*=\"hovercraft-row-\"]')?.hasAttribute('%s') === true",
      tip_attribute("template")
    )))
    first_template <<- row_template(page)
  })

  it("keeps the counts of a tagged item that renderUI() draws 30 times", {
    page$hover("#leaks_ru_btn")
    before <- snapshot()
    redraw(
      "leaks_ru_go",
      "document.getElementById('leaks_ru_btn')?.textContent?.includes('RU v%d')",
      hover = "#leaks_ru_btn"
    )
    expect_no_growth(before, snapshot())
  })

  it("keeps the counts of a tagged item that insertUI() adds and removeUI() takes away 30 times", {
    cycle <- function(i) {
      set_input("leaks_ins_go", i)
      expect_true(page$wait_until("document.getElementById('leaks_ins_btn') !== null"))
      set_input("leaks_ins_rm", i)
      expect_true(page$wait_until("document.getElementById('leaks_ins_btn') === null"))
    }
    cycle(1L)
    before <- snapshot()
    for (i in 2:(leak_draws + 1L)) {
      cycle(i)
    }
    expect_no_growth(before, snapshot())
  })

  it("keeps the counts of a module that is added and removed 30 times", {
    cycle <- function(i) {
      set_input("leaks_mod_go", i)
      expect_true(page$wait_until(sprintf("document.getElementById('leaks_m%d-btn') !== null", i)))
      set_input("leaks_mod_rm", i)
      expect_true(page$wait_until(sprintf("document.getElementById('leaks_m%d-btn') === null", i)))
    }
    cycle(1L)
    before <- snapshot()
    for (i in 2:(leak_draws + 1L)) {
      cycle(i)
    }
    expect_no_growth(before, snapshot())
  })

  it("keeps the counts of a DT table that renderDT() draws 30 times", {
    before <- snapshot()
    redraw("leaks_dt_go", "document.querySelector('#leaks_dt tbody tr td:nth-child(2)')?.textContent === '%d'")
    expect_no_growth(before, snapshot())
  })

  it("keeps the counts of a reactable table that renderReactable() draws 30 times", {
    before <- snapshot()
    redraw("leaks_rt_go", "document.querySelector('#leaks_rt .rt-tbody .rt-td:nth-child(2)')?.textContent === '%d'")
    expect_no_growth(before, snapshot())
  })

  it("leaves a reactable table quiet after it is drawn again with other words: no watcher writes the old ones back", {
    # Stale watchers on one container each re-mark the rows with their own draw's words, and each write wakes the others:
    # an endless exchange of attribute writes. A table drawn once is quiet, so the writes in 400 ms are few.
    page$evaluate(paste(
      "window.writes = 0;",
      "new MutationObserver((list) => { window.writes += list.length; }).observe(",
      "  document.getElementById('leaks_rt'), { subtree: true, childList: true, attributes: true, characterData: true }",
      "); 0"
    ))
    set_input("leaks_rt_go", leak_draws + 1L)
    expect_true(page$wait_until(
      "document.querySelector('#leaks_rt .rt-tbody .rt-td:nth-child(2)')?.textContent === '31'"
    ))
    page$settle(1)
    page$evaluate("window.writes = 0; 0")
    page$hold(0.4)
    expect_lt(page$evaluate("window.writes"), 5L)
    # The words are the last draw's, not the first's and not an earlier one's: the same as a table drawn once with them.
    fresh <- local_page(bench_url(app, "leaks", list(max_pins = 2, rt = 31)), environment())
    expect_true(fresh$wait_until("document.querySelector('#leaks_rt [class*=\"hovercraft-row-\"]') !== null"))
    expect_true(fresh$wait_until(sprintf(
      "document.querySelector('#leaks_rt [class*=\"hovercraft-row-\"]')?.hasAttribute('%s') === true",
      tip_attribute("template")
    )))
    expect_identical(row_template(page), row_template(fresh))
    expect_false(identical(row_template(page), first_template))
  })

  it("keeps one parked output for a tip_output() whose region draws again, and none when it is cleared", {
    expect_true(page$wait_until("document.getElementById('leaks_pk_btn') !== null"))
    page$settle()
    before <- snapshot()
    redraw("leaks_pk_go", "document.getElementById('leaks_pk_btn')?.textContent?.includes('PK v%d')")
    expect_no_growth(before, snapshot())
    # The output is whole when the tip opens: once in the page, bound once, with its text.
    expect_identical(count_id("leaks_pk_out"), 1L)
    pin_item("leaks_pk_btn")
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip-pin #leaks_pk_out')?.textContent === 'The output the tip holds'"
    ))
    expect_identical(count_id("leaks_pk_out"), 1L)
    expect_true(bound_once("leaks_pk_out"))
    # Its text is in the tip once, not twice.
    expect_identical(
      page$evaluate(
        "document.querySelector('.hc-tip-pin').textContent.split('The output the tip holds').length - 1"
      ),
      1L
    )
    close_tips()
    set_input("leaks_pk_clear", 1L)
    expect_true(page$wait_until("document.getElementById('leaks_pk_btn') === null"))
    page$settle()
    expect_identical(page$evaluate("document.querySelectorAll('.hc-output-park [id=\"leaks_pk_out\"]').length"), 0L)
    expect_identical(count_id("leaks_pk_out"), 0L)
  })

  it("parks the output once when only its region draws again, and its tip is defined outside the region", {
    expect_true(page$wait_until("document.getElementById('leaks_pkb_out') !== null"))
    redraw("leaks_pkb_go", "document.getElementById('leaks_pkb_out')?.dataset.v === '%d'")
    page$settle()
    expect_identical(count_id("leaks_pkb_out"), 1L)
    pin_item("leaks_pkb_btn")
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin #leaks_pkb_out')?.textContent === 'Answer pkb'"))
    expect_identical(count_id("leaks_pkb_out"), 1L)
    expect_true(bound_once("leaks_pkb_out"))
    close_tips()
    expect_identical(count_id("leaks_pkb_out"), 1L)
  })

  it("keeps one element with the id when the region draws again while its tip is open", {
    pin_item("leaks_pkb_btn")
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin #leaks_pkb_out') !== null"))
    set_input("leaks_pkb_go", 99L)
    expect_true(page$wait_until("document.getElementById('leaks_pkb_out')?.dataset.v === '99'"))
    expect_true(page$wait_until("document.querySelectorAll('[id=\"leaks_pkb_out\"]').length === 1"))
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin #leaks_pkb_out') !== null"))
    expect_true(bound_once("leaks_pkb_out"))
    close_tips()
    expect_identical(count_id("leaks_pkb_out"), 1L)
  })

  it("lets go of an output whose region is cleared while its tip is open, when the tip closes", {
    expect_true(page$wait_until("document.getElementById('leaks_pkd_out') !== null"))
    before <- held_stats()
    pin_item("leaks_pkd_btn")
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin #leaks_pkd_out') !== null"))
    set_input("leaks_pkd_clear", 1L)
    expect_true(page$wait_until(sprintf("window.HovercraftShiny.stats().marks === %d", before[["marks"]] - 1L)))
    close_tips()
    expect_identical(count_id("leaks_pkd_out"), 0L)
    expect_identical(page$evaluate("document.querySelectorAll('.hc-output-park [id=\"leaks_pkd_out\"]').length"), 0L)
    after <- held_stats()
    expect_identical(unname(after[["outputs"]]), unname(before[["outputs"]]))
    expect_identical(unname(after[["shells"]]), unname(before[["shells"]]) - 1L)
  })

  it("keeps nothing for an output with keep_alive = FALSE once its region is cleared after its tip closed", {
    expect_true(page$wait_until("document.getElementById('leaks_pkc_out') !== null"))
    pin_item("leaks_pkc_btn")
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin #leaks_pkc_out') !== null"))
    close_tips()
    before <- held_stats()
    set_input("leaks_pkc_clear", 1L)
    expect_true(page$wait_until(sprintf("window.HovercraftShiny.stats().marks === %d", before[["marks"]] - 1L)))
    after <- held_stats()
    expect_identical(unname(after[["outputs"]]), unname(before[["outputs"]]) - 1L)
    expect_identical(unname(after[["shells"]]), unname(before[["shells"]]) - 1L)
    expect_identical(count_id("leaks_pkc_out"), 0L)
  })

  it("shows the output again when its region is cleared and drawn again while the tip is open, and the tip then closes", {
    expect_true(page$wait_until("document.getElementById('leaks_pke_v') !== null"))
    expect_true(page$wait_until("document.getElementById('leaks_pke_out') !== null"))
    pin_item("leaks_pke_btn")
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin #leaks_pke_out')?.textContent === 'Answer pke'"))
    before <- held_stats()
    set_input("leaks_pke_clear", 1L)
    expect_true(page$wait_until("document.getElementById('leaks_pke_v') === null"))
    expect_true(page$wait_until(sprintf("window.HovercraftShiny.stats().marks === %d", before[["marks"]] - 1L)))
    set_input("leaks_pke_clear", 2L)
    expect_true(page$wait_until("document.getElementById('leaks_pke_v') !== null"))
    expect_true(page$wait_until(sprintf("window.HovercraftShiny.stats().marks === %d", before[["marks"]])))
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin #leaks_pke_out')?.textContent === 'Answer pke'"))
    expect_identical(count_id("leaks_pke_out"), 1L)
    close_tips()
    # The output is parked, whole, and shows when the tip opens again.
    expect_identical(count_id("leaks_pke_out"), 1L)
    pin_item("leaks_pke_btn")
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin #leaks_pke_out')?.textContent === 'Answer pke'"))
    expect_identical(count_id("leaks_pke_out"), 1L)
    expect_true(bound_once("leaks_pke_out"))
    close_tips()
  })

  it("hands a redrawn output to the box that holds it when two pinned tips share one output", {
    expect_true(page$wait_until("document.getElementById('leaks_pkf_out') !== null"))
    before <- held_stats()
    pin_item("leaks_pkf_btn")
    pin_item("leaks_pkf_btn2")
    expect_true(page$wait_until("document.querySelectorAll('.hc-tip-pin:not([hidden])').length === 2"))
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin:not([hidden]) #leaks_pkf_out') !== null"))
    set_input("leaks_pkf_go", 2L)
    expect_true(page$wait_until("document.getElementById('leaks_pkf_out')?.dataset.v === '2'"))
    expect_true(page$wait_until("document.querySelectorAll('[id=\"leaks_pkf_out\"]').length === 1"))
    expect_true(bound_once("leaks_pkf_out"))
    # The box that holds the output closes first (the newest pin), and the other takes it.
    page$evaluate("window.Hovercraft.close(document.getElementById('leaks_pkf_btn2')); 0")
    expect_true(page$wait_until("document.querySelectorAll('.hc-tip-pin:not([hidden])').length === 1"))
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip-pin:not([hidden]) #leaks_pkf_out')?.textContent === 'Answer pkf'"
    ))
    expect_identical(count_id("leaks_pkf_out"), 1L)
    expect_true(bound_once("leaks_pkf_out"))
    page$evaluate("window.Hovercraft.close(document.getElementById('leaks_pkf_btn')); 0")
    expect_true(page$wait_unpinned())
    page$settle()
    expect_identical(count_id("leaks_pkf_out"), 1L)
    expect_identical(page$evaluate("document.querySelectorAll('.hc-output-park [id=\"leaks_pkf_out\"]').length"), 1L)
    after <- held_stats()
    expect_identical(unname(after[["shells"]]), unname(before[["shells"]]))
    expect_identical(unname(after[["marks"]]), unname(before[["marks"]]))
  })

  it("takes a panel and the watcher of its table back at each close, over 30 pins and redraws", {
    expect_true(page$wait_until("document.querySelector('#leaks_dtp tbody tr[data-hovercraft-tip-id]') !== null"))
    cycle <- function(i) {
      # As a keyboard reader does: the row takes focus (the table gives it a tab stop a frame after it is drawn), then P.
      row <- "#leaks_dtp tbody tr[data-hovercraft-tip-id]"
      expect_true(page$wait_until(sprintf("document.querySelector('%s')?.hasAttribute('tabindex') === true", row)))
      page$evaluate(sprintf("document.querySelector('%s').focus(); 0", row))
      page$press("p", wait = 0)
      # The pin has its panel and the panel's width has a watcher: the counts that no other scenario moves.
      expect_true(page$wait_until("HovercraftTable.stats().panels === 1 && HovercraftTable.stats().observers === 1"))
      set_input("leaks_dtp_go", i)
      expect_true(page$wait_until(sprintf(
        "document.querySelector('#leaks_dtp tbody tr td:nth-child(2)')?.textContent === '%d'",
        i
      )))
      page$evaluate("window.Hovercraft.close(); 0")
      expect_true(page$wait_until("HovercraftTable.stats().panels === 0 && HovercraftTable.stats().observers === 0"))
    }
    cycle(1L)
    before <- snapshot()
    for (i in 2:(leak_draws + 1L)) {
      cycle(i)
    }
    expect_no_growth(before, snapshot())
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

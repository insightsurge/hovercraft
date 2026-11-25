skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if_not_installed("DT")
skip_if_not_installed("reactable")
skip_if_not_installed("bslib")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# What the page script makes and must let go of when Shiny draws or removes its host (#131): watchers
# (MutationObserver, ResizeObserver), event listeners, animation frames, timers, helper boxes (pin and More strips,
# parked outputs), duplicate ids and the table script's panels. Each scenario draws the same thing once, counts what is
# alive, draws it 30 more times and counts again: a count that grew is a leak.
#
# The counts come from a shim that the page loads before its own scripts (`leak_shim`): it wraps the browser's
# observers, `addEventListener`, `requestAnimationFrame` and the timers, and counts only what still matters: an observer
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

  const frames = new Set();
  const nativeFrame = window.requestAnimationFrame.bind(window);
  const nativeCancelFrame = window.cancelAnimationFrame.bind(window);
  window.requestAnimationFrame = (fn) => {
    const id = nativeFrame((time) => {
      frames.delete(id);
      fn(time);
    });
    frames.add(id);
    return id;
  };
  window.cancelAnimationFrame = (id) => {
    frames.delete(id);
    return nativeCancelFrame(id);
  };

  const timers = new Set();
  const nativeTimeout = window.setTimeout.bind(window);
  const nativeInterval = window.setInterval.bind(window);
  const nativeClear = window.clearTimeout.bind(window);
  window.setTimeout = (fn, ms, ...rest) => {
    const id = nativeTimeout(
      (...args) => {
        timers.delete(id);
        if (typeof fn === 'function') fn(...args);
      },
      ms,
      ...rest,
    );
    timers.add(id);
    return id;
  };
  window.setInterval = (fn, ms, ...rest) => {
    const id = nativeInterval(fn, ms, ...rest);
    timers.add(id);
    return id;
  };
  window.clearTimeout = (id) => {
    timers.delete(id);
    return nativeClear(id);
  };
  window.clearInterval = window.clearTimeout;

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
      frames: frames.size,
      timers: timers.size,
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
  app <- start_app(function() {
    long <- function() tip_more("The long form.", on = "button")
    card_ui <- function(id) {
      ns <- shiny::NS(id)
      tip_on(shiny::actionButton(ns("btn"), "Mod"), "A module's tip", id = ns("tip"), pin = TRUE, more = long())
    }
    card_server <- function(id) shiny::moduleServer(id, function(input, output, session) NULL)
    version <- function(input, name) input[[name]] %||% 0L
    rows <- function() {
      tip_spec("**{{ team }}**: {{ v }}", pin = TRUE, more = tip_more("More {{ team }}.", on = "button"))
    }

    ui <- bslib::page_fluid(
      use_hovercraft(max_pins = 2),
      shiny::uiOutput("ru"),
      shiny::div(id = "ins_slot"),
      shiny::div(id = "mod_slot"),
      shiny::uiOutput("pk"),
      shiny::textOutput("pk_text"),
      DT::DTOutput("dt"),
      reactable::reactableOutput("rt")
    )
    server <- function(input, output, session) {
      output$ru <- shiny::renderUI({
        n <- version(input, "ru_go")
        tip_on(
          shiny::actionButton("ru_btn", paste0("RU v", n)),
          paste("Render tip", n),
          id = "ru_tip",
          pin = TRUE,
          more = long()
        )
      })

      shiny::observeEvent(input$ins_go, {
        shiny::insertUI(
          "#ins_slot",
          ui = tip_on(
            shiny::actionButton("ins_btn", "Inserted"),
            "Inserted tip",
            id = "ins_tip",
            pin = TRUE,
            more = long()
          )
        )
      })
      shiny::observeEvent(input$ins_rm, shiny::removeUI("#ins_btn"))

      shiny::observeEvent(input$mod_go, {
        id <- paste0("m", input$mod_go)
        shiny::insertUI("#mod_slot", ui = shiny::div(id = paste0("w-", id), card_ui(id)))
        card_server(id)
      })
      shiny::observeEvent(input$mod_rm, shiny::removeUI(paste0("#w-m", input$mod_rm)))

      # An output that the tip holds, inside the region that draws again: the repro of the parked-output leak.
      output$pk_text <- shiny::renderText("The output the tip holds")
      output$pk <- shiny::renderUI({
        n <- version(input, "pk_go")
        if (isTRUE(input$pk_clear > 0)) {
          return(NULL)
        }
        shiny::tagList(
          tip_define(pk_t = tip_template(tip_text("Parked {{ n }}"), tip_output(shiny::textOutput("pk_out")))),
          tip_on(
            shiny::actionButton("pk_btn", paste0("PK v", n)),
            template = "pk_t",
            record = list(n = n),
            id = "pk_tip"
          )
        )
      })
      output$pk_out <- shiny::renderText("The output the tip holds")

      output$dt <- DT::renderDT(
        {
          n <- version(input, "dt_go")
          tip_table(
            DT::datatable(data.frame(team = c("Owls", "Foxes"), v = n), rownames = FALSE),
            rows = rows(),
            id = "dt"
          )
        },
        server = FALSE
      )
      output$rt <- reactable::renderReactable({
        n <- version(input, "rt_go")
        tip_table(reactable::reactable(data.frame(team = c("Owls", "Foxes"), v = n)), rows = rows(), id = "rt")
      })
    }
    shiny::shinyApp(ui, server)
  })
  page <- local_page(app$url, environment(), init = leak_shim)

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

  it("starts and connects", {
    expect_true(app$proc$is_alive())
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    expect_true(page$wait_until("document.getElementById('ru_btn') !== null"))
    expect_true(page$wait_until("document.querySelector('#dt td') !== null"))
    expect_true(page$wait_until("document.querySelector('#rt .rt-td') !== null"))
  })

  it("keeps the counts of a tagged item that renderUI() draws 30 times", {
    page$hover("#ru_btn")
    before <- snapshot()
    redraw("ru_go", "document.getElementById('ru_btn')?.textContent?.includes('RU v%d')", hover = "#ru_btn")
    expect_no_growth(before, snapshot())
  })

  it("keeps the counts of a tagged item that insertUI() adds and removeUI() takes away 30 times", {
    cycle <- function(i) {
      set_input("ins_go", i)
      expect_true(page$wait_until("document.getElementById('ins_btn') !== null"))
      set_input("ins_rm", i)
      expect_true(page$wait_until("document.getElementById('ins_btn') === null"))
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
      set_input("mod_go", i)
      expect_true(page$wait_until(sprintf("document.getElementById('m%d-btn') !== null", i)))
      set_input("mod_rm", i)
      expect_true(page$wait_until(sprintf("document.getElementById('m%d-btn') === null", i)))
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
    redraw("dt_go", "document.querySelector('#dt tbody tr td:nth-child(2)')?.textContent === '%d'")
    expect_no_growth(before, snapshot())
  })

  it("keeps the counts of a reactable table that renderReactable() draws 30 times", {
    before <- snapshot()
    redraw("rt_go", "document.querySelector('#rt .rt-tbody .rt-td:nth-child(2)')?.textContent === '%d'")
    expect_no_growth(before, snapshot())
  })

  it("keeps one parked output for a tip_output() whose region draws again, and none when it is cleared", {
    expect_true(page$wait_until("document.getElementById('pk_btn') !== null"))
    page$settle()
    before <- snapshot()
    redraw("pk_go", "document.getElementById('pk_btn')?.textContent?.includes('PK v%d')")
    expect_no_growth(before, snapshot())
    set_input("pk_clear", 1L)
    expect_true(page$wait_until("document.getElementById('pk_btn') === null"))
    cleared <- snapshot()
    expect_identical(unname(cleared[["parked"]]), 0L)
    expect_identical(page$evaluate("document.querySelectorAll('[id=\"pk_out\"]').length"), 0L)
  })

  it("leaves the console without errors", {
    expect_identical(page$problems(), character())
  })
})

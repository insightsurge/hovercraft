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
  app <- start_app(function() {
    long <- function() tip_more("The long form.", on = "button")
    card_ui <- function(id) {
      ns <- shiny::NS(id)
      tip_on(shiny::actionButton(ns("btn"), "Mod"), "A module's tip", id = ns("tip"), pin = TRUE, more = long())
    }
    card_server <- function(id) shiny::moduleServer(id, function(input, output, session) NULL)
    version <- function(input, name) input[[name]] %||% 0L
    # The words change with every draw, so a watcher left from an earlier draw would write the old words back.
    rows <- function(n = 0L) {
      tip_spec(
        paste0("v", n, " **{{ team }}**: {{ v }}"),
        pin = TRUE,
        more = tip_more("More {{ team }}.", on = "button")
      )
    }

    held_ui <- function(name, keep_alive) {
      tip_template_name <- paste0(name, "_t")
      shiny::tagList(
        do.call(
          tip_define,
          stats::setNames(
            list(tip_template(tip_text("Held {{ n }}"), tip_output(paste0(name, "_out"), keep_alive = keep_alive))),
            tip_template_name
          )
        ),
        tip_on(
          shiny::actionButton(paste0(name, "_btn"), toupper(name)),
          template = tip_template_name,
          record = list(n = 1),
          id = paste0(name, "_tip"),
          pin = TRUE
        ),
        shiny::uiOutput(name)
      )
    }

    ui <- bslib::page_fluid(
      use_hovercraft(max_pins = 2),
      shiny::uiOutput("ru"),
      shiny::div(id = "ins_slot"),
      shiny::div(id = "mod_slot"),
      shiny::uiOutput("pk"),
      shiny::textOutput("pk_text"),
      # Outputs that a region draws and a tip outside the region holds (the tip_define() is not in the region).
      held_ui("pkb", keep_alive = TRUE),
      held_ui("pkc", keep_alive = FALSE),
      held_ui("pkd", keep_alive = TRUE),
      DT::DTOutput("dtp"),
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
            id = "pk_tip",
            pin = TRUE
          )
        )
      })
      output$pk_out <- shiny::renderText("The output the tip holds")

      # A region that draws the output a tip holds (version in `data-v`, so a draw can be told), and that clears on request.
      region <- function(name) {
        output[[name]] <- shiny::renderUI({
          n <- version(input, paste0(name, "_go"))
          if (isTRUE(input[[paste0(name, "_clear")]] > 0)) {
            return(NULL)
          }
          shiny::tagAppendAttributes(shiny::textOutput(paste0(name, "_out")), `data-v` = n)
        })
        output[[paste0(name, "_out")]] <- shiny::renderText(paste("Answer", name))
      }
      for (name in c("pkb", "pkc", "pkd")) {
        region(name)
      }

      output$dtp <- DT::renderDT(
        {
          n <- version(input, "dtp_go")
          tip_table(
            DT::datatable(data.frame(team = c("Owls", "Foxes"), v = n), rownames = FALSE),
            rows = tip_spec("**{{ team }}**: {{ v }}", pin = TRUE, to = "in_place"),
            id = "dtp"
          )
        },
        server = FALSE
      )

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
        # `?rt=31` starts the table at the words of the 31st draw, for a fresh draw to compare with.
        n <- input$rt_go %||% as.integer(shiny::parseQueryString(session$clientData$url_search)$rt %||% 0L)
        tip_table(reactable::reactable(data.frame(team = c("Owls", "Foxes"), v = n)), rows = rows(n), id = "rt")
      })
    }
    shiny::shinyApp(ui, server)
  })
  page <- local_page(app$url, environment(), init = leak_shim)
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
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin:not([hidden])') === null"))
    page$settle()
  }
  held_stats <- function() unlist(jsonlite::fromJSON(page$evaluate("JSON.stringify(window.HovercraftShiny.stats())")))
  # What the first row of the reactable says its template is: a name made from the words, so it changes with them.
  row_template <- function(p) {
    p$evaluate(sprintf(
      "document.querySelector('#rt [class*=\"hovercraft-row-\"]')?.getAttribute('%s')",
      tip_attribute("template")
    ))
  }

  it("starts and connects", {
    expect_true(app$proc$is_alive())
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    expect_true(page$wait_until("document.getElementById('ru_btn') !== null"))
    expect_true(page$wait_until("document.querySelector('#dt td') !== null"))
    expect_true(page$wait_until("document.querySelector('#rt .rt-td') !== null"))
    expect_true(page$wait_until(sprintf(
      "document.querySelector('#rt [class*=\"hovercraft-row-\"]')?.hasAttribute('%s') === true",
      tip_attribute("template")
    )))
    first_template <<- row_template(page)
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

  it("leaves a reactable table quiet after it is drawn again with other words: no watcher writes the old ones back", {
    # Stale watchers on one container each re-mark the rows with their own draw's words, and each write wakes the others:
    # an endless exchange of attribute writes. A table drawn once is quiet, so the writes in 400 ms are few.
    page$evaluate(paste(
      "window.writes = 0;",
      "new MutationObserver((list) => { window.writes += list.length; }).observe(",
      "  document.getElementById('rt'), { subtree: true, childList: true, attributes: true, characterData: true }",
      "); 0"
    ))
    set_input("rt_go", leak_draws + 1L)
    expect_true(page$wait_until("document.querySelector('#rt .rt-tbody .rt-td:nth-child(2)')?.textContent === '31'"))
    page$settle(1)
    page$evaluate("window.writes = 0; 0")
    page$hold(0.4)
    expect_lt(page$evaluate("window.writes"), 5L)
    # The words are the last draw's, not the first's and not an earlier one's: the same as a table drawn once with them.
    fresh <- local_page(paste0(app$url, "?rt=31"), environment())
    expect_true(fresh$wait_until("document.querySelector('#rt [class*=\"hovercraft-row-\"]') !== null"))
    expect_true(fresh$wait_until(sprintf(
      "document.querySelector('#rt [class*=\"hovercraft-row-\"]')?.hasAttribute('%s') === true",
      tip_attribute("template")
    )))
    expect_identical(row_template(page), row_template(fresh))
    expect_false(identical(row_template(page), first_template))
  })

  it("keeps one parked output for a tip_output() whose region draws again, and none when it is cleared", {
    expect_true(page$wait_until("document.getElementById('pk_btn') !== null"))
    page$settle()
    before <- snapshot()
    redraw("pk_go", "document.getElementById('pk_btn')?.textContent?.includes('PK v%d')")
    expect_no_growth(before, snapshot())
    # The output is whole when the tip opens: once in the page, bound once, with its text.
    expect_identical(count_id("pk_out"), 1L)
    pin_item("pk_btn")
    expect_true(page$wait_until(
      "document.querySelector('.hc-tip-pin #pk_out')?.textContent === 'The output the tip holds'"
    ))
    expect_identical(count_id("pk_out"), 1L)
    expect_true(bound_once("pk_out"))
    close_tips()
    set_input("pk_clear", 1L)
    expect_true(page$wait_until("document.getElementById('pk_btn') === null"))
    page$settle()
    expect_identical(page$evaluate("document.querySelectorAll('.hc-output-park [id=\"pk_out\"]').length"), 0L)
    expect_identical(count_id("pk_out"), 0L)
  })

  it("parks the output once when only its region draws again, and its tip is defined outside the region", {
    expect_true(page$wait_until("document.getElementById('pkb_out') !== null"))
    redraw("pkb_go", "document.getElementById('pkb_out')?.dataset.v === '%d'")
    page$settle()
    expect_identical(count_id("pkb_out"), 1L)
    pin_item("pkb_btn")
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin #pkb_out')?.textContent === 'Answer pkb'"))
    expect_identical(count_id("pkb_out"), 1L)
    expect_true(bound_once("pkb_out"))
    close_tips()
    expect_identical(count_id("pkb_out"), 1L)
  })

  it("keeps one element with the id when the region draws again while its tip is open", {
    pin_item("pkb_btn")
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin #pkb_out') !== null"))
    set_input("pkb_go", 99L)
    expect_true(page$wait_until("document.getElementById('pkb_out')?.dataset.v === '99'"))
    expect_true(page$wait_until("document.querySelectorAll('[id=\"pkb_out\"]').length === 1"))
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin #pkb_out') !== null"))
    expect_true(bound_once("pkb_out"))
    close_tips()
    expect_identical(count_id("pkb_out"), 1L)
  })

  it("lets go of an output whose region is cleared while its tip is open, when the tip closes", {
    expect_true(page$wait_until("document.getElementById('pkd_out') !== null"))
    before <- held_stats()
    pin_item("pkd_btn")
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin #pkd_out') !== null"))
    set_input("pkd_clear", 1L)
    expect_true(page$wait_until(sprintf("window.HovercraftShiny.stats().marks === %d", before[["marks"]] - 1L)))
    close_tips()
    expect_identical(count_id("pkd_out"), 0L)
    expect_identical(page$evaluate("document.querySelectorAll('.hc-output-park [id=\"pkd_out\"]').length"), 0L)
    after <- held_stats()
    expect_identical(unname(after[["outputs"]]), unname(before[["outputs"]]))
    expect_identical(unname(after[["shells"]]), unname(before[["shells"]]) - 1L)
  })

  it("keeps nothing for an output with keep_alive = FALSE once its region is cleared after its tip closed", {
    expect_true(page$wait_until("document.getElementById('pkc_out') !== null"))
    pin_item("pkc_btn")
    expect_true(page$wait_until("document.querySelector('.hc-tip-pin #pkc_out') !== null"))
    close_tips()
    before <- held_stats()
    set_input("pkc_clear", 1L)
    expect_true(page$wait_until(sprintf("window.HovercraftShiny.stats().marks === %d", before[["marks"]] - 1L)))
    after <- held_stats()
    expect_identical(unname(after[["outputs"]]), unname(before[["outputs"]]) - 1L)
    expect_identical(unname(after[["shells"]]), unname(before[["shells"]]) - 1L)
    expect_identical(count_id("pkc_out"), 0L)
  })

  it("takes a panel and the watcher of its table back at each close, over 30 pins and redraws", {
    expect_true(page$wait_until("document.querySelector('#dtp tbody tr[data-hovercraft-tip-id]') !== null"))
    cycle <- function(i) {
      # As a keyboard reader does: the row takes focus (the table gives it a tab stop a frame after it is drawn), then P.
      row <- "#dtp tbody tr[data-hovercraft-tip-id]"
      expect_true(page$wait_until(sprintf("document.querySelector('%s')?.hasAttribute('tabindex') === true", row)))
      page$evaluate(sprintf("document.querySelector('%s').focus(); 0", row))
      page$press("p", wait = 0.2)
      # The pin has its panel and the panel's width has a watcher: the counts that no other scenario moves.
      expect_true(page$wait_until("HovercraftTable.stats().panels === 1 && HovercraftTable.stats().observers === 1"))
      set_input("dtp_go", i)
      expect_true(page$wait_until(sprintf(
        "document.querySelector('#dtp tbody tr td:nth-child(2)')?.textContent === '%d'",
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

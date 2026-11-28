# One headless Chrome per test process, shared by every browser test: each test opens its own tab (a ChromoteSession) in
# it. Starting a Chrome for every test was the main cause of flaky runs: a slow start (a busy machine, a CI runner)
# failed the test with a chromote start-up timeout that passed when the test ran alone. testthat runs the test files in
# worker processes (`Config/testthat/parallel`), and each process loads this file, so each has its own Chrome, its own
# restart count and its own close at the end.
test_browser_env <- new.env(parent = emptyenv())
test_browser_env$restarts <- 0L

# How many times a run replaces a dead Chrome. A Chrome that keeps dying (memory, a broken install) would fail every
# later test with "Chromote has been closed."; after this many replacements the browser tests skip with a message.
chrome_restart_limit <- 2L

# Milliseconds any browser test waits for a signal (a DOM state, an input value, a page load) before it fails. One
# generous limit for every wait: a wait that passes returns as soon as its signal arrives, so a long limit costs nothing
# on a fast machine. Never sleep for a fixed time instead: wait for the signal.
browser_timeout <- 30000

# The limit, in seconds, of every wait. Only a test of the helpers' own failure paths shortens it, with
# `withr::local_options(hovercraft.browser_wait = 1)`.
browser_wait <- function() getOption("hovercraft.browser_wait", browser_timeout / 1000)

# The first line of every -browser.R file: skips the file on CRAN, without Chrome, or without a package the browser
# tests need (Shiny and the packages that run an app in the background), plus the `pkgs` this file uses (`"DT"`). A skip
# that is only this file's own (the test bench's folder) goes on the next line.
skip_if_no_browser <- function(pkgs = character()) {
  testthat::skip_on_cran()
  for (pkg in c("chromote", "shiny", "bslib", "callr", "httpuv", "pkgload", "withr", pkgs)) {
    testthat::skip_if_not_installed(pkg)
  }
  testthat::skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")
}

# The Chrome shared by the tests: the running one, or a new one when its process has ended. It skips the calling test
# first on CRAN or without Chrome, so no test starts a Chrome there.
test_browser <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("chromote")
  testthat::skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")
  browser <- test_browser_env$browser
  if (!is.null(browser)) {
    problem <- chrome_problem(browser)
    if (is.null(problem)) {
      return(browser)
    }
    chrome_count_restart(problem)
    chrome_close(browser)
  }
  test_browser_env$browser <- chrome_start()
}

# Why `browser` cannot be used (a sentence), or NULL when it can.
chrome_problem <- function(browser) {
  if (!browser$is_alive()) {
    return("its process has ended")
  }
  tryCatch(
    {
      browser$check_active()
      NULL
    },
    error = function(e) conditionMessage(e)
  )
}

# Count one replacement of the Chrome, or skip the calling test when the run has used its replacements up.
chrome_count_restart <- function(problem) {
  if (test_browser_env$restarts >= chrome_restart_limit) {
    testthat::skip(paste0(
      "The shared Chrome stopped (",
      problem,
      ") after ",
      chrome_restart_limit,
      " restarts in this run; the remaining browser tests are skipped."
    ))
  }
  test_browser_env$restarts <- test_browser_env$restarts + 1L
  cli::cli_alert_warning(
    "The shared Chrome stopped ({problem}); starting a new one (restart {test_browser_env$restarts} of {chrome_restart_limit})."
  )
  invisible()
}

# Start a Chrome and close it when the test process ends. Its profile is in tempdir(), so R removes it with the session
# (chromote's own keeps its profile in the user's Application Support folder), and so are Chrome's temporary files: it
# keeps them in TMPDIR and leaves folders behind, which R CMD check reports as a NOTE about the system temp directory.
# Starting Chrome can take longer than chromote's default 10 seconds on a slow runner, hence the longer timeout.
chrome_start <- function() {
  tmp <- file.path(tempdir(), "chrome-tmp")
  dir.create(tmp, showWarnings = FALSE)
  profile <- tempfile("hovercraft-chrome-")
  browser <- withr::with_options(
    list(chromote.timeout = 60),
    withr::with_envvar(
      c(TMPDIR = tmp),
      chromote::Chromote$new(
        browser = chromote::Chrome$new(
          args = c(chromote::default_chrome_args(), paste0("--user-data-dir=", profile))
        )
      )
    )
  )
  # Pages opened from this browser inherit the limit, in seconds, for navigation and page commands.
  browser$default_timeout <- browser_timeout / 1000
  withr::defer(chrome_close(browser), envir = testthat::teardown_env())
  browser
}

# Close a Chrome; safe when its process has gone, and an error here must not replace the error that ended the run.
chrome_close <- function(browser) {
  tryCatch(browser$close(), error = function(e) NULL)
  invisible()
}

# Polls `check()` every `poll` seconds until it is true on `holds` polls in a row, and says so; false when `seconds`
# pass first. The answer is what the polls saw, never a fresh look after the loop: a page that is true for one poll and
# changes before a last check would otherwise fail a test at once ("waited 0.1 s of 30 s", #128), and a condition that
# flickers true while the page is still changing would pass before the page has settled. The short pause is the polling
# interval, not a fixed wait.
wait_polls <- function(check, seconds, poll = 0.05, holds = 2L) {
  deadline <- Sys.time() + seconds
  run <- 0L
  repeat {
    run <- if (isTRUE(check())) run + 1L else 0L
    if (run >= holds) {
      return(TRUE)
    }
    if (Sys.time() >= deadline) {
      return(FALSE)
    }
    Sys.sleep(poll)
  }
}

# Opens a page (an HTML file or a URL) in a new tab of the shared Chrome and gives a small set of helpers. The tab is
# closed when the calling test ends, pass or fail.
local_page <- function(html_file, env = parent.frame(), init = NULL) {
  session <- chromote::ChromoteSession$new(parent = test_browser(), width = 900, height = 700)
  # Motion is on by default, whatever the machine says: the macOS and Windows CI runners ask for reduced motion, and
  # the engine honours it, so no tip would animate there. Tests of the reduced case set it with `emulate()`.
  session$Emulation$setEmulatedMedia(features = list(list(name = "prefers-reduced-motion", value = "no-preference")))
  # A close that fails (Chrome died meanwhile) must not hide the error that ended the test.
  withr::defer(
    tryCatch(if (session$parent$is_active()) session$close(), error = function(e) NULL),
    envir = env
  )
  # Leave the page first, so a page with a live connection (Shiny) is not cut off while its tab is closing.
  withr::defer(tryCatch(session$Page$navigate("about:blank", wait_ = TRUE), error = function(e) NULL), envir = env)

  problems <- character()
  session$Runtime$enable()
  session$Runtime$consoleAPICalled(callback = function(event) {
    if (event$type %in% c("error", "assert")) {
      problems <<- c(
        problems,
        paste(vapply(event$args, function(a) as.character(a$value %||% a$description), ""), collapse = " ")
      )
    }
  })
  session$Runtime$exceptionThrown(callback = function(event) {
    problems <<- c(problems, event$exceptionDetails$text)
  })
  # A script that runs in every document before the page's own, to catch what happens while it loads.
  if (!is.null(init)) {
    session$Page$enable()
    session$Page$addScriptToEvaluateOnNewDocument(source = init)
  }
  session$Page$navigate(if (grepl("^https?://", html_file)) html_file else paste0("file://", normalizePath(html_file)))

  evaluate <- function(js) session$Runtime$evaluate(js, returnByValue = TRUE)$result$value
  # Waits until the JavaScript `js` is true in the page on two polls in a row; false when `seconds` (the shared limit by
  # default) pass first. See `wait_polls()`.
  wait_until <- function(js, seconds = browser_wait()) {
    wait_polls(function() evaluate(js), seconds)
  }
  # The `until =` of an action: waits until the JavaScript `until` is true in the page and fails the test, naming the
  # action and the expression, when it never is. A test that acts and then reads must wait for the change it caused:
  # `settle()` is not that signal, it only says nothing changed for a moment, and a change that starts late (a Shiny
  # round trip, the engine's timer) is not yet under way when it looks.
  wait_after <- function(action, until) {
    if (!wait_until(until)) {
      cli::cli_abort("{action}: {until} did not hold within {browser_wait()} s.")
    }
    invisible()
  }
  # Waits until the page has gone quiet: nothing in the DOM changed for `quiet` seconds, Shiny is not busy and no
  # transition or animation that has an end is running, and no DT table is loading and the window has stopped scrolling.
  # Use it where a test acted on the page (a redraw, a scroll, a re-sort) and must not read it before the change has
  # landed.
  settle <- function(quiet = 0.25) {
    js <- sprintf(
      "new Promise((resolve) => {
        let timer;
        const busy = () => document.documentElement.classList.contains('shiny-busy') ||
          [...document.querySelectorAll('.dataTables_processing, .dt-processing')].some((n) => getComputedStyle(n).display !== 'none') ||
          document.getAnimations().some((a) => a.playState === 'running' && a.effect.getComputedTiming().iterations !== Infinity);
        let where = window.scrollX + ',' + window.scrollY;
        const check = () => {
          // A smooth scroll changes neither the DOM nor an animation: the position tells.
          const now = window.scrollX + ',' + window.scrollY;
          if (busy() || now !== where) { where = now; arm(); } else { observer.disconnect(); resolve(true); }
        };
        const arm = () => { clearTimeout(timer); timer = setTimeout(check, %d); };
        const observer = new MutationObserver(arm);
        observer.observe(document, { subtree: true, childList: true, attributes: true, characterData: true });
        arm();
      })",
      as.integer(quiet * 1000)
    )
    invisible(
      session$Runtime$evaluate(
        js,
        awaitPromise = TRUE,
        returnByValue = TRUE,
        timeout_ = browser_wait()
      )$result$value
    )
  }
  # Clicks the element `selector` names, found and clicked in one step in the page, as a person's click is. Use it for
  # anything Shiny can redraw: a click by pointer position takes two steps (read the box, then press there), and a
  # redraw between them lands the press on whatever moved in. A selector that matches nothing fails the test. `until` is
  # a JavaScript expression the test waits for after the click (the change the click causes); it fails when it never
  # holds.
  click_now <- function(selector, until = NULL) {
    found <- evaluate(sprintf(
      "(() => { const node = document.querySelector(%s); if (node) node.click(); return node !== null; })()",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    ))
    if (!isTRUE(found)) {
      cli::cli_abort("No element matches {.val {selector}}.")
    }
    if (!is.null(until)) {
      wait_after(sprintf("clicked %s", selector), until)
    }
    invisible()
  }
  # Waits until a tip is on show. Pinned tips count too.
  wait_open <- function() {
    wait_until(sprintf("document.querySelector('.%s:not([hidden])') !== null", tip_names()$css))
  }
  # Waits until no hint is on show (a pinned tip stays: it needs a click away or Escape).
  wait_closed <- function() {
    wait_until(sprintf("document.querySelector('.%1$s:not(.%1$s-pin):not([hidden])') === null", tip_names()$css))
  }
  # Waits until Shiny has connected to its server.
  wait_connected <- function() {
    wait_until("window.Shiny?.shinyapp?.isConnected?.() === true")
  }
  # Waits until the hint on show has its Pin button.
  wait_pin_button <- function() {
    wait_until("document.querySelector('.hovercraft-pin-strip-button') !== null")
  }
  # Waits until the text of the element with this id matches the regular expression `pattern`: an output of the app
  # under test that reports what the server heard.
  wait_text <- function(id, pattern) {
    wait_until(sprintf(
      "new RegExp(%s).test(document.getElementById(%s)?.textContent ?? '')",
      jsonlite::toJSON(pattern, auto_unbox = TRUE),
      jsonlite::toJSON(id, auto_unbox = TRUE)
    ))
  }
  # The app's `#seen` output holds a JSON object of what the server heard: `reported(name)` reads one of its fields,
  # `wait_reported(name, test)` waits until the JavaScript `test` is true of it (as `v`).
  reported <- function(name) {
    all <- jsonlite::fromJSON(evaluate("document.getElementById('seen').textContent"), simplifyVector = FALSE)
    all[[name]]
  }
  wait_reported <- function(name, test) {
    wait_until(sprintf(
      "(() => { try { const v = JSON.parse(document.getElementById('seen').textContent)[%s]; return !!(%s); } catch (e) { return false; } })()",
      jsonlite::toJSON(name, auto_unbox = TRUE),
      test
    ))
  }
  # Waits until a pinned tip is on show; `wait_unpinned()` until none is (a hint may still be on show).
  wait_pinned <- function() {
    wait_until(sprintf("document.querySelector('.%s-pin:not([hidden])') !== null", tip_names()$css))
  }
  wait_unpinned <- function() {
    wait_until(sprintf("document.querySelector('.%s-pin:not([hidden])') === null", tip_names()$css))
  }
  # Waits until the tips on show (hints and pins) have exactly these words, in page order: `wait_shown("Owls: 7 wins")`,
  # or `wait_shown()` for none. Use it where a test read `press()` or `click()`'s answer after a hold.
  wait_shown <- function(texts = character()) {
    wait_until(sprintf(
      "JSON.stringify([...document.querySelectorAll('.%s')].filter((t) => !t.hidden).map((t) => t.textContent)) === %s",
      tip_names()$css,
      jsonlite::toJSON(jsonlite::toJSON(as.character(texts)), auto_unbox = TRUE)
    ))
  }
  # Waits the given seconds. Only for what a signal cannot tell: that nothing happens (a tip that must stay shut, or
  # stay open) or that the engine's own timer, its open and close delay, has run out. Say in a comment what the wait is
  # for.
  hold <- function(seconds) {
    Sys.sleep(seconds)
    invisible()
  }
  # Where the pointer goes to rest on the element `selector` names. It waits (the shared limit) for the element to be in
  # the page, since a page can still be drawing it; a selector that still matches nothing fails the test with that said,
  # as `click_now()` does, not with a JSON error from reading an empty answer.
  box_of <- function(selector) {
    wait_until(
      sprintf("document.querySelector(%s) !== null", jsonlite::toJSON(selector, auto_unbox = TRUE)),
      browser_wait()
    )
    box <- evaluate(sprintf(
      "(() => { const n = document.querySelector(%s); if (!n) return null; const r = n.getBoundingClientRect(); return JSON.stringify([r.x + 10, r.y + r.height / 2]); })()",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    ))
    if (is.null(box)) {
      cli::cli_abort("No element matches {.val {selector}}.")
    }
    jsonlite::fromJSON(box)
  }
  # Moves the pointer onto the element, waits for the element's own hint to open and come to rest, then reads the tips
  # on show. A hint that never opens fails the test (after the shared limit) with the selector and the condition that
  # failed named; the hint of the tip the pointer left does not count. Pass `opens = FALSE` where no tip is expected to
  # open (a tag with no tip, a disabled tip, a check that nothing opens): the engine's open delay is held, and the tips
  # on show are read.
  hover <- function(selector, opens = TRUE) {
    box <- box_of(selector)
    hint_js <- sprintf("document.querySelector('.%1$s:not(.%1$s-pin):not([hidden])') !== null", tip_names()$css)
    session$Input$dispatchMouseEvent(type = "mouseMoved", x = 890, y = 690)
    # The engine's close delay, for the tip the pointer leaves: wait until no hint is on show, for the delay at most (a
    # hint that stays, such as one a focus holds open, gives no signal). This is the one wait with a limit of its own.
    wait_polls(function() !isTRUE(evaluate(hint_js)), 0.2, poll = 0.02, holds = 1L)
    session$Input$dispatchMouseEvent(type = "mouseMoved", x = box[[1]] - 2, y = box[[2]])
    session$Input$dispatchMouseEvent(type = "mouseMoved", x = box[[1]], y = box[[2]])
    if (opens) {
      # The engine marks the tipped element, or the one that holds it (a table row), with its open kind: the hint on
      # show is this element's own when that mark says "hint" on it or an element around it. Read from the element the
      # selector names (the one under the pointer can be another, such as a table's scrolling layer), or from the one
      # under the pointer.
      open_attribute <- paste0(tip_names()$attribute, "-open")
      own_js <- sprintf(
        paste0(
          "(() => { const own = (n) => n?.closest('[%2$s]')?.getAttribute('%2$s') === 'hint';",
          " return (own(document.querySelector(%1$s)) || own(document.elementFromPoint(%3$f, %4$f))) && %5$s; })()"
        ),
        jsonlite::toJSON(selector, auto_unbox = TRUE),
        open_attribute,
        box[[1]],
        box[[2]],
        hint_js
      )
      # And it has stopped moving: a test that measures the tip after `hover()` reads where it rests.
      still_js <- paste(
        "document.getAnimations().every((a) =>",
        "a.playState !== 'running' || a.effect.getComputedTiming().iterations === Infinity)"
      )
      seconds <- browser_wait()
      opened <- wait_polls(
        function() isTRUE(evaluate(own_js)) && isTRUE(evaluate(still_js)),
        seconds,
        poll = 0.02,
        holds = 2L
      )
      if (!opened) {
        why <- if (isTRUE(evaluate(own_js))) "its hint was still moving" else "no hint of its own showed"
        cli::cli_abort("hovered {selector}: no tip opened within {seconds} s ({why}).")
      }
    } else {
      # No tip is expected: nothing to wait for, so hold for the engine's open delay (what a tip would take to open).
      hold(0.6)
    }
    shown()
  }
  shown <- function() {
    js <- sprintf(
      "JSON.stringify([...document.querySelectorAll('.%s')].filter((t) => !t.hidden).map((t) => t.textContent))",
      tip_names()$css
    )
    jsonlite::fromJSON(evaluate(js))
  }
  # The page region `region = c(left, top, right, bottom)` in CSS pixels as a base64 PNG, for the page to read back.
  screenshot <- function(region) {
    clip <- list(
      x = region[[1]],
      y = region[[2]],
      width = region[[3]] - region[[1]],
      height = region[[4]] - region[[2]],
      scale = 2
    )
    session$Page$captureScreenshot(format = "png", clip = clip)$data
  }
  # Presses and releases a key the way a keyboard does (focus-visible follows), then holds `wait` seconds, the engine's
  # delay, and returns the tips on show. Keep the hold where the test checks that nothing happens. Where the key changes
  # something, pass `until`, a JavaScript expression the test waits for instead of holding (it fails when it never
  # holds), or `wait = 0` where the test goes on to wait for a signal itself (`wait_open()`, `wait_closed()`,
  # `wait_until()`).
  press <- function(key, ctrl = FALSE, wait = 0.6, until = NULL) {
    codes <- c(
      Tab = 9,
      Enter = 13,
      Escape = 27,
      " " = 32,
      End = 35,
      Home = 36,
      ArrowLeft = 37,
      ArrowUp = 38,
      ArrowRight = 39,
      ArrowDown = 40
    )
    args <- list(
      key = key,
      code = if (key == " ") {
        "Space"
      } else if (nchar(key) == 1L) {
        paste0("Key", toupper(key))
      } else {
        key
      },
      windowsVirtualKeyCode = if (key %in% names(codes)) unname(codes[[key]]) else utf8ToInt(toupper(key)),
      modifiers = if (ctrl) 2L else 0L
    )
    do.call(session$Input$dispatchKeyEvent, c(list(type = "rawKeyDown"), args))
    do.call(session$Input$dispatchKeyEvent, c(list(type = "keyUp"), args))
    # A key can open a tip, close one, move to another or do nothing: a test that expects a change waits for its signal
    # (`until`, or `wait = 0` and a wait of its own); one that expects nothing holds for the engine's delay.
    if (is.null(until)) {
      hold(wait)
    } else {
      wait_after(sprintf("pressed %s", key), until)
    }
    shown()
  }
  # Types text into the focused field, as a keyboard's text does: the page gets its input events.
  type <- function(text) {
    session$Input$insertText(text = text)
    Sys.sleep(0.2)
    invisible()
  }
  # Clicks the element with this id, found and clicked in the page: `click_now()` for an id (a button the test presses).
  click_id <- function(id, until = NULL) {
    click_now(paste0("#", id), until = until)
  }
  # The box of the element `selector` names, as a list of left, top, right, bottom, width and height in CSS pixels.
  rect <- function(selector) {
    jsonlite::fromJSON(evaluate(sprintf(
      "(() => { const r = document.querySelector(%s).getBoundingClientRect(); return JSON.stringify({ left: r.left, top: r.top, right: r.right, bottom: r.bottom, width: r.width, height: r.height }); })()",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    )))
  }
  # The middle of the element's box, as c(x, y).
  centre <- function(selector) {
    box <- rect(selector)
    c((box$left + box$right) / 2, (box$top + box$bottom) / 2)
  }
  # Scrolls the element to the middle of the window, waits until the page is quiet, then hovers it (`opens` as for
  # `hover()`).
  hover_in_view <- function(selector, opens = TRUE) {
    evaluate(sprintf(
      "document.querySelector(%s)?.scrollIntoView({ block: 'center', behavior: 'instant' }); 0",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    ))
    settle(0.1)
    hover(selector, opens = opens)
  }
  # Gives the element a tab stop (a table's keyboard layer does it a frame after drawing a row), focuses it and presses
  # the pin key. Where the test goes on to wait for the pin.
  pin_key <- function(selector) {
    json <- jsonlite::toJSON(selector, auto_unbox = TRUE)
    wait_until(sprintf("document.querySelector(%s)?.hasAttribute('tabindex') === true", json))
    evaluate(sprintf("document.querySelector(%s).focus()", json))
    press("p", wait = 0)
  }
  # Rests the pointer on the element (so its tip opens, `opens` as for `hover()`), then clicks it with the mouse, holds
  # `wait` seconds and returns the tips on show. Pass `until` or `wait = 0` where the click changes something, as for
  # `press()`.
  click <- function(selector, wait = 0.7, until = NULL, opens = TRUE) {
    hover(selector, opens = opens)
    box <- box_of(selector)
    for (type in c("mousePressed", "mouseReleased")) {
      session$Input$dispatchMouseEvent(type = type, x = box[[1]], y = box[[2]], button = "left", clickCount = 1)
    }
    # A click can pin, toggle, close or do nothing: see `press()` for `wait` and `until`.
    if (is.null(until)) {
      hold(wait)
    } else {
      wait_after(sprintf("clicked %s", selector), until)
    }
    shown()
  }
  # Emulates a media feature of the reader, such as `emulate("prefers-reduced-motion", "reduce")`.
  emulate <- function(feature, value) {
    session$Emulation$setEmulatedMedia(features = list(list(name = feature, value = value)))
    invisible()
  }
  # Gives the page this size in CSS pixels, such as `resize(375, 700)` for a phone; call it again to go back. It waits
  # for the size and then until the page is quiet (`settle()`), which is not a signal that the page has reacted.
  resize <- function(width, height) {
    viewport(width, height)
    settle()
    invisible()
  }
  # Moves the pointer to a point in the page and, with `click = TRUE`, presses and releases the left button there.
  mouse <- function(x, y, click = FALSE) {
    session$Input$dispatchMouseEvent(type = "mouseMoved", x = x, y = y)
    if (click) {
      for (type in c("mousePressed", "mouseReleased")) {
        session$Input$dispatchMouseEvent(type = type, x = x, y = y, button = "left", clickCount = 1)
      }
    }
    invisible()
  }
  # A finger tap at a point, with touch emulation on for the tap only: the page gets touch pointer events, then the
  # click.
  touch_tap <- function(x, y) {
    session$Emulation$setTouchEmulationEnabled(enabled = TRUE)
    session$Input$dispatchTouchEvent(type = "touchStart", touchPoints = list(list(x = x, y = y)))
    session$Input$dispatchTouchEvent(type = "touchEnd", touchPoints = list())
    invisible()
  }
  touch_off <- function() {
    session$Emulation$setTouchEmulationEnabled(enabled = FALSE)
    invisible()
  }
  # Sets the window's size, as a phone has it: `viewport(320, 640)`; `viewport(900, 700)` puts it back. It returns when
  # the page has that size; the page's own reaction (a tip placed again) is for the test to wait for.
  viewport <- function(width, height) {
    session$Emulation$setDeviceMetricsOverride(width = width, height = height, deviceScaleFactor = 1, mobile = FALSE)
    wait_after(
      sprintf("resized to %s x %s", width, height),
      sprintf("window.innerWidth === %d && window.innerHeight === %d", as.integer(width), as.integer(height))
    )
    invisible()
  }
  list(
    evaluate = evaluate,
    viewport = viewport,
    mouse = mouse,
    touch_tap = touch_tap,
    touch_off = touch_off,
    emulate = emulate,
    resize = resize,
    wait_until = wait_until,
    wait_after = wait_after,
    settle = settle,
    click_now = click_now,
    wait_open = wait_open,
    wait_closed = wait_closed,
    wait_shown = wait_shown,
    wait_pinned = wait_pinned,
    wait_unpinned = wait_unpinned,
    wait_connected = wait_connected,
    wait_pin_button = wait_pin_button,
    wait_text = wait_text,
    reported = reported,
    wait_reported = wait_reported,
    click_id = click_id,
    rect = rect,
    centre = centre,
    hover_in_view = hover_in_view,
    pin_key = pin_key,
    hold = hold,
    hover = hover,
    click = click,
    press = press,
    type = type,
    shown = shown,
    screenshot = screenshot,
    problems = function() problems,
    # The tab's chromote session, for input the helpers above do not cover (other mouse buttons, touch gestures).
    session = session
  )
}

# Writes `...` (HTML tags) into a page file in a temporary folder, to open with `local_page()`. The file goes when the
# calling test ends. `dir` is the body's text direction ("rtl").
page_file <- function(..., dir = NULL, .env = parent.frame()) {
  file <- withr::local_tempfile(fileext = ".html", .local_envir = .env)
  htmltools::save_html(
    htmltools::tags$html(htmltools::tags$head(), htmltools::tags$body(style = "min-height: 700px;", dir = dir, ...)),
    file
  )
  file
}

# Puts `x` at a fixed spot of a page made with `page_file()`: `place(x, "left: 40px; top: 40px;")`.
place <- function(x, style) {
  htmltools::div(style = paste("position: absolute;", style), x)
}

# Waits until the app in the background process `app` answers at `url`, for at most the shared limit; false when it
# stopped or did not answer in time. The caller stops the app when its test ends, pass or fail
# (`withr::defer(app$kill())` right after starting it), so a test that fails early does not leave its R process running
# until the whole suite ends.
wait_for_app <- function(app, url) {
  deadline <- Sys.time() + browser_timeout / 1000
  repeat {
    up <- !is.null(tryCatch(suppressWarnings(readLines(url, n = 1L)), error = function(e) NULL))
    if (up || !app$is_alive() || Sys.time() > deadline) {
      return(up)
    }
    Sys.sleep(0.1) # the polling interval while the app starts
  }
}

# Start a Shiny app made by `make()` (a function that gives the app, run in the background R) and wait until it answers.
start_app <- function(make, env = parent.frame(), ...) {
  environment(make) <- globalenv()
  path <- find.package("hovercraft")
  port <- httpuv::randomPort()
  proc <- callr::r_bg(
    function(make, port, path, ...) {
      if (dir.exists(file.path(path, "inst"))) {
        pkgload::load_all(path, quiet = TRUE)
      } else {
        library(hovercraft)
      }
      shiny::runApp(make(...), port = port, host = "127.0.0.1", launch.browser = FALSE)
    },
    args = list(make = make, port = port, path = path, ...),
    supervise = TRUE
  )
  withr::defer(proc$kill(), envir = env)
  url <- paste0("http://127.0.0.1:", port)
  if (!wait_for_app(proc, url)) {
    # Say why: a page that never connects tells nothing about an app that did not start (#175).
    output <- function(lines) if (length(lines) == 0L) "(nothing)" else paste(utils::tail(lines, 20L), collapse = "\n")
    err <- output(proc$read_error_lines())
    out <- output(proc$read_output_lines())
    cli::cli_abort(c(
      "The test app did not answer at {.url {url}}.",
      "i" = if (proc$is_alive()) "Its process is still running." else "Its process has ended.",
      "i" = "Its error output (the last lines):\n{err}",
      "i" = "Its output (the last lines):\n{out}"
    ))
  }
  list(url = url, proc = proc)
}

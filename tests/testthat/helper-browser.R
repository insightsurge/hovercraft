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

# Opens a page (an HTML file or a URL) in a new tab of the shared Chrome and gives a small set of helpers. The tab is
# closed when the calling test ends, pass or fail.
local_page <- function(html_file, env = parent.frame(), init = NULL) {
  session <- chromote::ChromoteSession$new(parent = test_browser(), width = 900, height = 700)
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
  # Waits until the JavaScript `js` is true in the page; false when `seconds` (the shared limit by default) pass first.
  # It returns at once when the condition already holds. The short pause is the polling interval, not a fixed wait.
  wait_until <- function(js, seconds = browser_timeout / 1000) {
    deadline <- Sys.time() + seconds
    while (!isTRUE(evaluate(js)) && Sys.time() < deadline) {
      Sys.sleep(0.05)
    }
    isTRUE(evaluate(js))
  }
  # Waits until the page has gone quiet: nothing in the DOM changed for `quiet` seconds, Shiny is not busy and no
  # transition or animation that has an end is running, and no DT table is loading and the window has stopped scrolling. Use it where a test acted on the page (a redraw, a scroll, a
  # re-sort) and must not read it before the change has landed.
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
        timeout_ = browser_timeout / 1000
      )$result$value
    )
  }
  # Clicks the element `selector` names, found and clicked in one step in the page, as a person's click is. Use it for
  # anything Shiny can redraw: a click by pointer position takes two steps (read the box, then press there), and a
  # redraw between them lands the press on whatever moved in. A selector that matches nothing fails the test.
  click_now <- function(selector) {
    found <- evaluate(sprintf(
      "(() => { const node = document.querySelector(%s); if (node) node.click(); return node !== null; })()",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    ))
    if (!isTRUE(found)) {
      cli::cli_abort("No element matches {.val {selector}}.")
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
  # Waits the given seconds. Only for what a signal cannot tell: that nothing happens (a tip that must stay shut, or
  # stay open) or that the engine's own timer, its open and close delay, has run out. Say in a comment what the wait is for.
  hold <- function(seconds) {
    Sys.sleep(seconds)
    invisible()
  }
  # Moves the pointer onto the element and rests there longer than the engine's delay, then reads the tips on show.
  hover <- function(selector) {
    box <- jsonlite::fromJSON(evaluate(sprintf(
      "(() => { const r = document.querySelector(%s).getBoundingClientRect(); return JSON.stringify([r.x + 10, r.y + r.height / 2]); })()",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    )))
    session$Input$dispatchMouseEvent(type = "mouseMoved", x = 890, y = 690)
    hold(0.2) # the engine's close delay, for the tip the pointer leaves
    session$Input$dispatchMouseEvent(type = "mouseMoved", x = box[[1]] - 2, y = box[[2]])
    session$Input$dispatchMouseEvent(type = "mouseMoved", x = box[[1]], y = box[[2]])
    hold(0.6) # the engine's open delay: a tag with no tip gives no signal either
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
  # Presses and releases a key the way a keyboard does (focus-visible follows), then waits past the engine's delay.
  press <- function(key, ctrl = FALSE, wait = 0.6) {
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
    hold(wait) # the engine's open and close delay, as for hover
    shown()
  }
  # Types text into the focused field, as a keyboard's text does: the page gets its input events.
  type <- function(text) {
    session$Input$insertText(text = text)
    Sys.sleep(0.2)
    invisible()
  }
  # Rests the pointer on the element (so its tip opens), then clicks it with the mouse; returns the tips on show.
  click <- function(selector) {
    hover(selector)
    box <- jsonlite::fromJSON(evaluate(sprintf(
      "(() => { const r = document.querySelector(%s).getBoundingClientRect(); return JSON.stringify([r.x + 10, r.y + r.height / 2]); })()",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    )))
    for (type in c("mousePressed", "mouseReleased")) {
      session$Input$dispatchMouseEvent(type = type, x = box[[1]], y = box[[2]], button = "left", clickCount = 1)
    }
    hold(0.7) # the engine's open and close delay, as for hover
    shown()
  }
  # Emulates a media feature of the reader, such as `emulate("prefers-reduced-motion", "reduce")`.
  emulate <- function(feature, value) {
    session$Emulation$setEmulatedMedia(features = list(list(name = feature, value = value)))
    invisible()
  }
  # Gives the page this size in CSS pixels, such as `resize(375, 700)` for a phone; call it again to go back.
  resize <- function(width, height) {
    session$Emulation$setDeviceMetricsOverride(width = width, height = height, deviceScaleFactor = 1, mobile = FALSE)
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
  # A finger tap at a point, with touch emulation on for the tap only: the page gets touch pointer events, then the click.
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
  # Sets the window's size, as a phone has it: `viewport(320, 640)`; `viewport(900, 700)` puts it back.
  viewport <- function(width, height) {
    session$Emulation$setDeviceMetricsOverride(width = width, height = height, deviceScaleFactor = 1, mobile = FALSE)
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
    settle = settle,
    click_now = click_now,
    wait_open = wait_open,
    wait_closed = wait_closed,
    hold = hold,
    hover = hover,
    click = click,
    press = press,
    type = type,
    shown = shown,
    screenshot = screenshot,
    problems = function() problems
  )
}

# Waits until the app in the background process `app` answers at `url`, for at most the shared limit; false when it
# stopped or did not answer in time. The caller stops the app when its test ends, pass or fail (`withr::defer(app$kill())`
# right after starting it), so a test that fails early does not leave its R process running until the whole suite ends.
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
  wait_for_app(proc, url)
  list(url = url, proc = proc)
}

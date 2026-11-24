# A headless Chrome whose profile is in tempdir(), so R removes it with the session (chromote's own keeps its profile
# in the user's Application Support folder). Opens an HTML file or a URL and gives a small set of helpers.
local_page <- function(html_file, env = parent.frame(), init = NULL) {
  old <- options(chromote.timeout = 60)
  withr::defer(options(old), envir = env)
  browser <- chromote::Chrome$new(
    args = c(chromote::default_chrome_args(), paste0("--user-data-dir=", tempfile("hovercraft-chrome-")))
  )
  withr::defer(browser$close(), envir = env)
  chrome <- chromote::Chromote$new(browser = browser)
  withr::defer(chrome$close(), envir = env)
  session <- chromote::ChromoteSession$new(parent = chrome, width = 900, height = 700)
  withr::defer(session$close(), envir = env)
  # Leave the page first, so a page with a live connection (Shiny) is not cut off while Chrome is closing.
  withr::defer(session$Page$navigate("about:blank", wait_ = TRUE), envir = env)

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
  wait_until <- function(js, seconds = 10) {
    deadline <- Sys.time() + seconds
    while (!isTRUE(evaluate(js)) && Sys.time() < deadline) {
      Sys.sleep(0.1)
    }
    isTRUE(evaluate(js))
  }
  # Moves the pointer onto the element and rests there longer than the engine's delay, then reads the tips on show.
  hover <- function(selector) {
    box <- jsonlite::fromJSON(evaluate(sprintf(
      "(() => { const r = document.querySelector(%s).getBoundingClientRect(); return JSON.stringify([r.x + 10, r.y + r.height / 2]); })()",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    )))
    session$Input$dispatchMouseEvent(type = "mouseMoved", x = 890, y = 690)
    Sys.sleep(0.2)
    session$Input$dispatchMouseEvent(type = "mouseMoved", x = box[[1]] - 2, y = box[[2]])
    session$Input$dispatchMouseEvent(type = "mouseMoved", x = box[[1]], y = box[[2]])
    Sys.sleep(0.6)
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
    Sys.sleep(wait)
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
    Sys.sleep(0.7)
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
    Sys.sleep(0.3)
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
    hover = hover,
    click = click,
    press = press,
    type = type,
    shown = shown,
    screenshot = screenshot,
    problems = function() problems
  )
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
  for (i in 1:100) {
    up <- !is.null(tryCatch(suppressWarnings(readLines(url, n = 1L)), error = function(e) NULL))
    if (up || !proc$is_alive()) {
      break
    }
    Sys.sleep(0.1)
  }
  list(url = url, proc = proc)
}

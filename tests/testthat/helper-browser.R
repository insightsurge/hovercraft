# A headless Chrome whose profile is in tempdir(), so R removes it with the session (chromote's own keeps its profile
# in the user's Application Support folder). Opens an HTML file or a URL and gives a small set of helpers.
local_page <- function(html_file, env = parent.frame()) {
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
  list(
    evaluate = evaluate,
    wait_until = wait_until,
    hover = hover,
    shown = shown,
    screenshot = screenshot,
    problems = function() problems
  )
}

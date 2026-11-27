# The browser test helper's own rules (`helper-browser.R`), tested without starting a Chrome.
describe("the shared Chrome's restart limit", {
  it("counts a replacement and warns, until the run has used its replacements up", {
    withr::defer(test_browser_env$restarts <- 0L)
    test_browser_env$restarts <- 0L
    for (i in seq_len(chrome_restart_limit)) {
      expect_no_error(suppressMessages(chrome_count_restart("its process has ended")))
    }
    expect_identical(test_browser_env$restarts, chrome_restart_limit)
    skipped <- tryCatch(chrome_count_restart("its process has ended"), skip = function(cond) cond)
    expect_s3_class(skipped, "skip")
    expect_match(conditionMessage(skipped), "remaining browser tests are skipped")
    expect_identical(test_browser_env$restarts, chrome_restart_limit)
  })
})

describe("wait_for_app()", {
  it("answers true once the address answers", {
    page <- withr::local_tempfile(fileext = ".html")
    writeLines("<p>up</p>", page)
    app <- list(is_alive = function() TRUE)
    expect_true(wait_for_app(app, paste0("file://", page)))
  })

  it("answers false at once when the app has stopped", {
    app <- list(is_alive = function() FALSE)
    expect_false(wait_for_app(app, paste0("file://", withr::local_tempfile())))
  })
})

# The wait that every browser test uses (#128): it answers from what its polls saw.
describe("wait_polls()", {
  # A check that answers from a script, then repeats its last answer; `calls()` says how often it was asked.
  scripted <- function(answers) {
    n <- 0L
    list(
      check = function() {
        n <<- n + 1L
        answers[[min(n, length(answers))]]
      },
      calls = function() n
    )
  }

  it("answers true for a condition that holds, after two polls", {
    page <- scripted(list(TRUE))
    expect_true(wait_polls(page$check, seconds = 5, poll = 0.01))
    expect_identical(page$calls(), 2L)
  })

  it("does not answer on a poll that was true once and then changed: it waits for two in a row", {
    # True, false, then true for good: one true poll is not enough. The old wait left its loop on the first true poll,
    # looked once more, saw false and answered false at once.
    page <- scripted(list(TRUE, FALSE, TRUE, TRUE))
    expect_true(wait_polls(page$check, seconds = 5, poll = 0.01))
    expect_identical(page$calls(), 4L)
  })

  it("answers false at the limit for a condition that is true for one poll and never again", {
    page <- scripted(list(TRUE, FALSE))
    started <- Sys.time()
    expect_false(wait_polls(page$check, seconds = 0.3, poll = 0.01))
    expect_gte(as.numeric(Sys.time() - started, units = "secs"), 0.25)
  })

  it("answers false at the limit for a condition that never holds, and treats an error value as false", {
    expect_false(wait_polls(function() NULL, seconds = 0.1, poll = 0.01))
    expect_false(wait_polls(function() NA, seconds = 0.1, poll = 0.01))
  })

  it("does not look again after the limit: what the last polls saw is the answer", {
    page <- scripted(list(FALSE, FALSE, TRUE))
    # The limit passes on the third poll, which is true once: not enough, and no extra look turns it into an answer.
    expect_false(wait_polls(page$check, seconds = 0.02, poll = 0.015))
    expect_lte(page$calls(), 4L)
  })
})

describe("skip_if_no_browser()", {
  it("skips a file that needs a package which is not installed, and names it", {
    withr::local_envvar(NOT_CRAN = "true")
    skip_if_not_installed("chromote")
    skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")
    skipped <- tryCatch(skip_if_no_browser("notapackage.hovercraft"), skip = function(cond) cond)
    expect_s3_class(skipped, "skip")
    expect_match(conditionMessage(skipped), "notapackage.hovercraft", fixed = TRUE)
  })

  it("skips on CRAN", {
    withr::local_envvar(NOT_CRAN = "false")
    expect_s3_class(tryCatch(skip_if_no_browser(), skip = function(cond) cond), "skip")
  })
})

describe("page_file() and place()", {
  it("write a page with the tags at a fixed spot", {
    file <- page_file(place(htmltools::tags$button("Go", id = "go"), "left: 5px; top: 6px;"), dir = "rtl")
    expect_true(file.exists(file))
    html <- paste(readLines(file), collapse = "\n")
    expect_match(html, "position: absolute; left: 5px; top: 6px;", fixed = TRUE)
    expect_match(html, "dir=\"rtl\"", fixed = TRUE)
  })
})

describe("the test bench's helpers", {
  it("find the bench beside the tests, and skip where it is not", {
    expect_true(dir.exists(bench_dir()))
    expect_true(file.exists(file.path(bench_dir(), "app.R")))
    skipped <- tryCatch(skip_if_no_bench(file.path(tempdir(), "no-bench-here")), skip = function(cond) cond)
    expect_s3_class(skipped, "skip")
  })

  it("write the bench's address with one tab and other settings", {
    bench <- list(url = "http://127.0.0.1:1")
    expect_identical(bench_url(bench), "http://127.0.0.1:1")
    expect_identical(bench_url(bench, "js"), "http://127.0.0.1:1/?only=js")
    expect_identical(bench_url(bench, "js", list(max_pins = 2)), "http://127.0.0.1:1/?only=js&max_pins=2")
  })

  it("read log lines with nested details as they are", {
    rows <- bench_log_rows('[{"n":1,"kind":"tip","id":"a","detail":{"state":"pinned","ids":["a"]}}]')
    expect_identical(rows[[1]]$detail$state, "pinned")
    expect_identical(rows[[1]]$detail$ids, list("a"))
  })
})

# The rule that keeps a browser test from reading the page before the change it caused has landed (#175): an action that
# changes something (a click, a key, a scroll, a resize, a focus) is not followed by a measurement unless the test waits
# for a signal first. Read as text, so it runs without a browser.
describe("the waits in the browser test files", {
  # The lines of `lines` where such an action is followed, within three lines (skipping blank lines, comments and
  # `settle()`, which says the page is quiet and not that the change happened), by a measurement with no wait between
  # and no `until =`, `$hold()` or a wait of more than 0 seconds on the action itself: as `file:line`.
  unwaited_actions <- function(lines, file = "") {
    code <- sub("(^|\\s)#.*$", "", lines)
    action <- paste0(
      "press_button\\(|\\$mouse\\([^)]*(click = TRUE|, TRUE\\))|mouse_at\\([^)]*click = TRUE|",
      "\\$click\\(|\\$click_id\\(|\\$click_now\\(|\\$press\\(|\\$resize\\(|\\$viewport\\(|\\$evaluate\\(.*(\\.(click|focus|draw)\\(|",
      "scrollTo|scrollBy|scrollIntoView|scrollLeft =|setInputValue)"
    )
    waited <- "wait_|\\$log_has\\(|\\$hover\\(|\\$hold\\(|until = |wait = (0\\.[1-9]|[1-9])"
    # What reads the page's state.
    measure <- paste0(
      "box_of\\(|\\$rect\\(|\\$centre\\(|\\$reported\\(|\\$shown\\(|getBoundingClientRect|\\$evaluate\\(|",
      "expect_(true|null|equal|identical|lt|gt|lte|gte|match|no_match|false|length)\\("
    )
    # A line that is itself an action (an `evaluate()` that blurs or clicks), that only reads where the focus is (a
    # focus lands at once) or the console, or that defines a function, does not count.
    not_measure <- paste0(
      "<- function|problems\\(|activeElement|Hovercraft\\.|dispatchEvent|setInputValue|bench_close_tips\\(|",
      "\\.(click|focus|blur|draw)\\(|scrollTo|scrollBy|scrollIntoView"
    )
    found <- integer()
    for (i in seq_along(code)) {
      if (!grepl(action, code[[i]]) || grepl("<- function", code[[i]], fixed = TRUE) || grepl(waited, code[[i]])) {
        next
      }
      for (j in seq_len(min(3L, length(code) - i)) + i) {
        # A call that goes on over the next lines is read as one.
        next_line <- trimws(code[[j]])
        if (grepl("\\($", next_line)) {
          next_line <- trimws(paste(code[j:min(j + 2L, length(code))], collapse = " "))
        }
        if (!nzchar(next_line) || grepl("^\\w+\\$settle\\(", next_line)) {
          next
        }
        if (!grepl(waited, next_line) && grepl(measure, next_line) && !grepl(not_measure, next_line)) {
          found <- c(found, i)
        }
        break
      }
    }
    if (length(found) == 0L) character() else paste0(file, ":", found)
  }

  it("flags an action that is read before its change has landed, and not one that waits", {
    expect_identical(unwaited_actions(c("page$click_id(\"go\")", "expect_equal(box_of()$top, 1)"), "f.R"), "f.R:1")
    expect_identical(unwaited_actions(c("page$press(\"Escape\", wait = 0)", "x <- page$rect(\"#a\")")), ":1")
    expect_identical(
      unwaited_actions(c("page$resize(375, 700)", "page$settle()", "# why", "x <- page$rect(\"#a\")")),
      ":1"
    )
    expect_identical(
      unwaited_actions(c("page$click(\"#a\", until = \"x\")", "expect_equal(box_of()$top, 1)")),
      character()
    )
    expect_identical(
      unwaited_actions(c("page$press(\"p\", wait = 0.6) # nothing", "expect_equal(box_of()$top, 1)")),
      character()
    )
    expect_identical(
      unwaited_actions(c("page$press(\"m\", wait = 0)", "expect_true(page$wait_until(\"x\"))")),
      character()
    )
    expect_identical(
      unwaited_actions(c("page$press(\"Escape\", wait = 0)", "page$evaluate(\"document.activeElement.blur()\")")),
      character()
    )
    expect_identical(
      unwaited_actions(c("press_button <- function() {", "  page$mouse(1, 2, click = TRUE)", "  page$settle()", "}")),
      character()
    )
    # One case for each kind of action and measure the rule knows.
    flagged <- function(action, measure = "expect_equal(box_of()$top, 1)") {
      identical(unwaited_actions(c(action, measure)), ":1")
    }
    expect_true(flagged("page$evaluate(\"$('#t').DataTable().draw(); 0\")"))
    expect_true(flagged("page$evaluate(\"Shiny.setInputValue('a', 1)\")"))
    expect_true(flagged("page$mouse(10, 20, TRUE)"))
    expect_true(flagged("mouse_at(page, 10, 20, click = TRUE)"))
    expect_true(flagged("off$press(\"p\")"))
    expect_true(flagged("page$press(\"p\")", "expect_true(page$evaluate(\"x\"))"))
    expect_true(flagged("page$press(\"p\")", "expect_null(read(\"seen\"))"))
    expect_true(flagged("page$press(\"p\")", "x <- page$shown()"))
    expect_identical(unwaited_actions(c("page$press(\"p\")", "off$settle()", "expect_equal(box_of()$top, 1)")), ":1")
    expect_false(flagged("page$press(\"p\")", "page$evaluate(\"window.scrollBy(0, 5)\")"))
    expect_true(flagged("page$press(\"p\")", "expect_equal(got$scroll, 1)"))
  })

  it("has no action that is read before its change has landed", {
    files <- list.files(testthat::test_path(), "^test-.*-browser\\.R$", full.names = TRUE)
    skip_if(length(files) == 0L, "the test files are not here")
    found <- as.character(unlist(lapply(files, function(file) {
      unwaited_actions(readLines(file, warn = FALSE), basename(file))
    })))
    expect_identical(
      found,
      character(),
      info = paste(
        "An action is followed by a measurement with no wait between. Pass `until =` (a JavaScript expression for the",
        "change), wait for a signal, or hold on purpose (`wait = 0.6` or `$hold()`, with a comment) where nothing",
        "should happen:\n",
        paste(found, collapse = "\n")
      )
    )
  })
})

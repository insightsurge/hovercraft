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

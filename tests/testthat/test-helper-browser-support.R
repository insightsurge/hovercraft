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

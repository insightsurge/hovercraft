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

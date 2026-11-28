skip_if_no_browser()

# The failure paths of the browser helpers (#175): a wait that never gets its signal fails the test and says which wait
# it was, and `hover()` does not take the hint of the tip the pointer has left for its own. The limit is shortened here
# so that a wait that must fail fails in a second.
describe("the browser helpers' waits", {
  page <- local_page(page_file(
    place(tip_on(htmltools::tags$button("Tipped", id = "tipped"), "A tip"), "left: 40px; top: 40px;"),
    place(htmltools::tags$button("Plain", id = "plain"), "left: 40px; top: 200px;")
  ))

  it("starts", {
    expect_true(page$wait_until("document.getElementById('plain') !== null"))
  })

  it("fails a hover on a tag with no tip, naming the selector, unless no tip is expected", {
    withr::local_options(hovercraft.browser_wait = 1)
    expect_error(
      page$hover("#plain"),
      "hovered #plain: no tip opened within 1 s (no hint of its own showed)",
      fixed = TRUE
    )
    expect_length(page$hover("#plain", opens = FALSE), 0L)
  })

  it("does not take the hint of the tip the pointer has left for the hint of the tag it is on now", {
    withr::local_options(hovercraft.browser_wait = 1)
    expect_identical(page$hover("#tipped"), "A tip")
    expect_error(page$hover("#plain"), "hovered #plain: no tip opened", fixed = TRUE)
  })

  it("fails an action whose change never comes, naming the action and the expression", {
    withr::local_options(hovercraft.browser_wait = 1)
    expect_error(page$press("a", until = "false"), "pressed a: false did not hold within 1 s", fixed = TRUE)
    expect_error(page$click_id("plain", until = "false"), "clicked #plain: false did not hold within 1 s", fixed = TRUE)
    expect_length(page$press("a", until = "true"), 0L)
  })
})

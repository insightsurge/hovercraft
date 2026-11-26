# Helpers for the browser tests of the bench's behaviour scenarios (test-bench-<tab>-browser.R): bring a spot into
# view, read the tips on show and where they sit, and check that a scenario leaves the page clean (HC-077).

# Scrolls the element `selector` names to the middle of the window and waits until the page is quiet.
bench_scroll_to <- function(page, selector) {
  page$evaluate(sprintf(
    "document.querySelector(%s).scrollIntoView({ block: 'center', behavior: 'instant' }); 0",
    jsonlite::toJSON(selector, auto_unbox = TRUE)
  ))
  page$settle()
  invisible()
}

# The box of the element `selector` names, as a list of left, top, right and bottom in CSS pixels.
bench_rect <- function(page, selector) {
  jsonlite::fromJSON(page$evaluate(sprintf(
    "(() => { const r = document.querySelector(%s).getBoundingClientRect();
      return JSON.stringify({ left: r.left, top: r.top, right: r.right, bottom: r.bottom }); })()",
    jsonlite::toJSON(selector, auto_unbox = TRUE)
  )))
}

# Every tip on show, as a data frame: its words, whether it is pinned, its side (the engine's `data-side`, "sheet" for
# a bottom sheet), its box, whether it is drawn (`visibility`), and whether it is the topmost thing at its own centre
# (so nothing, a backdrop or a Top Layer included, covers it).
bench_tips <- function(page) {
  js <- sprintf(
    "JSON.stringify([...document.querySelectorAll('.%1$s')].filter((t) => !t.hidden).map((t) => {
      const r = t.getBoundingClientRect();
      const at = document.elementFromPoint(r.left + r.width / 2, r.top + Math.min(r.height / 2, 12));
      return { text: t.textContent.replace(/^\\u00d7/, ''), pinned: t.classList.contains('%1$s-pin'),
        side: t.dataset.side ?? '', left: r.left, top: r.top, right: r.right, bottom: r.bottom,
        drawn: getComputedStyle(t).visibility === 'visible', on_top: t.contains(at) };
    }))",
    tip_names()$css
  )
  tips <- jsonlite::fromJSON(page$evaluate(js))
  if (length(tips) == 0L) {
    return(data.frame(text = character(), pinned = logical(), side = character(), drawn = logical()))
  }
  tips
}

# Waits until exactly the tips with these words are on show (pinned or not), in any order.
bench_wait_tips <- function(page, texts) {
  page$wait_until(sprintf(
    "(() => { const want = %s.slice().sort();
      const got = [...document.querySelectorAll('.%s')].filter((t) => !t.hidden)
        .map((t) => t.textContent.replace(/^\\u00d7/, '')).sort();
      return JSON.stringify(got) === JSON.stringify(want); })()",
    jsonlite::toJSON(as.character(texts)),
    tip_names()$css
  ))
}

# The tip events the bench logged after line `since`, as "state id reason" strings ("pinned a_tip", "closed a_tip
# replaced"), to read what the engine said happened.
bench_tip_events <- function(page, since = 0) {
  rows <- Filter(function(row) row$kind %in% c("hint", "tip"), page$log(since))
  vapply(
    rows,
    function(row) {
      reason <- row$detail$reason
      trimws(paste(row$detail$state, row$id, if (is.character(reason)) reason else ""))
    },
    ""
  )
}

# Presses Escape until no tip is on show, as a reader closes them one by one (closing a pin hands the focus back to its
# item, which may show that item's hint again), a few times at most; true once none is on show.
bench_close_tips <- function(page) {
  css <- tip_names()$css
  open <- sprintf("document.querySelector('.%s:not([hidden])') !== null", css)
  for (i in 1:4) {
    if (!isTRUE(page$evaluate(open))) {
      break
    }
    page$press("Escape")
  }
  page$wait_until(sprintf("document.querySelector('.%s:not([hidden])') === null", css))
}

# HC-077, the end of every scenario: close what is open with Escape (as a reader would), then expect no tip, pin or
# sheet on show, no `inert` or locked scroll left on the body, and nothing on the console. Errors come from the browser
# itself (`page$problems()`) and from the page's own log, which also hears `console.error` called before the test's
# listener.
expect_bench_clean <- function(page) {
  expect_true(bench_close_tips(page))
  body <- jsonlite::fromJSON(page$evaluate(
    "JSON.stringify({ inert: document.body.inert || document.body.hasAttribute('inert'),
      overflow: document.body.style.overflow })"
  ))
  expect_false(body$inert)
  expect_identical(body$overflow, "")
  expect_identical(page$problems(), character())
  errors <- Filter(function(row) identical(row$kind, "error"), page$log(0))
  expect_length(errors, 0L)
}

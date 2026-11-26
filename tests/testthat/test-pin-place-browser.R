skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("withr")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")

# Where a pinned tip is drawn (engine 0.59.0): a tip pinned from its hover hint stays where the hint was (hovercraft #135),
# and two pinned tips that would cover each other are drawn apart (hovercraft #138).
describe("where a pinned tip is drawn", {
  page_file <- function(..., .env = parent.frame()) {
    file <- withr::local_tempfile(fileext = ".html", .local_envir = .env)
    htmltools::save_html(
      htmltools::tags$html(htmltools::tags$head(), htmltools::tags$body(style = "min-height: 700px;", ...)),
      file
    )
    file
  }
  place <- function(x, style) htmltools::div(style = paste("position: absolute;", style), x)
  open_boxes <- "[...document.querySelectorAll('.hc-tip-pin:not([hidden])')]"
  rects <- function(page) {
    jsonlite::fromJSON(
      page$evaluate(sprintf(
        "JSON.stringify(%s.map((b) => { const r = b.getBoundingClientRect(); return { left: r.left, top: r.top, right: r.right, bottom: r.bottom, width: r.width, text: b.textContent }; }))",
        open_boxes
      ))
    )
  }

  it("keeps a tip pinned from its hint where the hint was, with the long form on the pin", {
    page <- local_page(
      page_file(
        use_hovercraft(),
        place(
          tip_on(
            htmltools::tags$button("Pin me", id = "pm"),
            "Short",
            more = tip_more(
              "The long form, with enough words in it that the pinned box is wider than the hint was.",
              on = "pin"
            ),
            id = "pm_tip",
            pin = TRUE,
            placement = "bottom"
          ),
          "left: 300px; top: 80px;"
        )
      ),
      environment()
    )
    expect_true(page$wait_until("window.Hovercraft !== undefined"))
    page$hover("#pm")
    expect_true(page$wait_until("document.querySelectorAll('.hc-tip:not(.hc-tip-pin):not([hidden])').length === 1"))
    hint <- jsonlite::fromJSON(page$evaluate(
      "(() => { const r = document.querySelector('.hc-tip:not(.hc-tip-pin):not([hidden])').getBoundingClientRect(); return JSON.stringify({ left: r.left, top: r.top, width: r.width }); })()"
    ))
    page$evaluate("window.Hovercraft.pin(document.getElementById('pm'), { by: 'reader', via: 'key' }); 0")
    expect_true(page$wait_until(sprintf("%s.some((b) => b.textContent.includes('long form'))", open_boxes)))
    pinned <- rects(page)
    expect_identical(nrow(pinned), 1L)
    # The same place: the box starts at the hint's left edge and top, and only grows away from it.
    expect_lte(abs(pinned$left - hint$left), 1, label = paste("pinned left", pinned$left, "hint left", hint$left))
    expect_lte(abs(pinned$top - hint$top), 1, label = paste("pinned top", pinned$top, "hint top", hint$top))
    expect_gte(pinned$width, hint$width - 1)
    page$evaluate("window.Hovercraft.close(); 0")
  })

  it("draws two pinned tips that would cover each other apart, each still beside its own item", {
    page <- local_page(
      page_file(
        use_hovercraft(max_pins = 3),
        place(
          tip_on(
            htmltools::tags$button("One", id = "one", style = "width: 70px;"),
            "Summary statistics for cohort alpha",
            pin = TRUE,
            placement = "bottom"
          ),
          "left: 240px; top: 300px;"
        ),
        place(
          tip_on(
            htmltools::tags$button("Two", id = "two", style = "width: 70px;"),
            "Summary statistics for cohort beta",
            pin = TRUE,
            placement = "bottom"
          ),
          "left: 360px; top: 300px;"
        )
      ),
      environment()
    )
    expect_true(page$wait_until("window.Hovercraft !== undefined"))
    for (id in c("one", "two")) {
      page$evaluate(sprintf(
        "window.Hovercraft.pin(document.getElementById('%s'), { by: 'reader', via: 'key' }); 0",
        id
      ))
    }
    expect_true(page$wait_until(sprintf("%s.length === 2", open_boxes)))
    # Whether the two boxes overlap and how far each is from its own item, once both are placed: the older engine drew the
    # second box below the first, 56px away from its item; 0.59.0 puts it on another side of its own item.
    gap <- "
      (() => {
        const boxes = [...document.querySelectorAll('.hc-tip-pin:not([hidden])')].map((b) => b.getBoundingClientRect());
        const items = ['one', 'two'].map((id) => document.getElementById(id).getBoundingClientRect());
        if (boxes.length !== 2) return null;
        const apart = (a, b) => a.right <= b.left || b.right <= a.left || a.bottom <= b.top || b.bottom <= a.top;
        const near = (box, item) => Math.max(0, box.top - item.bottom, item.top - box.bottom);
        return JSON.stringify({
          apart: apart(boxes[0], boxes[1]),
          gaps: [near(boxes[0], items[0]), near(boxes[1], items[1])]
        });
      })()"
    expect_true(page$wait_until(sprintf("(() => { const g = %s; return g !== null && JSON.parse(g).apart; })()", gap)))
    got <- jsonlite::fromJSON(page$evaluate(gap))
    expect_true(got$apart)
    expect_lte(max(got$gaps), 12, label = paste("gaps to the items", toString(got$gaps)))
    page$evaluate("window.Hovercraft.close(); 0")
  })
})

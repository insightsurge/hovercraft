# Makes the pictures in vignettes/articles/figures/ with headless Chrome. Run by hand from the package root:
#
#   Rscript tools/make-figures.R
#
# An article must build with no Chrome, so its pictures are made here and kept in the repository. Run this again
# when a tip's look changes. Needs chromote, bslib, pkgload and a Chrome.

stopifnot(
  file.exists("DESCRIPTION"),
  requireNamespace("chromote", quietly = TRUE),
  requireNamespace("bslib", quietly = TRUE)
)
pkgload::load_all(quiet = TRUE)
out <- file.path("vignettes", "articles", "figures")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

browser <- chromote::Chrome$new(
  args = c(chromote::default_chrome_args(), paste0("--user-data-dir=", tempfile("hovercraft-chrome-")))
)
on.exit(browser$close(), add = TRUE)
chrome <- chromote::Chromote$new(browser = browser)
on.exit(chrome$close(), add = TRUE)

# One picture: the page, the page's own mode, the tag to rest on, and the file it is saved to.
shoot <- function(page, name, mode = "light", on = "#go", width = 420, height = 150) {
  dir <- tempfile("page-")
  dir.create(dir)
  path <- file.path(dir, "page.html")
  htmltools::save_html(page, path)
  session <- chromote::ChromoteSession$new(parent = chrome, width = width, height = height)
  on.exit(session$close(), add = TRUE)
  session$Page$navigate(paste0("file://", path))
  evaluate <- function(js) session$Runtime$evaluate(js, returnByValue = TRUE)$result$value
  deadline <- Sys.time() + 10
  while (!isTRUE(evaluate(sprintf("typeof %s === 'object'", tip_names()$global))) && Sys.time() < deadline) {
    Sys.sleep(0.1)
  }
  evaluate(sprintf("document.documentElement.setAttribute('data-bs-theme', '%s')", mode))
  box <- jsonlite::fromJSON(evaluate(sprintf(
    "(() => { const r = document.querySelector('%s').getBoundingClientRect(); return JSON.stringify([r.x + 10, r.y + r.height / 2]); })()",
    on
  )))
  session$Input$dispatchMouseEvent(type = "mouseMoved", x = width - 2, y = height - 2)
  Sys.sleep(0.2)
  session$Input$dispatchMouseEvent(type = "mouseMoved", x = box[[1]] - 2, y = box[[2]])
  session$Input$dispatchMouseEvent(type = "mouseMoved", x = box[[1]], y = box[[2]])
  Sys.sleep(0.7)
  shot <- session$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(shot$data), file.path(out, paste0(name, ".png")))
  message("wrote ", file.path(out, paste0(name, ".png")))
}

button <- function(text = "Runs the **check**") {
  tip_on(htmltools::tags$button(id = "go", class = "btn btn-primary", "Run"), text)
}
page <- function(..., style = NULL) {
  bslib::page_fluid(use_hovercraft(style = style), htmltools::tags$div(style = "padding: 16px", ...))
}

# 1. The page's own bslib variables, and the page turning dark.
for (mode in c("light", "dark")) {
  shoot(page(button()), paste0("default-", mode), mode)
}

# 2. A theme of the developer's own roles.
brand <- tip_theme(
  light = list(bg = "#1b3a6b", text = "#ffffff", radius = 10, shadow = "0 6px 18px rgba(27, 58, 107, 0.35)"),
  dark = list(bg = "#9ec5fe", text = "#0b1f3a")
)
for (mode in c("light", "dark")) {
  shoot(page(button(), style = brand), paste0("theme-", mode), mode)
}

# 3. A bslib theme, read through its variables.
warm <- bslib::bs_theme(
  version = 5,
  bg = "#fdf6e3",
  fg = "#073642",
  primary = "#268bd2",
  "border-radius" = "0.6rem",
  "body-bg-dark" = "#002b36",
  "body-color-dark" = "#eee8d5"
)
for (mode in c("light", "dark")) {
  shoot(page(button(), style = warm), paste0("bs-theme-", mode), mode)
}

# 4. A template, filled from a record.
invisible(tip_define(
  diff = tip_template(
    tip_title("{{ label }}"),
    tip_badge("{{ status }}", status = "warning"),
    tip_fields(Records = "{{ records | number:0 }}", Differences = "{{ differences | number:0 }}"),
    tip_text("{{ share | percent:1 }} of the records differ.")
  )
))
row <- list(label = "ADAE", status = "Check", records = 5304, differences = 3, share = 0.0006)
templated <- tip_on(
  htmltools::tags$button(id = "go", class = "btn btn-outline-secondary", "ADAE"),
  template = "diff",
  record = row
)
templates <- tip_define(
  diff = tip_template(
    tip_title("{{ label }}"),
    tip_badge("{{ status }}", status = "warning"),
    tip_fields(Records = "{{ records | number:0 }}", Differences = "{{ differences | number:0 }}"),
    tip_text("{{ share | percent:1 }} of the records differ.")
  )
)
for (mode in c("light", "dark")) {
  shoot(
    bslib::page_fluid(templates, htmltools::tags$div(style = "padding: 16px", templated)),
    paste0("template-", mode),
    mode,
    height = 200
  )
}

# What a page needs beyond the engine's own two files, delivered as files and as data, never as inline script or style,
# so a page under a strict Content-Security-Policy (`script-src 'self'; style-src 'self'`) works:
# - the runtime dependency: inst/runtime/ holds the start script (it attaches the engine, defines the templates the page
#   carries and sets the page's preset), the Shiny layer and the badge colours. It is told its names in a `<meta>`;
# - generated stylesheets (a theme, a page's style) are written to a folder of this session's `tempdir()` and served as
#   files by their dependency;
# - what used to be a script statement, such as defining a template, is data: JSON (as base64) in a `<meta>` of a
#   dependency, or in a `<script type="application/json">` block, which a browser does not run and a policy does not
#   block. The runtime script reads both, also when Shiny adds them to the page later.

# The runtime dependency. It comes after the engine and the dependencies that carry data, so tip_on() and the others add
# it last.
tip_runtime_dependency <- function() {
  htmltools::htmlDependency(
    name = "hovercraft-runtime",
    version = tip_engine_version(),
    src = "runtime",
    package = "hovercraft",
    script = c("shiny.js", "runtime.js"),
    stylesheet = c("shiny.css", "status.css"),
    meta = list("hovercraft-config" = tip_meta_value(tip_runtime_config())),
    all_files = FALSE
  )
}

# A JSON value for a `<meta>`'s content, as base64: htmltools writes a meta's content between double quotes without
# escaping them, so the text itself must hold none. The scripts that read it decode it.
tip_meta_value <- function(x) {
  jsonlite::base64_enc(charToRaw(enc2utf8(tip_json(x))))
}

# The folder of generated files. One per R process, under `tempdir()`, which R removes with the process; the files are
# named by what is in them, so a file written twice is the same file.
tip_gen_dir <- function() {
  if (is.null(tip_state$gen_dir) || !dir.exists(tip_state$gen_dir)) {
    tip_state$gen_dir <- tempfile("hovercraft-")
    dir.create(tip_state$gen_dir)
  }
  tip_state$gen_dir
}

# A dependency of generated content: a stylesheet written to a file, and operations for the runtime script as data.
# `ops` is a list of objects such as `list(op = "page", attributes = list(...))`; the runtime script runs each once.
tip_gen_dependency <- function(name, css = "", ops = list()) {
  stylesheet <- NULL
  if (nzchar(css)) {
    stylesheet <- paste0(substr(rlang::hash(css), 1L, 16L), ".css")
    path <- file.path(tip_gen_dir(), stylesheet)
    if (!file.exists(path)) {
      writeLines(css, path, useBytes = TRUE)
    }
  }
  htmltools::htmlDependency(
    name = name,
    version = tip_engine_version(),
    src = list(file = tip_gen_dir()),
    stylesheet = stylesheet,
    meta = if (length(ops)) list("hovercraft-ops" = tip_meta_value(unname(ops))),
    all_files = FALSE
  )
}

# Operations as JSON for a `<meta>` or a `<script type="application/json">`: a `</` is written `<\/`, so that the text
# cannot end the block that holds it.
tip_ops_json <- function(ops) {
  gsub("</", "<\\/", tip_json(unname(ops)), fixed = TRUE)
}

# The operations as a JSON data block for a tag, such as the output of tip_define(). A browser does not run it.
tip_ops_script <- function(ops) {
  htmltools::tags$script(type = "application/json", `data-hovercraft-ops` = "", htmltools::HTML(tip_ops_json(ops)))
}

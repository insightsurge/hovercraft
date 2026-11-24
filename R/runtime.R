# What a page needs beyond the engine's own two files, delivered as files and as data, never as inline script or style,
# so a page under a strict Content-Security-Policy (`script-src 'self'; style-src 'self'`) works:
# - the runtime dependency: inst/runtime/ holds the start script (it attaches the engine, defines the templates the page
#   carries and sets the page's preset), the Shiny layer and the badge colours. It is told its names in a `<meta>`;
# - generated CSS (a theme, a page's style) is data too, in a `<meta>`, and the runtime script applies it as a
#   constructed style sheet;
# - what used to be a script statement, such as defining a template, is data: JSON (as base64) in a `<meta>` of a
#   dependency, or in a `<script type="application/json">` block, which a browser does not run and a policy does not
#   block. The runtime script reads both, also when Shiny adds them to the page later.

# The runtime dependency. It comes after the engine and the dependencies that carry data, so tip_on() and the others add
# it last.
tip_runtime_dependency <- function() {
  htmltools::htmlDependency(
    name = "hovercraft-runtime",
    version = tip_package_version(),
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

# A dependency of generated content: a stylesheet and operations for the runtime script, both as data in a `<meta>`.
# It holds no file of its own and points at the package's own folder, so it works in any R process, also one that
# reads it back from a cache after the process that made it has gone (R Markdown and Quarto cache a chunk's
# dependencies, and Shiny's `bindCache()` can replay them from a cache that outlives the process). The runtime script
# applies the CSS as a constructed style sheet, which a policy that allows no inline style does not block.
# `ops` is a list of objects such as `list(op = "page", attributes = list(...))`; the runtime script runs each once.
tip_gen_dependency <- function(name, css = "", ops = list()) {
  if (nzchar(css)) {
    ops <- c(list(list(op = "css", id = substr(rlang::hash(css), 1L, 16L), text = css)), ops)
  }
  htmltools::htmlDependency(
    name = name,
    version = tip_package_version(),
    src = "runtime",
    package = "hovercraft",
    meta = if (length(ops)) list("hovercraft-ops" = tip_meta_value(unname(ops))),
    all_files = FALSE
  )
}

# The files of inst/runtime/ are the package's own, so they are versioned with the package: a release that changes
# shiny.js but not the engine is not served from a cached path of the engine's version.
tip_package_version <- function() as.character(getNamespaceVersion("hovercraft"))

# Operations as JSON for a `<meta>` or a `<script type="application/json">`: a `</` is written `<\/`, so that the text
# cannot end the block that holds it.
tip_ops_json <- function(ops) {
  gsub("</", "<\\/", tip_json(unname(ops)), fixed = TRUE)
}

# The operations as a JSON data block for a tag, such as the output of tip_define(). A browser does not run it.
tip_ops_script <- function(ops) {
  htmltools::tags$script(type = "application/json", `data-hovercraft-ops` = "", htmltools::HTML(tip_ops_json(ops)))
}

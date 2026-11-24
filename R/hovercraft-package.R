#' hovercraft: tips for Shiny and bslib pages
#'
#' `r lifecycle::badge("experimental")`
#' Put a tip on any HTML tag. The tip is plain text, or a named template filled from the values of one item, such as a
#' row of a table. Start with [tip_on()]; name templates with [tip_define()] and build
#' them with [tip_template()] and the block functions ([tip_title()] and friends). The tips are drawn by one small
#' JavaScript file that ships with the package, so nothing needs installing in the browser.
#'
#' @keywords internal
"_PACKAGE"

# Shiny reads the `_pinned` input of a table through a handler of its own type (see `tip_pinned_input_type()`). It is
# registered now when Shiny is already loaded, and when Shiny loads if it comes later.
tip_register_with_shiny <- function() {
  setHook(packageEvent("shiny", "onLoad"), function(...) tip_register_pinned_input())
  if (isNamespaceLoaded("shiny")) {
    tip_register_pinned_input()
  }
  invisible()
}

# nocov start: runs when the package loads, before the tests (and their coverage) begin; the function it calls is tested
.onLoad <- function(libname, pkgname) tip_register_with_shiny()
# nocov end

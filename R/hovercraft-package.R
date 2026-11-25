#' hovercraft: tips for Shiny and bslib pages
#'
#' Put a tip on any HTML tag. The tip is plain text, or a named template filled from the values of one item, such as a
#' row of a table. Start with [tip_on()]; name templates with [tip_define()] and build
#' them with [tip_template()] and the block functions ([tip_title()] and friends). The tips are drawn by one small
#' JavaScript file that ships with the package, so nothing needs installing in the browser.
#'
#' @section Stable and experimental:
#' `r lifecycle::badge("stable")` in 0.1.x: [tip_on()] with `text`, `template`, `record`, `placement`, `id`, `pin` (one
#' pinned tip at a time), `more` and a preset name as `style`; [use_hovercraft()] with a preset name; [tip_presets()];
#' [tip_template()], [tip_define()], [tip_title()], [tip_text()], [tip_badge()], [tip_fields()] and [tip_code()]. The
#' names of the inputs (`_hint`, `_pin`, `_pinned`, `_copy`, `_retry`) do not change.
#'
#' `r lifecycle::badge("experimental")` and free to change: [tip_table()] and [tip_spec()]; `events` and the fields of
#' the input values; [tip_show()], [tip_hide()] and [tip_update()]; `when_disabled`; [tip_output()]; [tip_style()],
#' [tip_theme()] and [tip_marker()], and `style` or `marker` given those objects; [tip_more()]; `max_pins` and `keys`
#' of [use_hovercraft()].
#'
#' @section Known limits:
#' Each is also on the page of the function it belongs to. Server calls reach only tags that are on the page and drawn
#' at that moment. A pin closes when its item is removed or hidden by a tab, a conditional panel or a sidebar, and a
#' `DT` table in server mode has only the rows of the current page on the screen. The inputs are `NULL` until their
#' first event. A page without Shiny (R Markdown, Quarto, a static page) has no inputs, server calls or live outputs.
#' On a viewport narrower than 600 pixels the engine keeps one pin at a time. A tag inside
#' `bslib::tooltip()` or `bslib::popover()` keeps the tip that bslib draws. Reconnecting an app is untested.
#'
#' @keywords internal
"_PACKAGE"

# Shiny reads the `_pinned` input of a table through a handler of its own type (see `tip_pinned_input_type()`). It is
# registered now when Shiny is already loaded, and when Shiny loads if it comes later.
tip_register_with_shiny <- function() {
  hook <- packageEvent("shiny", "onLoad")
  ours <- function(f) isTRUE(attr(f, "hovercraft"))
  # One hook of ours, however often the package is loaded (pkgload loads it again): the old one goes, the new one stays.
  setHook(hook, Filter(Negate(ours), getHook(hook)), "replace")
  setHook(hook, structure(function(...) tip_register_pinned_input(), hovercraft = TRUE))
  if (isNamespaceLoaded("shiny")) {
    tip_register_pinned_input()
  }
  invisible()
}

# nocov start: runs when the package loads, before the tests (and their coverage) begin; the function it calls is tested
.onLoad <- function(libname, pkgname) tip_register_with_shiny()
# nocov end

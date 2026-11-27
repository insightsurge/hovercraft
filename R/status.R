# The words a badge takes for `status`: Shiny's own, as in bslib's value boxes and alerts. The engine's badge field is
# called `tone` and takes any word; R is the one that limits it to these four, so a misspelt word is an error here and
# not a plain badge in the browser.
tip_statuses <- c("success", "warning", "danger", "info")

# A status read from the record is one placeholder and nothing else; "pre {{ tone }}" would never be one of the four
# words, so it is not taken for a placeholder and stops with the error that names them.
tip_status_has_placeholder <- function(status) {
  if (!is.character(status) || length(status) != 1L || is.na(status)) {
    return(FALSE)
  }
  ph <- tip_rules()$placeholder
  word <- trimws(status)
  # A placeholder that is not closed is still taken for one, so the placeholder check names what is wrong with it.
  startsWith(word, ph$open) &&
    lengths(gregexpr(ph$open, word, fixed = TRUE)) == 1L &&
    (endsWith(word, ph$close) || !grepl(ph$close, word, fixed = TRUE))
}

tip_check_status <- function(status, call = rlang::caller_env()) {
  if (is.null(status)) {
    return(invisible(status))
  }
  if (tip_status_has_placeholder(status)) {
    tip_check_text(status, "The status of `tip_badge()`", call = call)
    return(invisible(status))
  }
  if (!status %in% tip_statuses) {
    tip_abort(
      c(
        "{.arg status} is {.val {status}}, which is not a status.",
        i = paste0(
          "Give one of {.val {tip_statuses}}, a placeholder such as {.code \"",
          tip_escape("{{ status }}"),
          "\"}, or leave it out for a plain badge."
        )
      ),
      class = "hovercraft_error_status",
      call = call
    )
  }
  invisible(status)
}

# hovercraft's own stylesheet (inst/runtime/status.css, which test-status.R holds to this function), a small one on top
# of the engine: it colours a badge by its `tone` from the page's
# Bootstrap colours (5.3 and later; bslib pages have them, dark mode included). Where a page has no Bootstrap the
# colours after the commas are used, a light set, and a dark set under `data-bs-theme="dark"`, a tip's dark mode and the
# operating system's dark mode. It only names the
# engine's own public door, the badge's class and its `data-tone` attribute, and the class prefix comes from the
# engine's names; the after-copy check in test-engine.R fails when a copy of the engine lacks either.
tip_status_css <- function() {
  badge <- paste0(".", tip_names()$css, "-badge")
  light <- list(
    success = c("#d1e7dd", "#0a3622", "#a3cfbb"),
    warning = c("#fff3cd", "#664d03", "#ffe69c"),
    danger = c("#f8d7da", "#58151c", "#f1aeb5"),
    info = c("#cff4fc", "#055160", "#9eeaf9")
  )
  dark <- list(
    success = c("#051b11", "#75b798", "#0f5132"),
    warning = c("#332701", "#ffda6a", "#997404"),
    danger = c("#2c0b0e", "#ea868f", "#842029"),
    info = c("#032830", "#6edff6", "#087990")
  )
  # `scopes` are the selector prefixes of one rule: its selectors share the declarations.
  rule <- function(status, fallback, scopes = "") {
    sprintf(
      paste0(
        "%s{background:var(--bs-%s-bg-subtle,%s);",
        "color:var(--bs-%s-text-emphasis,%s);border-color:var(--bs-%s-border-subtle,%s)}"
      ),
      paste0(scopes, badge, "[data-tone=\"", status, "\"]", collapse = ","),
      status,
      fallback[[1]],
      status,
      fallback[[2]],
      status,
      fallback[[3]]
    )
  }
  # The dark set holds under the page's `data-bs-theme="dark"`, under a tip's own dark mode, and in the operating
  # system's dark mode on a page that has no `data-bs-theme` (a tip forced to light mode stays light), as theme.R does.
  tip <- paste0(".", tip_names()$css)
  dark_scopes <- c("[data-bs-theme=\"dark\"] ", "[data-hovercraft-mode=\"dark\"] ")
  system_scope <- paste0(":root:not([data-bs-theme]) ", tip, ":not([data-hovercraft-mode=\"light\"]) ")
  rules <- c(
    vapply(tip_statuses, function(status) rule(status, light[[status]]), ""),
    vapply(tip_statuses, function(status) rule(status, dark[[status]], dark_scopes), ""),
    paste0(
      "@media (prefers-color-scheme:dark){",
      paste(vapply(tip_statuses, function(status) rule(status, dark[[status]], system_scope), ""), collapse = ""),
      "}"
    )
  )
  paste(rules, collapse = "\n")
}

# The words a badge takes for `status`: Shiny's own, as in bslib's value boxes and alerts. The engine's badge field is
# called `tone` and takes any word; R is the one that limits it to these four, so a misspelt word is an error here and
# not a plain badge in the browser.
tip_statuses <- c("success", "warning", "danger", "info")

tip_check_status <- function(status, call = rlang::caller_env()) {
  if (!is.null(status) && !status %in% tip_statuses) {
    tip_abort(
      c(
        "{.arg status} is {.val {status}}, which is not a status.",
        i = "Give one of {.val {tip_statuses}}, or leave it out for a plain badge."
      ),
      class = "hovercraft_error_status",
      call = call
    )
  }
  invisible(status)
}

# hovercraft's own stylesheet, a small one on top of the engine: it colours a badge by its `tone` from the page's
# Bootstrap colours (5.3 and later; bslib pages have them, dark mode included). Where a page has no Bootstrap the
# colours after the commas are used, a light set, and a dark set under `data-bs-theme="dark"`. It only names the
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
  rule <- function(status, fallback, scope = "") {
    sprintf(
      paste0(
        "%s%s[data-tone=\"%s\"]{background:var(--bs-%s-bg-subtle,%s);",
        "color:var(--bs-%s-text-emphasis,%s);border-color:var(--bs-%s-border-subtle,%s)}"
      ),
      scope,
      badge,
      status,
      status,
      fallback[[1]],
      status,
      fallback[[2]],
      status,
      fallback[[3]]
    )
  }
  rules <- c(
    vapply(tip_statuses, function(status) rule(status, light[[status]]), ""),
    vapply(tip_statuses, function(status) rule(status, dark[[status]], "[data-bs-theme=\"dark\"] "), "")
  )
  paste0("<style>", paste(rules, collapse = "\n"), "</style>")
}

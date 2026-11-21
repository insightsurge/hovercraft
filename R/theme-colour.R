# Colours and CSS values for a theme. A colour is anything R knows (a name, hex, rgb() output, an "rgba(0, 0, 0, 0.8)"
# string, three numbers from 0 to 255) or a CSS variable (`--team-blue` or `var(--team-blue)`), which the browser works
# out and R cannot measure. Values end up in a <style> block, so each is read from a short allowed set of characters.

tip_var_pattern <- "^(var\\([[:space:]]*--[A-Za-z][A-Za-z0-9_-]*[[:space:]]*\\)|--[A-Za-z][A-Za-z0-9_-]*)$"

tip_is_var <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && grepl(tip_var_pattern, x)
}

# `var(--x)` for `--x` or `var( --x )`.
tip_var <- function(x) {
  if (startsWith(x, "var(")) x else paste0("var(", x, ")")
}

# c(red, green, blue, alpha) with colour from 0 to 255 and alpha from 0 to 1, or NULL when `x` is no colour. Numbers are
# red, green and blue, never an R palette index.
tip_channels <- function(x) {
  if (is.numeric(x)) {
    ok <- isTRUE(checkmate::check_numeric(x, len = 3L, lower = 0, upper = 255, any.missing = FALSE))
    return(if (ok) c(as.numeric(x), 1) else NULL)
  }
  if (!isTRUE(checkmate::check_string(x, min.chars = 1L))) {
    return(NULL)
  }
  if (grepl("^rgba?\\(", x, ignore.case = TRUE)) {
    match <- regmatches(x, regexec("^(rgb|rgba)\\((.*)\\)$", x, ignore.case = TRUE))[[1]]
    if (!length(match)) {
      return(NULL)
    }
    parts <- trimws(strsplit(match[[3]], ",", fixed = TRUE)[[1]])
    count <- if (tolower(match[[2]]) == "rgb") 3L else 4L
    number <- "^(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)$"
    if (length(parts) != count || !all(grepl(number, parts, perl = TRUE))) {
      return(NULL)
    }
    channels <- as.numeric(parts)
    if (any(channels[1:3] > 255) || (count == 4L && channels[[4]] > 1)) {
      return(NULL)
    }
    return(if (count == 3L) c(channels, 1) else channels)
  }
  if (grepl("^#[0-9a-fA-F]{3,4}$", x)) {
    x <- paste0("#", paste(rep(strsplit(substring(x, 2), "")[[1]], each = 2), collapse = ""))
  }
  channels <- tryCatch(grDevices::col2rgb(x, alpha = TRUE), error = function(error) NULL)
  if (is.null(channels)) {
    return(NULL)
  }
  unname(c(channels[1:3, 1], channels[4, 1] / 255))
}

# The colour as CSS: #rrggbb, or rgba() when it is not opaque; a variable as var(--x). NULL when it is neither.
tip_colour <- function(x) {
  if (tip_is_var(x)) {
    return(tip_var(x))
  }
  channels <- tip_channels(x)
  if (is.null(channels)) {
    return(NULL)
  }
  if (channels[[4]] == 1 && all(channels[1:3] == round(channels[1:3]))) {
    return(tolower(grDevices::rgb(channels[[1]], channels[[2]], channels[[3]], maxColorValue = 255)))
  }
  numbers <- vapply(channels, function(v) format(v, scientific = FALSE, trim = TRUE, digits = 15), "")
  paste0("rgba(", paste(numbers, collapse = ", "), ")")
}

# The contrast ratio of `fg` on `bg` (both colours R can read), the foreground's alpha laid over the background.
tip_contrast <- function(fg, bg) {
  front <- tip_channels(fg)
  back <- tip_channels(bg)[1:3] / 255
  mixed <- front[1:3] / 255 * front[[4]] + back * (1 - front[[4]])
  luminance <- function(channels) {
    sum(ifelse(channels <= 0.04045, channels / 12.92, ((channels + 0.055) / 1.055)^2.4) * c(0.2126, 0.7152, 0.0722))
  }
  values <- c(luminance(mixed), luminance(back))
  (max(values) + 0.05) / (min(values) + 0.05)
}

# Text that is safe in a CSS declaration (a shadow, a font family, a custom property's value): a short set of
# characters, with nothing that could end the declaration, open a block, add a URL or comment.
tip_css_text <- function(x) {
  if (!isTRUE(checkmate::check_string(x, min.chars = 1L))) {
    return(NULL)
  }
  bad <- grepl("[^A-Za-z0-9 ,.'\"()#%_+*-]", x) || grepl("url\\(|expression\\(|@import", x, ignore.case = TRUE)
  if (bad) NULL else trimws(x)
}

# A corner: a number of pixels, a CSS length or a variable.
tip_length <- function(x) {
  if (is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 0) {
    return(paste0(format(x, scientific = FALSE, trim = TRUE), "px"))
  }
  if (tip_is_var(x)) {
    return(tip_var(x))
  }
  if (isTRUE(checkmate::check_string(x)) && grepl("^([0-9]+\\.?[0-9]*|\\.[0-9]+)(px|rem|em|%)?$", x)) {
    return(x)
  }
  NULL
}

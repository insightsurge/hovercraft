# The package's help pages, read for what they say about each export and argument: whether it is stable or
# experimental. test-book.R reads the same table to know every argument the book must show.

# The help pages as Rd objects: from the source's man/ when the package is loaded from its source (the installed help
# may be older), else the installed help.
book_rd_db <- function() {
  root <- find.package("hovercraft")
  if (dir.exists(file.path(root, "man"))) tools::Rd_db(dir = root) else tools::Rd_db("hovercraft")
}

# The pieces of an Rd object with a given tag, such as "\\arguments".
rd_part <- function(rd, tag) Filter(function(x) identical(attr(x, "Rd_tag"), tag), rd)

# Whether a piece of a help page carries a badge: "stable", "experimental", "mixed" (both, as in "stable as a preset
# name, experimental as an object") or NA for none.
rd_badge <- function(x) {
  text <- paste(unlist(x), collapse = " ")
  found <- c(
    stable = grepl("lifecycle-stable", text, fixed = TRUE),
    experimental = grepl("lifecycle-experimental", text, fixed = TRUE)
  )
  if (all(found)) "mixed" else if (any(found)) names(found)[found] else NA_character_
}

# The name of an \item, split where one item names several arguments ("row, column"); `...` as "...".
rd_item_names <- function(item) {
  name <- item[[1]]
  if (any(vapply(name, function(x) identical(attr(x, "Rd_tag"), "\\dots"), TRUE))) {
    return("...")
  }
  trimws(strsplit(paste(unlist(name), collapse = ""), ",")[[1]])
}

# One row per export and per argument: `fn`, `arg` (NA for the function itself), `badge` (an argument with no badge
# of its own takes its function's), `documented` (whether the argument has an \item on the help page) and
# `content_dots` (whether the function's `...` takes content, such as tip_define()'s named templates, rather than
# having to be empty). An
# argument documented on a page but not in any of its functions' formals reaches a function through its `...`, and
# counts as that function's, unless the page says the `...` must be empty.
book_rd_args <- function(db = book_rd_db(), exports = getNamespaceExports("hovercraft")) {
  rows <- lapply(db, function(rd) {
    aliases <- intersect(unlist(lapply(rd_part(rd, "\\alias"), unlist)), exports)
    if (!length(aliases)) {
      return(NULL)
    }
    page_badge <- rd_badge(rd_part(rd, "\\description"))
    items <- Filter(function(x) identical(attr(x, "Rd_tag"), "\\item"), unlist(rd_part(rd, "\\arguments"), FALSE))
    item_badge <- list()
    for (item in items) {
      for (name in rd_item_names(item)) item_badge[[name]] <- rd_badge(item[[2]])
    }
    dots_text <- if (!is.null(item_badge[["..."]])) {
      paste(unlist(Filter(function(i) identical(rd_item_names(i), "..."), items)), collapse = " ")
    }
    open_dots <- !is.null(dots_text) && !grepl("Must be empty", dots_text, fixed = TRUE)
    formals_of <- lapply(aliases, function(fn) names(formals(getExportedValue("hovercraft", fn))))
    passed <- setdiff(names(item_badge), c(unlist(formals_of), "..."))
    do.call(rbind, lapply(seq_along(aliases), function(i) {
      fn <- aliases[[i]]
      args <- setdiff(formals_of[[i]], "...")
      if (open_dots && "..." %in% formals_of[[i]]) args <- c(args, passed)
      badge <- vapply(args, function(a) item_badge[[a]] %||% NA_character_, "")
      badge[is.na(badge)] <- page_badge
      data.frame(
        fn = fn,
        arg = c(NA_character_, args),
        badge = c(page_badge, unname(badge)),
        documented = c(TRUE, args %in% names(item_badge)),
        content_dots = open_dots && "..." %in% formals_of[[i]]
      )
    }))
  })
  do.call(rbind, rows)
}

# The same table as "fn(arg)" and "fn" strings, the way contents.R names them.
book_rd_names <- function(args) as.character(ifelse(is.na(args$arg), args$fn, paste0(args$fn, "(", args$arg, ")")))

# Run by hand, from the package root, before any release:   Rscript tools/check-release.R
#
# It fails (exit 1, one line for each problem) when
#   - inst/engine/engine.json says the engine was not copied from its main branch,
#   - a vendored file does not match the sha256 engine.json recorded for it, or
#   - the package's public text names the engine's project: README.md, DESCRIPTION, NEWS.md, _pkgdown.yml, R/, man/,
#     and the files the engine ships (inst/engine/www/, inst/engine/presets/, inst/engine/tip.json and
#     inst/engine/engine.json), or
#   - the maintainer address in DESCRIPTION is a noreply address, or
#   - DESCRIPTION has no URL or BugReports, _pkgdown.yml has no url, or one of those links does not load (the
#     repository must be public before a CRAN release), or
#   - a file still has a TODO(founder) (DESCRIPTION, inst/COPYRIGHTS, the licence files, the vignettes, cran-comments.md).
# It is not part of the test suite, so the normal tests never skip and a work-in-progress copy (the engine's
# integration branch, an engine that still carries its project's name) passes them; it is the release gate's check.

stop_if_missing <- function(path) {
  if (!file.exists(path)) {
    stop("Run this from the package root: ", path, " is missing.", call. = FALSE)
  }
}
stop_if_missing("DESCRIPTION")
stop_if_missing("inst/engine/engine.json")
engine <- jsonlite::read_json("inst/engine/engine.json")
problems <- character()

if (!identical(engine$branch, "main")) {
  problems <- c(problems, sprintf("engine.json says branch \"%s\"; a release is copied from \"main\".", engine$branch))
}

sha256 <- function(path) {
  tool <- Sys.which(c("shasum", "sha256sum"))
  tool <- tool[nzchar(tool)][1L]
  if (is.na(tool)) {
    stop("Neither shasum nor sha256sum is on the PATH.", call. = FALSE)
  }
  args <- c(if (basename(tool) == "shasum") c("-a", "256"), shQuote(path))
  strsplit(system2(tool, args, stdout = TRUE), " ", fixed = TRUE)[[1]][[1]]
}
for (file in unlist(engine$files)) {
  path <- file.path("inst/engine", file)
  if (!file.exists(path)) {
    problems <- c(problems, sprintf("%s is missing.", path))
  } else if (!identical(sha256(path), engine$sha256[[file]])) {
    problems <- c(problems, sprintf("%s does not match the sha256 in engine.json.", path))
  }
}

public <- c(
  "README.md",
  "DESCRIPTION",
  "NEWS.md",
  "_pkgdown.yml",
  list.files(c("R", "man"), recursive = TRUE, full.names = TRUE),
  list.files(c("inst/engine/www", "inst/engine/presets"), recursive = TRUE, full.names = TRUE),
  "inst/engine/tip.json",
  "inst/engine/engine.json"
)
project <- paste0("atl", "as") # written in two pieces so this file does not name it in one
for (path in public[file.exists(public)]) {
  hits <- grep(project, readLines(path, warn = FALSE, encoding = "UTF-8"), ignore.case = TRUE)
  if (length(hits)) {
    problems <- c(problems, sprintf("%s names the engine's project on %d line(s).", path, length(hits)))
  }
}

# CRAN mails the maintainer address, and GitHub's noreply addresses cannot receive mail.
if (any(grepl("noreply", readLines("DESCRIPTION", warn = FALSE), fixed = TRUE))) {
  problems <- c(problems, "DESCRIPTION has a noreply maintainer address; CRAN needs one that can receive mail.")
}

# CRAN wants public links, and --as-cran fetches them: while the repository is private they 404. Each link in URL and
# BugReports, and the site's url in _pkgdown.yml, is fetched here and must load.
description <- read.dcf("DESCRIPTION", fields = c("URL", "BugReports"))
links <- character()
for (field in colnames(description)) {
  if (is.na(description[1, field]) || !nzchar(trimws(description[1, field]))) {
    problems <- c(problems, sprintf("DESCRIPTION has no %s.", field))
  } else {
    links <- c(links, trimws(strsplit(description[1, field], "[,[:space:]]+")[[1]]))
  }
}
site <- sub("^url:[[:space:]]*", "", grep("^url:", readLines("_pkgdown.yml", warn = FALSE), value = TRUE))
if (!length(site)) {
  problems <- c(problems, "_pkgdown.yml has no url.")
}
for (link in unique(c(links[nzchar(links)], trimws(site)))) {
  status <- tryCatch(
    attr(curlGetHeaders(link, redirect = TRUE, timeout = 20L), "status"),
    error = function(e) NA_integer_
  )
  if (!identical(as.integer(status), 200L)) {
    problems <- c(
      problems,
      sprintf(
        "%s does not load (%s); make the repository and the site public.",
        link,
        if (is.na(status)) "no answer" else status
      )
    )
  }
}

# A placeholder for a decision the founder has yet to make must be gone before a release.
open_items <- c(
  public,
  "cran-comments.md",
  "inst/COPYRIGHTS",
  "LICENSE",
  "LICENSE.md",
  list.files("vignettes", pattern = "[.](Rmd|R|md)$", recursive = TRUE, full.names = TRUE)
)
for (path in open_items[file.exists(open_items)]) {
  if (any(grepl("TODO(founder)", readLines(path, warn = FALSE, encoding = "UTF-8"), fixed = TRUE))) {
    problems <- c(problems, sprintf("%s still has a TODO(founder).", path))
  }
}

if (length(problems)) {
  cat(paste0("NOT READY: ", problems, "\n"), sep = "")
  quit(status = 1L)
}
cat(
  "Release check passed: the engine is from main, its files match engine.json and no public text names its project.\n"
)

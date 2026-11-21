# Run by hand, from the package root, before any release:   Rscript tools/check-release.R
#
# It fails (exit 1, one line for each problem) when
#   - inst/engine/engine.json says the engine was not copied from its main branch,
#   - a vendored file does not match the sha256 engine.json recorded for it, or
#   - the package's public text names the engine's project: README.md, DESCRIPTION, NEWS.md, _pkgdown.yml, R/, man/,
#     and the files a page is given (inst/engine/www/ and inst/engine/engine.json), or
#   - the maintainer address in DESCRIPTION is a noreply address, or
#   - DESCRIPTION has no URL or no BugReports, or _pkgdown.yml no url (the repository must be public before a CRAN release), or
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
  list.files("inst/engine/www", recursive = TRUE, full.names = TRUE),
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

# A private repository has no public links, so DESCRIPTION leaves them out; CRAN wants them, and they must not 404.
description <- read.dcf("DESCRIPTION", fields = c("URL", "BugReports"))
for (field in colnames(description)) {
  if (is.na(description[1, field]) || !nzchar(trimws(description[1, field]))) {
    problems <- c(problems, sprintf("DESCRIPTION has no %s; make the repository public and add it.", field))
  }
}

if (!any(grepl("^url:", readLines("_pkgdown.yml", warn = FALSE)))) {
  problems <- c(problems, "_pkgdown.yml has no url; add it with DESCRIPTION's URL once the site is public.")
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

# Copies the tip engine into inst/engine/ and records what it copied. Local and gitignored: the package creator runs it.
#
#   Rscript tools/copy-engine.R <path to the engine's checkout>      (or set ENGINE_DIR)
#   ENGINE_BRANCH=main-interim Rscript tools/copy-engine.R <path>    (default branch: main)
#
# It refuses to run when the checkout has uncommitted changes, when its commit is not on origin/<ENGINE_BRANCH>, when
# the engine's own build check (tools/build_tip.py --check) fails, or when a file does not match tip.sha256. It copies
# tip.js and tip.css into inst/engine/www/ (the only folder a page is ever given), and tip.sha256 and the shared
# placeholder cases into inst/engine/, then writes inst/engine/engine.json: version, commit, branch, files, sha256, the
# names the engine uses (read from its NAMES block) and its tooltip rules (read from the schema). engine.json stays
# outside www/, so a rendered page never holds where the engine came from. Files under inst/engine/ are never edited by
# hand: run this again instead. A release is copied from main; the release test fails when engine.json says otherwise.

args <- commandArgs(trailingOnly = TRUE)
source_dir <- if (length(args) >= 1L) args[[1]] else Sys.getenv("ENGINE_DIR", unset = "")
branch <- Sys.getenv("ENGINE_BRANCH", unset = "main")
stop_if <- function(bad, ...) if (bad) stop(paste0(...), call. = FALSE)
stop_if(!nzchar(source_dir), "Give the path to the engine's checkout as the first argument, or set ENGINE_DIR.")
source_dir <- normalizePath(source_dir, mustWork = FALSE)
stop_if(!dir.exists(source_dir), "No such folder: ", source_dir)
root <- normalizePath(".")
stop_if(!file.exists(file.path(root, "DESCRIPTION")), "Run this from the root of the package.")
stop_if(!requireNamespace("jsonlite", quietly = TRUE), "The jsonlite package is needed to write engine.json.")

at <- function(...) file.path(source_dir, ...)
git <- function(...) system2("git", c("-C", shQuote(source_dir), ...), stdout = TRUE, stderr = FALSE)
for (needed in c(
  "kit/tip/tip.js",
  "kit/tip/tip.css",
  "kit/tip/tip.sha256",
  "tools/build_tip.py",
  "tools/tests/fixtures/tooltip/placeholders.json",
  "library/page-tree.schema.json"
)) {
  stop_if(!file.exists(at(needed)), "The checkout has no ", needed, ".")
}
stop_if(length(git("status", "--porcelain")) > 0L, "The checkout has uncommitted changes; commit or stash them first.")
commit <- git("rev-parse", "HEAD")
on_branch <- system2(
  "git",
  c("-C", shQuote(source_dir), "merge-base", "--is-ancestor", "HEAD", shQuote(paste0("origin/", branch))),
  stdout = FALSE,
  stderr = FALSE
) ==
  0L
stop_if(!on_branch, "Commit ", substr(commit, 1, 8), " is not on origin/", branch, " (fetch it, or set ENGINE_BRANCH).")

built <- system2(
  "python3",
  c(shQuote(at("tools/build_tip.py")), "--root", shQuote(source_dir), "--check"),
  stdout = TRUE,
  stderr = TRUE
)
stop_if(!is.null(attr(built, "status")), "The engine's own check failed: ", paste(built, collapse = " "))

sha256 <- function(path) {
  tool <- Sys.which(c("shasum", "sha256sum"))
  tool <- tool[nzchar(tool)][1L]
  stop_if(is.na(tool), "Neither shasum nor sha256sum is on the PATH.")
  out <- system2(tool, c(if (basename(tool) == "shasum") c("-a", "256"), shQuote(path)), stdout = TRUE)
  strsplit(out, " ", fixed = TRUE)[[1]][[1]]
}
listed <- strsplit(readLines(at("kit/tip/tip.sha256"), warn = FALSE), "  ", fixed = TRUE)
listed <- stats::setNames(vapply(listed, `[[`, "", 1L), vapply(listed, `[[`, "", 2L))
for (name in c("tip.js", "tip.css")) {
  stop_if(!name %in% names(listed), "tip.sha256 has no line for ", name, ".")
  stop_if(!identical(sha256(at("kit/tip", name)), listed[[name]]), name, " does not match tip.sha256.")
}

js <- readLines(at("kit/tip/tip.js"), warn = FALSE)
grab <- function(pattern, lines = js) {
  hit <- regmatches(lines, regexec(pattern, lines))
  hit <- hit[lengths(hit) > 0L]
  stop_if(length(hit) == 0L, "tip.js has no line like ", pattern)
  hit[[1]][-1L]
}
version <- grab("const VERSION = '([^']+)';")
names_line <- grab("const TIP_NAMES = Object\\.freeze\\(\\{ attr: '([^']+)', css: '([^']+)', who: '([^']+)'")
# The prefix of the engine's CSS variables and attributes: the attribute is data-<prefix>-tip.
stop_if(!grepl("^data-.+-tip$", names_line[[1]]), "The attribute name ", names_line[[1]], " is not data-<prefix>-tip.")
prefix <- sub("^data-(.+)-tip$", "\\1", names_line[[1]])
schema <- jsonlite::read_json(at("library/page-tree.schema.json"))
# Only the data goes into engine.json, not the prose that explains it.
strip_prose <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  keep <- if (is.null(names(x))) seq_along(x) else !names(x) %in% c("description", "about")
  lapply(x[keep], strip_prose)
}

dest <- file.path(root, "inst", "engine")
unlink(dest, recursive = TRUE)
dir.create(file.path(dest, "www"), recursive = TRUE, showWarnings = FALSE)
files <- c(
  "placeholders.json" = at("tools/tests/fixtures/tooltip/placeholders.json"),
  "tip.sha256" = at("kit/tip/tip.sha256"),
  "www/tip.css" = at("kit/tip/tip.css"),
  "www/tip.js" = at("kit/tip/tip.js")
)
for (name in names(files)) {
  stop_if(!file.copy(files[[name]], file.path(dest, name), overwrite = TRUE), "Could not copy ", name)
}
recorded <- c("placeholders.json", "www/tip.css", "www/tip.js")
record <- list(
  engine = version,
  commit = commit,
  branch = branch,
  files = as.list(recorded),
  sha256 = stats::setNames(lapply(recorded, function(name) sha256(file.path(dest, name))), recorded),
  names = list(
    attribute = names_line[[1]],
    css = names_line[[2]],
    global = names_line[[3]],
    prefix = prefix
  ),
  rules = list(
    tooltip = strip_prose(schema$rules$tooltip),
    names = list(pattern = schema$names$pattern, r_reserved = schema$names$r_reserved)
  )
)
jsonlite::write_json(record, file.path(dest, "engine.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)
cat(
  "Copied the engine ",
  version,
  " from ",
  substr(commit, 1, 8),
  " (origin/",
  branch,
  ") into inst/engine/.\n",
  sep = ""
)

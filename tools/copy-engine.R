# Copies the tip engine into inst/engine/ and records what it copied. Local and gitignored: the package creator runs it.
#
#   Rscript tools/copy-engine.R <path to the engine's checkout>      (or set ENGINE_DIR)
#   ENGINE_BRANCH=main-interim Rscript tools/copy-engine.R <path>    (default branch: main)
#
# It refuses to run when the checkout has uncommitted changes, when its commit is not on origin/<ENGINE_BRANCH>, when
# the engine's own build check (tools/build_tip.py --check) fails, or when a file does not match tip.sha256. It then
# builds the engine under this package's own names (tools/build_tip.py --names), refuses a build that still holds a
# trace of the names it was built from, and copies tip.js and tip.css into inst/engine/www/ (the only folder a page is
# ever given), and tip.json (the engine's page contract: attributes, events, placements, under this package's names),
# tip.sha256 and the shared placeholder cases into inst/engine/, and the engine's presets/ folder (when it has one) into
# inst/engine/presets/; the preset files are held to tip.sha256 and the old-name check like the other three. It writes
# inst/engine/engine.json: version, commit, branch, files, sha256, the names the built engine uses and its tooltip
# rules (read from the schema). engine.json stays outside www/, so a rendered page never holds where the engine came
# from. Files under inst/engine/ are never edited by hand: run this again instead. A release is copied from main; the
# release test fails when engine.json says otherwise. After every copy, run the tests: test-engine.R checks that the
# engine still has every name this package relies on.

args <- commandArgs(trailingOnly = TRUE)
source_dir <- if (length(args) >= 1L) args[[1]] else Sys.getenv("ENGINE_DIR", unset = "")
branch <- Sys.getenv("ENGINE_BRANCH", unset = "main")
# The names the engine is built under: the prefix of its attribute and CSS variables, its CSS class prefix, its
# JavaScript global and its product name.
engine_names <- list(prefix = "hovercraft", css = "hc", who = "Hovercraft", product = "hovercraft")
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
  "kit/tip/tip.json",
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
# The files the engine's build records: the script, the stylesheet, the contract and every preset, each held to its line.
preset_files <- function(dir) {
  if (dir.exists(file.path(dir, "presets"))) {
    file.path("presets", list.files(file.path(dir, "presets"), recursive = TRUE))
  } else {
    character()
  }
}
checked <- c("tip.js", "tip.css", "tip.json", preset_files(at("kit/tip")))
for (name in checked) {
  stop_if(!name %in% names(listed), "tip.sha256 has no line for ", name, ".")
  stop_if(!identical(sha256(at("kit/tip", name)), listed[[name]]), name, " does not match tip.sha256.")
}

# The names the checkout's own build carries: what must not be left in a renamed one.
grab <- function(pattern, lines) {
  lines <- paste(lines, collapse = " ")
  hit <- regmatches(lines, regexec(pattern, lines))
  hit <- hit[lengths(hit) > 0L]
  stop_if(length(hit) == 0L, "tip.js has no line like ", pattern)
  hit[[1]][-1L]
}
# The engine writes this object on one line or one key a line, so the lines are joined before the match.
names_pattern <- "const TIP_NAMES = Object\\.freeze\\(\\{ *attr: '([^']+)', *css: '([^']+)', *who: '([^']+)'"
was <- grab(names_pattern, readLines(at("kit/tip/tip.js"), warn = FALSE))
old_prefix <- sub("^data-(.+)-tip$", "\\1", was[[1]])
old_css <- paste0(sub("-tip$", "", was[[2]]), "-")

build <- tempfile("engine-build-")
status <- system2(
  "python3",
  c(
    shQuote(at("tools/build_tip.py")),
    "--root",
    shQuote(source_dir),
    "--names",
    shQuote(as.character(jsonlite::toJSON(engine_names, auto_unbox = TRUE))),
    "--out",
    shQuote(build)
  ),
  stdout = TRUE,
  stderr = TRUE
)
stop_if(!is.null(attr(status, "status")), "The engine's build with our names failed: ", paste(status, collapse = " "))
built_sums <- strsplit(readLines(file.path(build, "tip.sha256"), warn = FALSE), "  ", fixed = TRUE)
built_sums <- stats::setNames(vapply(built_sums, `[[`, "", 1L), vapply(built_sums, `[[`, "", 2L))
stop_if(
  !setequal(preset_files(build), preset_files(at("kit/tip"))),
  "The built presets/ folder does not hold the same files as the engine's."
)
for (name in c("tip.js", "tip.css", "tip.json", preset_files(build))) {
  stop_if(!name %in% names(built_sums), "The built tip.sha256 has no line for ", name, ".")
  stop_if(
    !identical(sha256(file.path(build, name)), built_sums[[name]]),
    "The built ",
    name,
    " does not match its tip.sha256."
  )
  text <- readLines(file.path(build, name), warn = FALSE)
  for (old in c(old_prefix, old_css)) {
    stop_if(
      # a name counts as left behind only where it starts a word ("-webkit-" holds "kit-" but is not the old name)
      any(grepl(paste0("(^|[^a-z0-9])", tolower(old)), tolower(text))),
      "The built ",
      name,
      " still holds the old name \"",
      old,
      "\"."
    )
  }
}

js <- readLines(file.path(build, "tip.js"), warn = FALSE)
version <- grab("const VERSION = '([^']+)';", js)
names_line <- grab(names_pattern, js)
# The prefix of the engine's CSS variables and attributes: the attribute is data-<prefix>-tip.
stop_if(!grepl("^data-.+-tip$", names_line[[1]]), "The attribute name ", names_line[[1]], " is not data-<prefix>-tip.")
prefix <- sub("^data-(.+)-tip$", "\\1", names_line[[1]])
stop_if(!identical(prefix, engine_names$prefix), "The built prefix ", prefix, " is not the one asked for.")
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
  "tip.json" = file.path(build, "tip.json"),
  "tip.sha256" = file.path(build, "tip.sha256"),
  "www/tip.css" = file.path(build, "tip.css"),
  "www/tip.js" = file.path(build, "tip.js")
)
# The engine's presets, when it has any: each file recorded below.
# They are taken from the build, the files that were checked above.
presets <- preset_files(build)
for (preset in presets) {
  files[[preset]] <- file.path(build, preset)
}
for (name in names(files)) {
  dir.create(dirname(file.path(dest, name)), recursive = TRUE, showWarnings = FALSE)
  stop_if(!file.copy(files[[name]], file.path(dest, name), overwrite = TRUE), "Could not copy ", name)
}
recorded <- c(
  "placeholders.json",
  "tip.json",
  "www/tip.css",
  "www/tip.js",
  presets
)
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
    prefix = prefix,
    product = engine_names$product
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

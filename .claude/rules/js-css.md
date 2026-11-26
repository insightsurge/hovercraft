---
paths:
  - "inst/**"
  - "tests/testthat/apps/testbench/www/**"
  - "tools/copy-engine.R"
---

# JavaScript and CSS

Only for packages with JavaScript or CSS in `inst/`.

- Keep it plain and readable, with no build step unless the package already has one.
- Ship every file the package needs inside it; never load from a CDN. Keep a third-party file's licence, and note
  where it came from.
- Biome formats and lints it, with the settings in `biome.json`.
- **Accessibility:** meaning never rests on colour alone, everything works from the keyboard, controls have labels,
  and motion honours `prefers-reduced-motion`.

## The vendored engine (`inst/engine/`)

The JavaScript and CSS are a built copy of an engine whose source lives in a separate private repository.

- **Never edit anything under `inst/engine/` by hand.** `tools/copy-engine.R` writes it; only the maintainer runs it,
  from a clean checkout of the engine's source (`Rscript tools/copy-engine.R <checkout>`, with `ENGINE_BRANCH` set when
  the commit is not on `main`). A change to the engine is made in its source, then copied.
- The script refuses a dirty checkout, a commit that is not on `origin/<ENGINE_BRANCH>`, and a build that still holds
  the engine's own names. It builds the engine under this package's names and records what it copied in `engine.json`
  (version, commit, branch, sha256 of each file, the names, the tooltip rules).
- Only `inst/engine/www/` (`tip.js`, `tip.css`) is given to a page, so no commit or branch reaches a rendered page.
  `engine.json`, `placeholders.json` and `tip.sha256` stay beside it. The engine's page contract, `tip.json`, is read
  the same way once the script copies it.
- Biome leaves `inst/engine/` alone (see `biome.json`); it is generated.
- Public text (README, help pages, vignettes, `DESCRIPTION`, `NEWS.md`) calls it "the tip engine" and never names the
  project it is built from.

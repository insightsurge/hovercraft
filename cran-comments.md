## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes for the reviewer

* The package ships a small JavaScript file and a stylesheet in `inst/engine/www/`, with the copyright holder and licence
  listed in `inst/COPYRIGHTS`. They are the unminified engine, built from its source files by concatenation, with no
  minifying or obfuscation; `inst/COPYRIGHTS` says where that source lives.
* The vignettes build offline and need no browser. Their pictures are made by hand and kept in `vignettes/figures/`.
* The tests that drive a browser (chromote) and the one that starts a Shiny app skip on CRAN.
* Nothing is written outside `tempdir()`, and no network is used.

## Before submission

* The repository is private for now, so `DESCRIPTION` has no `URL` or `BugReports`, and `_pkgdown.yml` has no `url` (they
  would return 404 under `--as-cran`, and pkgdown wants the two to agree). Once the repository and the site are public, add back:
  <!-- URL: https://insightsurge.github.io/hovercraft/, https://github.com/insightsurge/hovercraft -->
  <!-- BugReports: https://github.com/insightsurge/hovercraft/issues -->
  and the `url:` line in `_pkgdown.yml`. `tools/check-release.R` fails until both are there.

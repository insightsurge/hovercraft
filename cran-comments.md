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

* `URL` and `BugReports` in `DESCRIPTION`, and `url` in `_pkgdown.yml`, name the repository and the site. While the
  repository is private they return 404, which `--as-cran` reports. `tools/check-release.R` fetches them and fails until
  they load, so make the repository and the site public before submitting.

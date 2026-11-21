## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes for the reviewer

* The package ships a small JavaScript file and a stylesheet in `inst/engine/www/`, with the copyright holder and licence
  listed in `inst/COPYRIGHTS`. They are not minified or obfuscated; the file is the source.
* The vignettes build offline and need no browser. Their pictures are made by hand and kept in `vignettes/figures/`.
* The tests that drive a browser (chromote) and the one that starts a Shiny app skip on CRAN.
* Nothing is written outside `tempdir()`, and no network is used.

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

* TODO(founder): the maintainer address in `DESCRIPTION` is a GitHub `noreply` address, which cannot receive CRAN's
  confirmation mail. Use an address that can.
* TODO(founder): `URL` and `BugReports` point at a repository and a site that return 404 until they are public. Make them
  public, or remove the links.

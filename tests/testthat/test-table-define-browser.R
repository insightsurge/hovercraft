skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("callr")
skip_if_not_installed("httpuv")
skip_if_not_installed("pkgload")
skip_if_not_installed("withr")
skip_if_not_installed("DT")
skip_if_not_installed("reactable")
skip_if_not_installed("shinyjs")
skip_if_not_installed("promises")
skip_if_not_installed("later")
skip_if(is.null(chromote::find_chrome()), "No Chrome on this machine")
skip_if_no_bench()

# A DT table draws its rows, cells and header cells while it is built, before the widget's render hook runs. The
# templates its tips use must already be defined by then, or the engine warns on the console that a template "is not
# defined" for every tip, although the tips work afterwards.
describe("tip_table() on a DT table in Shiny", {
  bench <- start_bench(environment())
  # Keeps the page's console warnings from its first script on, which `local_page()` does not record.
  keep_warnings <- "window.__warnings = [];
    const warn = console.warn.bind(console);
    console.warn = (...args) => { window.__warnings.push(args.map(String).join(' ')); warn(...args); };"
  warnings_of <- function(tab) {
    url <- paste0(bench$url, "/?only=", tab)
    page <- local_page(url, environment(), init = keep_warnings)
    expect_true(page$wait_until("window.Shiny?.shinyapp?.isConnected?.() === true"))
    expect_true(page$wait_until("document.querySelector('table.dataTable tbody tr[data-hovercraft-tip-id]') !== null"))
    page$settle()
    jsonlite::fromJSON(page$evaluate("JSON.stringify(window.__warnings)"))
  }

  for (tab in c("tables", "layering")) {
    it(paste("defines every template before the table uses it:", tab, "tab"), {
      expect_identical(grep("is not defined", warnings_of(tab), value = TRUE), character())
    })
  }
})

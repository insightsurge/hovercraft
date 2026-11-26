# The test bench (apps/testbench) without a browser: its scenario list matches its spots, and every tab draws and
# starts its server. The browser tests that use the bench are in test-*-browser.R.

skip_if_no_bench()
skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("reactable")
skip_if_not_installed("shinyjs")
skip_if_not_installed("promises")
skip_if_not_installed("later")

# The bench's R files in an environment of their own, as Shiny loads them, read from the bench's folder.
load_bench <- function(env = parent.frame()) {
  bench <- new.env(parent = globalenv())
  withr::local_dir(bench_dir(), .local_envir = env)
  for (file in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) {
    sys.source(file, envir = bench)
  }
  bench
}

# The ids of every spot a page draws.
drawn_spots <- function(html) {
  regmatches(html, gregexpr("data-scenario=\"HC-[0-9]{3}\"", html))[[1]] |>
    sub(pattern = "data-scenario=\"(HC-[0-9]{3})\"", replacement = "\\1")
}

describe("the test bench's scenario list", {
  it("lists HC-001 to HC-077 once each, each on a tab the bench has, with a known kind and coverage", {
    bench <- load_bench()
    scenarios <- bench$bench_scenarios()
    expect_identical(scenarios$id, sprintf("HC-%03d", 1:77))
    expect_true(all(scenarios$tab %in% bench$bench_tabs$id))
    expect_true(all(scenarios$kind %in% c("browser", "r-only", "human")))
    expect_true(all(scenarios$coverage %in% c("covered", "partly", "none")))
    expect_true(all(nzchar(scenarios$steps)))
  })

  it("gives every scenario one spot, on the tab the list names", {
    bench <- load_bench()
    scenarios <- bench$bench_scenarios()
    for (tab in bench$bench_tabs$id) {
      html <- as.character(get(paste0("tab_", tab, "_ui"), envir = bench)())
      expect_setequal(drawn_spots(html), scenarios$id[scenarios$tab == tab])
    }
  })

  it("stops on a spot for an id the list does not have", {
    bench <- load_bench()
    expect_error(bench$spot("HC-999"), "No scenario HC-999")
  })
})

describe("the test bench's page", {
  it("reads its address, and falls back for a value it does not know", {
    bench <- load_bench()
    settings <- bench$bench_query("?only=js&keys=no_pin&max_pins=3&dir=rtl&theme=nope&tab=nope")
    expect_identical(settings$only, "js")
    expect_identical(settings$tab, "js")
    expect_false(settings$strip)
    expect_identical(settings$keys, "no_pin")
    expect_identical(settings$max_pins, "3")
    expect_identical(settings$dir, "rtl")
    expect_identical(settings$theme, "light")
    expect_identical(bench$bench_query("")$tab, "practice")
    expect_true(bench$bench_query("")$strip)
  })

  it("writes only the use_hovercraft() settings the address gives", {
    bench <- load_bench()
    plain <- bench$bench_hovercraft(bench$bench_query(""))
    expect_identical(plain, use_hovercraft())
    set <- bench$bench_hovercraft(bench$bench_query("?keys=off&max_pins=2&labels=custom&style=sober"))
    expect_identical(
      set,
      use_hovercraft(style = tip_style("sober"), max_pins = 2L, keys = FALSE, labels = bench$bench_custom_labels)
    )
  })

  it("draws the whole bench, one tab alone, and the iframe host", {
    bench <- load_bench()
    whole <- as.character(bench$bench_ui(list(QUERY_STRING = "")))
    for (tab in bench$bench_tabs$id) {
      expect_match(whole, sprintf("data-value=\"%s\"", tab), fixed = TRUE)
    }
    alone <- as.character(bench$bench_ui(list(QUERY_STRING = "?only=theme")))
    expect_match(alone, "data-value=\"theme\"", fixed = TRUE)
    expect_no_match(alone, "data-value=\"tables\"", fixed = TRUE)
    host <- as.character(bench$bench_ui(list(QUERY_STRING = "?host=1&only=js")))
    expect_match(host, "src=\"/?only=js&amp;strip=0\"", fixed = TRUE)
  })

  it("starts every tab's server with no error", {
    bench <- load_bench()
    expect_no_error(shiny::testServer(bench$bench_server, session$flushReact()))
  })
})

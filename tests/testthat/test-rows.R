describe("tip_rows()", {
  it("gives one named list of values per row, with the classes kept", {
    data <- data.frame(label = c("ADAE", "ADSL"), n = c(3, 0), when = as.Date(c("2026-01-02", "2026-03-04")))
    data$f <- factor(c("a", "b"))
    got <- tip_rows(data)
    expect_length(got, 2L)
    expect_named(got[[1]], c("label", "n", "when", "f"))
    expect_identical(got[[2]]$label, "ADSL")
    expect_identical(got[[1]]$when, as.Date("2026-01-02"))
    expect_identical(got[[2]]$f, factor("b", levels = c("a", "b"))[1L])
  })

  it("keeps only the columns asked for", {
    data <- data.frame(label = c("a", "b"), n = 1:2, secret = c("x", "y"))
    got <- tip_rows(data, columns = c("label", "n"))
    expect_named(got[[1]], c("label", "n"))
    expect_identical(got[[2]]$n, 2L)
    expect_named(tip_rows(data)[[1]], c("label", "n", "secret"))
  })

  it("refuses a column the data does not have, and a data-frame or matrix column", {
    data <- data.frame(label = c("a", "b"))
    expect_error(tip_rows(data, columns = "nope"), class = "hovercraft_error_record")
    data$m <- matrix(1:4, nrow = 2)
    expect_error(tip_rows(data), class = "hovercraft_error_record")
    expect_no_error(tip_rows(data, columns = "label"))
  })

  it("keeps a list-column cell whole", {
    data <- data.frame(id = 1:2)
    data$cell <- list(1:3, "x")
    expect_identical(tip_rows(data)[[1]]$cell, 1:3)
  })

  it("gives an empty list for no rows, and needs a data frame", {
    expect_identical(tip_rows(data.frame(a = 1)[0, , drop = FALSE]), list())
    expect_error(tip_rows(list(a = 1)), "data")
  })
})

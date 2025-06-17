library(testthat)

# Check if get_info throws an error for invalid input class
test_that("Throws an error for invalid input class in get_info", {
  expect_error(get_info(NULL), "dat must be a raw_data object.")
})

# Check if get_info returns info without group
test_that("Returns info without group", {
  dat <- structure(
    .Data  = data.frame(`sample type` = c("A", "B", "A"),
                        check.names = FALSE),
    samples = data.frame(`sample type` = c("A", "B"),
                         count = c(2, 1),
                         check.names = FALSE),
    NA_info = list(counts = data.frame(type = c(">", "<"),
                                       n = c(2, 43))),
    class = c("raw_data", "data.frame")
    )
  expect_identical(get_info(dat),
                   "Data contains 2 sample types and 2 NA types.")
})

test_that("Prints info with group", {
  dat <- structure(
    .Data  = data.frame(`sample type` = c("A", "B", "A"),
                        group = c(1, 2, 3),
                        check.names = FALSE),
    samples = data.frame(`sample type` = c("A", "B"),
                         count = c(2, 1),
                         check.names = FALSE),
    NA_info = list(counts = data.frame(type = c(">", "<"),
                                       n = c(2, 43))),
    group = "group",
    class = c("raw_data", "data.frame")
    )
  expect_identical(get_info(dat),
                   "Data contains 2 sample types and 2 NA types.\nGroupping by: \"group\" (3 levels).")
})

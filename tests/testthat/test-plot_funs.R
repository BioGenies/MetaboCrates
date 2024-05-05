library(testthat)

test_that("Function stops if data don't have a group", {
  dat <- tibble(group = 1:3)
  expect_error(plot_groups(dat))
})

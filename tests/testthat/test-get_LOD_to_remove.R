library(testthat)

# Check if function returns correct metabolite names
test_that("Returns metabolite names", {
  NA_info <- list(NA_ratios = tibble(metabolite = rep(c("C0", "C1", "C2"), each = 3),
                                     group = rep(1:3, 3),
                                     NA_frac = c(0, 0.1, 0, 0, 0, 0, 0.2, 0.3, 0.2)))
  expect_identical(get_LOD_to_remove(NA_info, 0.01),
                   "C2")
})

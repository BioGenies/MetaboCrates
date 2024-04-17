library(testthat)

test_that("calc_CV_for_QC calculates CV for different QC sample types and metabolites", {
  dat <- structure(
    .Dat = tibble(`sample type` = c("Sample", "QC Level 1", "QC Level 2", "Sample", "QC Level 1"),
                  group = c(1, 2, 2, 1, 3),
                  C0 = c(0.1, 0.3, 0.1, 0.1, 0),
                  C1 = rep(0, 5),
                  C2 = c(0.3, 0.2, 0.2, 0, 0.1)),
    metabolites = c("C0", "C1", "C2")
    )
  expect_equal(calc_CV_for_QC(dat),
               tibble())
})
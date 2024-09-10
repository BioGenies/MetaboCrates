library(testthat)

test_that("function calculates CV for different QC sample types and metabolites", {
  dat <- structure(
    .Data = list(),
    completed = tibble(`sample type` = c("Sample", "QC Level 1", "QC Level 2",
                                    "Sample", "QC Level 1"),
                  group = c(1, 2, 2, 1, 3),
                  C0 = c(0.1, 0.3, 0.1, 0.1, 0),
                  C1 = rep(0, 5),
                  C2 = c(0.3, 0.2, 0.2, 0, 0.1)),
    metabolites = c("C0", "C1", "C2")
    )

  expect_equal(calculate_CV(dat), {
    attr(dat, "cv") <- tibble(`sample type` = "QC Level 1",
                              metabolite = c("C0", "C2"),
                              CV = c(1.414, 0.471))
    dat
    }, tolerance = 0.001)
})

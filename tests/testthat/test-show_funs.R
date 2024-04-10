library(testthat)

dat <- structure(
  .Dat = tibble(`plate bar code` = c(1, 2),
                C0 = c(0.001, "< LOD"),
                C1 = c(0.3, 0.02),
                C2 = c(NA, "< LLOQ"),
                C3 = c("< LOD", 0)),
  removed = list(LOD = c("C0", "C2"),
                 QC = "C3")
)

test_that("show_data returns data with correct metabolites", {
  expect_equal(show_data(dat),
               structure(
                 .Dat = tibble(`plate bar code` = c(1, 2),
                               C1 = c(0.3, 0.02)),
                 removed = list(LOD = c("C0", "C2"),
                                QC = "C3")))
})

test_that("show_ratios returns NA ratios with correct metabolites",{
  attr(dat, "NA_info")$NA_ratios <- tibble(metabolite = rep(c("C0", "C1", "C2", "C3"), each = 3),
                                           group = rep(1:3, 4),
                                           NA_frac = c(0,0,0,0.1,0,0,0.1,0,0.222,1,0,0))
  
  expect_equal(show_ratios(dat),
               tibble(metabolite = rep("C1", 3),
                      group = 1:3,
                      NA_frac = c(0.1,0,0)))
})
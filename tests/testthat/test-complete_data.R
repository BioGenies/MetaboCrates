library(testthat)


path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)

dat_completed <- complete_data(dat, LOD_method = "limit", 
                               LLOQ_method = "limit", 
                               ULOQ_method = "limit")

completed <- attributes(dat_completed)[["completed"]]


test_that("Attribute completed is added to the data", {
  expect_true(all(dim(completed) == c(41, 27)))
  expect_s3_class(completed, c("raw_data", "data.frame"))
})

test_that("All the values are completed with limit method", {
  expect_false(any(completed == "< LOD", na.rm = TRUE))
  expect_false(any(completed == "< LLOQ", na.rm = TRUE))
  expect_false(any(completed == "> ULOQ", na.rm = TRUE))
})

test_that("Proper LOD values were imputed with limit method", {
  expect_equal(completed[2, 21], 0.17)
  expect_equal(completed[3, 21], 0.17)
  expect_equal(completed[2, 22], 0.019)
  expect_equal(completed[1, 23], NA_real_)
})

test_that("Proper LLOQ values were imputed with limit method", {
  expect_equal(completed[19, 22], 0.4)
  expect_equal(completed[19, 24], 0.1)
})

test_that("Proper ULOQ values were imputed with limit method", {
  expect_equal(completed[28, 19], 100)
  expect_equal(completed[39, 20], 10
  )
})


dat_completed <- complete_data(dat, LOD_method = "halfmin")
completed <- attributes(dat_completed)[["completed"]]

test_that("Proper LOD values were imputed with halfmin method", {
  expect_equal(completed[2, 21], Inf)
  expect_equal(completed[3, 21], Inf)
  expect_equal(completed[6, 26], 0.0525)
})


dat_completed <- complete_data(dat, LOD_method = "halflimit")
completed <- attributes(dat_completed)[["completed"]]

test_that("Proper LOD values were imputed with halflimit method", {
  expect_equal(completed[2, 21], 0.085)
  expect_equal(completed[3, 22], 0.0095)
  expect_equal(completed[6, 26], 0.0435)
})


dat_completed <- complete_data(dat, LOD_method = "random")
dat_completed <- attributes(dat_completed)[["completed"]]

test_that("Imputation of only LODs", {
  expect_equal(sum(is.na(dat_completed)), 198)
})

dat_completed <- complete_data(dat, LLOQ_method = "limit")
dat_completed <- attributes(dat_completed)[["completed"]]

test_that("Imputation of only LODs", {
  expect_equal(sum(is.na(dat_completed)), 324)
})

dat_completed <- complete_data(dat, ULOQ_method = "limit")
dat_completed <- attributes(dat_completed)[["completed"]]

test_that("Imputation of only ULOQs", {
  expect_equal(sum(is.na(dat_completed)), 319)
})







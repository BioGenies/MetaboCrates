library(testthat)


path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)

dat_completed <- complete_data(dat, LOD_method = "limit", 
                               LLOQ_method = "limit", 
                               ULOQ_method = "limit")

completed <- attributes(dat_completed)[["completed"]]

not_missing_dat <- structure(
  tibble(
    `sample type` = c("Sample", "Sample", "QC"),
    `plate bar code` = "1|1",
    `sample identification` = c("a", "b", "a"),
    C0 = c("0.5", "1", "NA"),
  ),
  NA_info = list(
    counts = tibble(type = character(0), n = double(0))
  ),
  LOD_table = tibble(
    `plate bar code` = c("LOD (calc.) 1|1", "LLOQ 1|1", "ULOQ 1|1"),
    C0 = c(0, 0.2, 3),
    type = c("LOD (calc.)", "LLOQ", "ULOQ")
  ),
  metabolites = "C0"
)

test_that("Message is shown when there are no missing values", {
  expect_message(complete_data(not_missing_dat), "No missing values found.")
})

test_that("Message is shown when there are no missing values of some type", {
  not_missing_dat[1, 4] <- "< LLOQ"
  attr(not_missing_dat, "NA_info")[["counts"]] <- 
    attr(not_missing_dat, "NA_info")[["counts"]] %>%
    add_row(type = "< LLOQ", n = 1)
  
  expect_message(complete_data(not_missing_dat), "No < LOD values found.")
  expect_message(complete_data(not_missing_dat), "No > ULOQ values found.")
  
  not_missing_dat[1, 4] <- "< LOD"
  attr(not_missing_dat, "NA_info")[["counts"]][1,1] <- "< LOD"
  
  expect_message(complete_data(not_missing_dat), "No < LLOQ values found.")
})

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


dat_completed <- complete_data(dat, ULOQ_method = "third quartile")
completed <- attributes(dat_completed)[["completed"]]

test_that("Proper ULOQ values were imputed with third quartile method", {
  expect_equal(completed[30, 19], 10.3)
  expect_equal(completed[17, 26], 0.238)
})


dat_completed <- complete_data(dat, LOD_method = "halfmin")
completed <- attributes(dat_completed)[["completed"]]

test_that("Proper LOD values were imputed with halfmin method", {
  expect_equal(completed[2, 21], NA_real_)
  expect_equal(completed[3, 21], NA_real_)
  expect_equal(completed[6, 26], 0.0525)
})


dat_completed <- complete_data(dat, LOD_method = "halflimit")
completed <- attributes(dat_completed)[["completed"]]

test_that("Proper LOD values were imputed with halflimit method", {
  expect_equal(completed[2, 21], 0.085)
  expect_equal(completed[3, 22], 0.0095)
  expect_equal(completed[6, 26], 0.0435)
})


dat_completed <- complete_data(dat, LOD_method = "limit-0.2min")
completed <- attributes(dat_completed)[["completed"]]

test_that("Proper LOD values were imputed with limit-0.2min method", {
  expect_equal(completed[2, 25], 0.081)
  expect_equal(completed[7, 26], 0.066)
})


dat_completed <- complete_data(dat, LOD_method = "logspline")
completed <- attributes(dat_completed)[["completed"]]

test_that("Proper LOD values were imputed with logspline method", {
  expect_equal(completed[5, 23], as.double(NA))
  expect_equal(round(completed[7, 26], 6), 0.114104)
})


dat_completed <- complete_data(dat, LOD_method = "random")
dat_completed <- attributes(dat_completed)[["completed"]]

test_that("Imputation of only LODs", {
  expect_equal(sum(is.na(dat_completed)), 198)
})

dat_completed <- complete_data(dat, LLOQ_method = "limit")
dat_completed <- attributes(dat_completed)[["completed"]]

test_that("Imputation of only LLOQs", {
  expect_equal(sum(is.na(dat_completed)), 324)
})

dat_completed <- complete_data(dat, ULOQ_method = "limit")
dat_completed <- attributes(dat_completed)[["completed"]]

test_that("Imputation of only ULOQs", {
  expect_equal(sum(is.na(dat_completed)), 319)
})

test_that("Error is thrown when the wrong LOD type is given", {
  expect_error(
    complete_data(dat, LOD_method = "limit", LOD_type = "OP"),
    "There is no OP values in LOD table."
  )
})


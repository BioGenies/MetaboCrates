library(testthat)

path <- get_example_data("small_biocrates_example.xls")
test_dat <- read_data(path)

dat_grp <- add_group(test_dat, "group")

test_that("Group is added to raw_data class.", {
  expect_true(attr(dat_grp, "group") == "group")
})

test_that("Wrong group name throws an error", {
  expect_error(
    add_group(test_dat, "wrong_name"),
    "Some of the provided columns: wrong_name, can't be found in your data!"
  )
})

test_that("Existing group throws a warning", {
  expect_warning(add_group(dat_grp, "plate bar code"), 
                 "You already have grouping defined in your data. It will be replaced!")
})

test_that("Existing group throws a warning", {
  expect_error(add_group(test_dat, "org. info"), 
                 "Grouping columns should not contain any NA's!")
})

test_that("Groups aren't too small.", {
  expect_error(add_group(test_dat, "well position"), 
               "Some group levels have less than 2 observations.")
})


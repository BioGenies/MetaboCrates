library(testthat)

path <- get_example_data("small_biocrates_example.xls")
test_dat <- read_data(path)

# Check if function returns correct metabolite names

test_that("Returns correct metabolite names", {
  
  expect_message(get_LOD_to_remove(test_dat, 0.01, TRUE), 
                 "No group to use! It will be ignored.")
  
  expect_identical(get_LOD_to_remove(test_dat, 0.8, FALSE), 
                   c("C3-DC (C4-OH)", "C3-OH", "C4:1"))
  
  test_dat <- add_group(test_dat, "group")
  
  expect_identical(get_LOD_to_remove(test_dat, 0.2, TRUE), 
                   c("C3-DC (C4-OH)", "C3-OH", "C3:1", "C4:1"))
})

library(testthat)

path <- get_example_data("small_biocrates_example.xls")
test_dat <- read_data(path)

test_that("remove_metabolites adds metabolites to the 'removed' attribute", {
  metabolites_to_remove <- c("C0", "C2")
  type <- "LOD"
  
  modified_dat <- remove_metabolites(test_dat, metabolites_to_remove, type)
  
  expect_equal(attr(modified_dat, "removed")[[type]], metabolites_to_remove)
})

test_that("remove_metabolites throws error when some of the provided metabolites
          are not present in the data", {
  expect_error(
    remove_metabolites(test_dat, c("C0", "C1"), "LOD"),
    "Some of the provided metabolites are not present in the data."
  )
})

test_that("remove_metabolites throws error when some of the provided metabolites
          are already removed", {
            rm_dat <- remove_metabolites(test_dat, "C0", "LOD")
            
            expect_error(
              remove_metabolites(rm_dat, c("C0", "C2"), "LOD"),
              "Some of the provided metabolites have already been removed."
            )
          })

test_that("unremove_all removes all metabolites from the 'removed' attribute for
          the correct type", {
  metabolites_to_remove <- c("C0", "C2")
  type <- "LOD"
  test_dat <- remove_metabolites(test_dat, metabolites_to_remove, type)
  
  modified_dat <- unremove_all(test_dat, type)
  
  expect_equal(attr(modified_dat, "removed")[[type]], NULL)
})

test_that("unremove_metabolites removes correct metabolites from the 'removed'
          attribute", {
  test_dat <- structure(list(), removed = list(
    LOD = c("C0", "C1", "C2", "C3", "C4"),
    QC = c("C0", "C2")))
  
  unremoved_dat <- structure(list(), removed = list(
    LOD = c("C1", "C3"),
    QC = NULL))
  
  expect_identical(unremove_metabolites(test_dat, c("C0", "C2", "C4")),
                   unremoved_dat)
})

test_that("calculate_CV throws error when data isn't completed", {
  test_dat <- list()
    
  expect_error(
    calculate_CV(test_dat),
    "Complete data first."
  )
})

test_that("get_CV_to_remove throws error when CVs aren't calculated", {
  test_dat <- list()
  
  expect_error(
    get_CV_to_remove(test_dat),
    "First, calculate the coefficient of variation using calculate_CV()."
  )
})

test_that("get_CV_to_remove returns correct metabolites", {
  test_dat <- structure(
    list(),
    cv = tibble(
      `sample type` = c("QC1", "QC2", "QC1", "QC2"),
      metabolite = c("C0", "C0", "C1", "C1"),
      CV = c(0.01, 0.5, 0.2, 0.004)
    )
  )
  
  expect_equal(
    get_CV_to_remove(test_dat, threshold = 0.3),
    "C0"
  )
})


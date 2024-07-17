library(testthat)

path <- get_example_data("small_biocrates_example.xls")
test_dat <- read_data(path)

test_that("remove_metabolites dodaje metabolity do atrybutu removed", {
  metabolites_to_remove <- c("C0", "C1", "C2")
  type <- "LOD"
  
  modified_dat <- remove_metabolites(test_dat, metabolites_to_remove, type)
  
  expect_equal(attr(modified_dat, "removed")[[type]], metabolites_to_remove)
})


test_that("unremove_all usuwa metabolity z atrybutu removed", {
  metabolites_to_remove <- c("C0", "C1", "C2")
  type <- "LOD"
  test_dat <- remove_metabolites(test_dat, metabolites_to_remove, type)
  
  modified_dat <- unremove_all(test_dat, type)
  
  expect_equal(attr(modified_dat, "removed")[[type]], NULL)
})

test_that("unremove_metabolites usuwa odpowiednie metabolity
          z atrybutu removed", {
  test_dat <- structure(list(), removed = list(
    LOD = c("C0", "C1", "C2", "C3", "C4"),
    QC = c("C0", "C2"),
    QC_man = c("C1", "C2")))
  
  unremoved_dat <- structure(list(), removed = list(
    LOD = c("C1", "C3"),
    QC = NULL,
    QC_man = c("C1")))
  
  expect_identical(unremove_metabolites(test_dat, c("C0", "C2", "C4")),
                   unremoved_dat)
})

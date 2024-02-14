library(testthat)

path <- get_example_data("small_biocrates_example.xls")

# test if conditions work

test_that("read_data throws error for wrong path", {
  expect_error(read_data("this_file_doesnt_exist.xlsx"), 
               "The file this_file_doesnt_exist.xlsx does not exist. Check if the path is correct.")
})

test_that("read_data throws error for wrong file extension", {
  expect_error(read_data(get_example_data("wrong_ext.txt")), 
               "File extension should be xls or xlsx, not txt.")
})

# test if the data is correct

test_dat <- read_data(path)
metabolites <- attr(test_dat, "metabolites")
LOD_table <- attr(test_dat, "LOD_table")

test_that("Column names are correct", {
  expect_equal(
    colnames(test_dat),
    c("plate bar code", "sample bar code", "sample type", "sample identification", 
      "collection date", "species", "material", "op", "group", "org. info", 
      "plate production no.", "plate note", "well position", "sample volume", 
      "run number", "injection number", "measurement time", "C0", "C2", 
      "C3", "C3-DC (C4-OH)", "C3-OH", "C3:1", "C4", "C4:1", "C5")
  )
})

test_that("Values in metabolomics matrix are correct", {
  expect_equal(
    test_dat[, "C0"], 
    c(NA, "45.100000000000001", "46.200000000000003", "NA", "43.299999999999997", 
      "30.100000000000001", "31.5", "38.600000000000001", "46.299999999999997", 
      NA, "48.299999999999997", "41.799999999999997", "52", NA, "23.399999999999999", 
      "69.700000000000003", "36.399999999999999", "53.200000000000003", 
      "40.799999999999997", "44.600000000000001", "40.399999999999999", 
      "30.100000000000001", NA, NA, "52.700000000000003", "42.799999999999997", 
      "42.200000000000003", "52.100000000000001", "38.399999999999999", 
      "45", "44.200000000000003", "< LOD", "42.799999999999997", "36.600000000000001", 
      "54.899999999999999", "34.399999999999999", "39.799999999999997", 
      "38.5", "74", "36.299999999999997", "26.199999999999999")
  )
})

test_that("Metabolites are correct", {
  expect_true(all(metabolites %in% colnames(test_dat)))
})

test_that("Names of columns in LOD_table. are proper.", {
  expect_true(all(colnames(LOD_table) %in% colnames(test_dat)))
})

test_that("Measurement time is in LOD_table.", {
  expect_true(colnames(LOD_table)[1] == "measurement time")
})

test_that("LOD_table columns are numeric", {
  lapply(2:ncol(LOD_table), function(ith_col) {
    expect_true(is.numeric(LOD_table[, ith_col]))
  })
})

test_that("Values in LOD_table are correct.", {
  expect_equal(LOD_table[, "C0"], c(0, 3.1, NA, NA, NA, NA))
  expect_equal(LOD_table[, "C3-DC (C4-OH)"], c(0, 0.17, NA, NA, 80, NA))
  expect_equal(LOD_table[, "measurement time"], c("LOD (calc.) 1036372121/1 [µM]", 
                                                  "LOD (calc.) 1036372116/1 [µM]", 
                                                  "LLOQ 1036372121/1 [µM]", 
                                                  "LLOQ 1036372116/1 [µM]", 
                                                  "ULOQ 1036372121/1 [µM]", 
                                                  "ULOQ 1036372116/1 [µM]"))
})









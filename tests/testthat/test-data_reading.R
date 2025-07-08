library(testthat)

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

path <- get_example_data("small_biocrates_example.xls")
test_dat <- read_data(path)

test_that("raw_data class is correct.", {
  expect_true(inherits(test_dat, c("raw_data", "data.frame")))
})

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
      NA, NA, "41.799999999999997", "52", NA, "23.399999999999999", 
      "69.700000000000003", "36.399999999999999", NA, "40.799999999999997", 
      "44.600000000000001", "40.399999999999999", "30.100000000000001", 
      NA, NA, "52.700000000000003", "42.799999999999997", "42.200000000000003", 
      "52.100000000000001", "38.399999999999999", "45", "44.200000000000003", 
      "< LOD", "42.799999999999997", "36.600000000000001", "54.899999999999999", 
      "34.399999999999999", "39.799999999999997", "38.5", "74", 
      "36.299999999999997", "26.199999999999999")
  )
})

# test if there are no duplicated column names

test_that("read_data throws error for duplicated column names", {
  expect_error(
    read_data(get_example_data("wrong_column_names.xlsx")),
    "Found duplicated column names."
  )
})

# attributes:

## metabolites
metabolites <- attr(test_dat, "metabolites")

test_that("Metabolites are correct", {
  expect_true(all(metabolites %in% colnames(test_dat)))
  expect_true(is.character(metabolites))
})

## LOD_table
LOD_table <- attr(test_dat, "LOD_table")

test_that("LOD_table class is correct.", {
  expect_true(inherits(LOD_table, "data.frame"))
})

test_that("Names of columns in LOD_table are proper.", {
  expect_true(all(c(colnames(LOD_table)) %in% c(colnames(test_dat), "type")))
})

test_that("Measurement time is in LOD_table.", {
  expect_true(colnames(LOD_table)[1] == "plate bar code")
})

test_that("LOD_table columns are numeric", {
  cols <- setdiff(colnames(LOD_table), 
                  c("plate bar code", "type"))
  lapply(cols, function(ith_col) {
    expect_true(is.numeric(LOD_table[, ith_col]))
  })
})

test_that("Values in LOD_table are correct.", {
  expect_equal(LOD_table[, "C0"], c(0, 3.1, NA, NA, NA, NA))
  expect_equal(LOD_table[, "C3-DC (C4-OH)"], c(0, 0.17, NA, NA, 80, NA))
  expect_equal(LOD_table[, "plate bar code"], c("LOD (calc.) 1036372121/1 [µM]", 
                                                "LOD (calc.) 1036372116/1 [µM]", 
                                                "LLOQ 1036372121/1 [µM]", 
                                                "LLOQ 1036372116/1 [µM]", 
                                                "ULOQ 1036372121/1 [µM]", 
                                                "ULOQ 1036372116/1 [µM]"))
})

## NA_info
NA_info <- attr(test_dat, "NA_info")
NA_ratios_type <- NA_info[["NA_ratios_type"]]
counts <- NA_info[["counts"]]

test_that("NA_info object is correct.", {
  expect_true(length(NA_info) == 3)
  expect_true(inherits(NA_info, "list"))
})

test_that("NA_ratios_type is valid.", {
  expect_true(!is.null(NA_ratios_type))
  expect_true(inherits(NA_ratios_type, "data.frame"))
  expect_true(all(colnames(NA_ratios_type) %in% c("metabolite", "type", "NA_frac")))
  
})

test_that("counts object is valid.", {
  expect_identical(counts, 
                   structure(list(type = c("< LOD", "> ULOQ", "< LLOQ"), 
                                  n = c(109, 9, 6)), 
                             class = c("tbl_df", "tbl", "data.frame"), 
                             row.names = c(NA, -3L)))
})

test_dat_grp <- add_group(test_dat, "group")
NA_ratios_group <- attr(test_dat_grp, "NA_info")[["NA_ratios_group"]]

test_that("NA_ratios_group is valid.", {
  expect_true(!is.null(NA_ratios_group))
  expect_true(inherits(NA_ratios_group, "data.frame"))
  expect_true(all(colnames(NA_ratios_group) %in% c("metabolite", "grouping_column", "NA_frac")))
})


## samples
samples <- attr(test_dat, "samples")

test_that("samples object is valid.", {
  expect_identical(
    samples, 
    structure(
      list(`sample type` = c("Blank", "QC Level 1", "QC Level 2", 
                             "QC Level 3", "Sample", "Standard L2", 
                             "Standard L3", "Standard L4", 
                             "Standard L6", "Standard L7", "Zero Sample"), 
           count = c(1L, 1L, 2L, 1L, 30L, 1L, 1L, 1L, 1L, 1L, 1L)), 
      class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -11L)))
})


test_that("sample identification column should be unique", {
  data <- tibble(
    `sample identification` = c("A", "B", "A"),
    `sample type` = c("Sample", "Sample", "Sample")
  )
  expect_error(MetaboCrates:::validate_raw_data(data), "Sample identification column has to be unique.")
})

test_that("incorrect metabolites values should be detected", {
  raw_data <- tibble(
    `sample identification` = c("A", "B", "C"),
    `sample type` = c("Sample", "Sample", "Sample"),
    metabolite_1 = c(1, NA, "invalid"),
    metabolite_2 = c(2, "< LOD", 3)
  )
  attr(raw_data, "metabolites") <- c("metabolite_1", "metabolite_2")
  expect_error(MetaboCrates:::validate_raw_data(raw_data), "Found incorrect metabolites values!")
})

test_that("required columns should be present", {
  raw_data <- tibble(
    `sample identification` = c("A", "B", "C"),
    `sample type` = c("Sample", "Sample", "Sample")
  )
  expect_error(MetaboCrates:::validate_raw_data(raw_data), "Data should contain row/s:")
})

test_that("quality control samples should be present", {
  raw_data <- tibble(
    `sample identification` = c("A", "B", "C"),
    `sample type` = c("Sample", "Sample", "Sample"),
    `plate bar code` = c(1, 2, 3),
    `measurement time` = c(1, 2, 3)
  )
  expect_error(MetaboCrates:::validate_raw_data(raw_data), "Data should contain quality control samples.")
})

test_that("quality control samples should not contain missing values", {
  raw_data <- tibble(
    `plate bar code` = c(1, 2, 3),
    `sample identification` = c("A", "B", "C"),
    `sample type` = c("Sample", "QC", "QC"),
    `measurement time` = c(1, 2, 3),
    metabolite_1 = c(1, NA, 3)
  )
  attr(raw_data, "metabolites") <- "metabolite_1"
  expect_error(MetaboCrates:::validate_raw_data(raw_data), "Quality contriol samples should not contain missing values!")
})

test_that("metabolites names should match", {
  raw_data <- dplyr::tibble(
    `plate bar code` = c(1, 2, 3),
    `sample identification` = c("A", "B", "C"),
    `sample type` = c("QC", "Sample", "Sample"),
    `measurement time` = c(1, 2, 3),
    metabolite_1 = c(1, 2, 3)
  )
  attr(raw_data, "metabolites") <- c("metabolite_1", "metabolite_2")
  expect_warning(MetaboCrates:::validate_raw_data(raw_data), "Metabolites")
})



test_that("sample identification column should be unique", {
  metabolomics_matrix <- tibble(
      `sample identification` = c("A", "B", "A"),
      `sample type` = c("Sample", "Sample", "Sample"),
      metabolite1 = c(0.1, "<LOD", 0.1),
      metabolite2 = c(0.05, 0.1, 0.1)
    )
  
  LOD_table <- tibble(
      `plate bar code` = c("Plate1", "Plate2"),
      metabolite1 = c(0.1, 0.2),
      metabolite2 = c(0.05, 0.1),
      type = c("LOD (calc.)", "LOD (calc.)"))
    
  metabolites <- c("metabolite1", "metabolite2")
  
    expect_error(MetaboCrates:::raw_data(metabolomics_matrix, LOD_table, metabolites), "Sample identification column has to be unique.")
  })


test_that("Test for mismatch between provided metabolites and LOD table", {
  metabolomics_matrix <- tibble(
    `sample type` = c("QC", "Sample"),
    `plate bar code` = c("Plate1", "Plate2"),
    `sample identification` = c("Sample1", "Sample2"),
    `measurement time` = c(1, 2),
    metabolite1 = c(0.1, "< LOD"),
    metabolite2 = c(3, 4)
  )

  LOD_table <- tibble(
      `plate bar code` = c("Plate1", "Plate2"),
      metabolite1 = c(0.5, 0.6),
      metabolite2 = c(0.7, 0.8),
    type = c("LOD (calc.)", "LOD (calc.)"))

  metabolites <- c("metabolite1", "metabolite3")

  expect_error(
    suppressWarnings(
      MetaboCrates:::raw_data(metabolomics_matrix, LOD_table, metabolites)
      ),
    "Provided metabolites do not match LOD table!"
  )
})

test_that("Test for group column not contained in the data", {
  metabolomics_matrix <- tibble(
    `sample type` = c("QC", "Sample", "Sample"),
    `plate bar code` = c("Plate1", "Plate2", "Plate3"),
    `sample identification` = c("QC1", "Sample1", "Sample2"),
    `measurement time` = c(1, 2, 3),
    metabolite1 = c(1, "<LOD", 3),
    metabolite2 = c(3, 4, 5),
  )

  LOD_table <-tibble(
    `plate bar code` = c("Plate1", "Plate2"),
    metabolite1 = c(0.5, 0.6),
    metabolite2 = c(0.7, 0.8),
    type = c("LOD (calc.)", "LOD (calc.)"))

  metabolites <- c("metabolite1", "metabolite2")

  expect_error(
    MetaboCrates:::raw_data(metabolomics_matrix, LOD_table, metabolites),
    "Found incorrect metabolites values!"
  )
})


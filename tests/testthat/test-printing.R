library(testthat)

# Check if get_info throws an error for invalid input class
test_that("Throws an error for invalid input class", {
  expect_error(get_info(NULL), "dat must be a raw_data object.")
})

# Check if get_info prints info without group
test_that("Prints info without group", {
  dat <- structure(
    .Data  = data.frame(
    `sample type` = c("A", "B", "A"),
    check.names = FALSE
    ),
    samples = data.frame(
      `sample type` = c("A", "B"),
      count = c(2, 1),
      check.names = FALSE
    ),
    NA_info = list(
      counts = data.frame(
        type = c(">", "<"),
        n = c(2, 43)
      )
    ),
    class = c("raw_data", "data.frame")
  )
  expect_identical(
    capture_output(get_info(dat)),
    paste0("Sample types:\n",
           paste0(capture.output(attr(dat, "samples")), collapse = "\n"),
           "\nShowing 2 out of 2 rows\nNA types:\n",
           paste0(capture.output(attr(dat, "NA_info")$count), collapse = "\n"),
           "\nShowing 2 out of 2 rows")
    )
})

# Check if get_info prints info with group

test_that("Prints info with group", {
  dat <- structure(
    .Data  = data.frame(
      `sample type` = c("A", "B", "A"),
      group = c(1, 2, 3),
      check.names = FALSE
    ),
    samples = data.frame(
      `sample type` = c("A", "B"),
      count = c(2, 1),
      check.names = FALSE
    ),
    NA_info = list(
      counts = data.frame(
        type = c(">", "<"),
        n = c(2, 43)
      )
    ),
    group = "group",
    class = c("raw_data", "data.frame")
  )
  expect_identical(
    capture_output(get_info(dat)),
    paste0("Sample types:\n",
           paste0(capture.output(attr(dat, "samples")), collapse = "\n"),
           "\nShowing 2 out of 2 rows\nNA types:\n",
           paste0(capture.output(attr(dat, "NA_info")$count), collapse = "\n"),
           "\nShowing 2 out of 2 rows\nGroup \"group\" levels:\n",
           paste0(capture.output(data.frame(
             group = 1:3,
             n = rep(1, 3)
           )), collapse = "\n"),
           "\nShowing 3 out of 3 rows"
    )
  )
})

# Check if print throws an error for invalid input class
test_that("Throws an error for invalid input class", {
  expect_error(print(NULL), "dat must be a raw_data object.")
})

# Check if print prints info without group
test_that("Prints info without group", {
  dat <- structure(
    .Data  = data.frame(
      `sample type` = c("A", "B", "A"),
      C1 = c(1, 2, 3),
      C2 = c(3, 2, 1),
      C3 = c(2, 1, 3),
      check.names = FALSE
    ),
    metabolites = c("C1", "C2", "C3"),
    samples = data.frame(
      `sample type` = c("A", "B"),
      count = c(2, 1),
      check.names = FALSE
    ),
    NA_info = list(
      counts = data.frame(
        type = c(">", "<"),
        n = c(2, 43)
      ),
      NA_ratios = tibble(
        metabolite = c("C1", "C1", "C2", "C3"),
        group = c(1, 2, 1, 3),
        NA_frac = c(0, 0.1, 0, 0.01)
      )
    ),
    class = c("raw_data", "data.frame")
  )
  expect_identical(
    capture_output(print(dat)),
    paste0("Metabolites: C1, C2, C3\n",
           paste0(capture.output(dat), collapse = "\n"),
           "\nShowing 3 out of 3 metabolites\nShowing 3 out of 3 rows\nNA ratios:\n",
           paste0(capture.output(attr(dat, "NA_info")$NA_ratios), collapse = "\n"),
           "\nShowing 4 out of 4 rows")
  )
})

# Check if get_info prints info with group

test_that("Prints info with group", {
  dat <- structure(
    .Data  = data.frame(
      `sample type` = c("A", "B", "A"),
      group = c(1, 2, 3),
      C1 = c(1, 2, 3),
      C2 = c(3, 2, 1),
      C3 = c(2, 1, 3),
      check.names = FALSE
    ),
    metabolites = c("C1", "C2", "C3"),
    samples = data.frame(
      `sample type` = c("A", "B"),
      count = c(2, 1),
      check.names = FALSE
    ),
    NA_info = list(
      counts = data.frame(
        type = c(">", "<"),
        n = c(2, 43)
      ),
      NA_ratios = tibble(
        metabolite = c("C1", "C1", "C2", "C3"),
        group = c(1, 2, 1, 3),
        NA_frac = c(0, 0.1, 0, 0.01)
      )
    ),
    group = "group",
    class = c("raw_data", "data.frame")
  )
  expect_identical(
    capture_output(print(dat)),
    paste0("Specified group: group\nMetabolites: C1, C2, C3\n",
           paste0(capture.output(dat), collapse = "\n"),
           "\nShowing 3 out of 3 metabolites\nShowing 3 out of 3 rows\nNA ratios:\n",
           paste0(capture.output(attr(dat, "NA_info")$NA_ratios), collapse = "\n"),
           "\nShowing 4 out of 4 rows")
  )
})

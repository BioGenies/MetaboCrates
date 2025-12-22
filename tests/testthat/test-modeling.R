library(testthat)

test_dat <- structure(
  list(),
  completed = data.frame(
    `sample identification` = 1:5,
    `sample type` = rep("Sample", 5),
    group = c(1, 3, 2, 1, 3),
    group1 = rep(1, 5),
    `C0` = c(0.2, 0.1, 0.0001, 0.05, 0.3),
    check.names = FALSE
  ),
  group = c("group", "group1"),
  metabolites = "C0"
)

test_that("get_modeling_data throws error when the data is not completed", {
  test_dat <- structure(list())
  expect_error(build_model(test_dat), "Complete data first.")
})

test_that(
  "get_modeling_data throws error when the response has more than three levels
   and no target level was specified", {
     expect_error(
       build_model(test_dat, "group"),
       "A specific level must be given when the response variable has three or more levels."
     ) 
   }
)

test_that("get_modeling_data throws error when the specified level is missing", {
  expect_error(
    build_model(test_dat, "group", "4"),
    "Given level wasn't found in a response variable."
  ) 
})

test_that(
  "get_modeling_data throws error when the response variable has only one level",
  {
    expect_error(
      build_model(test_dat, "group1", "1"),
      "Response variable has only one level."
    )
  }
)

test_that(
  "get_modeling_data throws error when too many observations have
  missing values", 
  {
    attr(test_dat, "completed") <- cbind(attr(test_dat ,"completed"),
                                         `C1` = rep(NA, 5))
    attr(test_dat, "metabolites") <- c("C0", "C1")
    expect_error(
      build_model(test_dat, "group", "3"),
      "Too many observations with missing values."
    ) 
  }
)

test_that(
  "get_modeling_data throws error when the number of unique values for any level
  is too small",
  {
    attr(test_dat, "completed") <- attr(test_dat, "completed")[1:3,]
    expect_error(
      build_model(test_dat, "group", "2"),
      "Provided response variable can't be used - too small number of unique values in levels."
    )
  }
)

test_that(
  "get_modeling_data throws error when the number of unique values for the
  target level is too small",
  {
    expect_error(
      build_model(test_dat, "group", "2"),
      "Provided level can't be used - too small number of unique values in this level."
    )
  }
)

test_that(
  "predict_probability throws error when there are no observations without 
  missing values",
  {
    model <- list()
    pred_dat <- data.frame()
    expect_error(
      predict_probability(model, pred_dat),
      "No observations without missing values found."
    )
  }
)

test_that(
  "predict_probability throws error when some of the predictors are missing",
  {
    model <- list(
      model = list(
        beta = data.frame(1, row.names = "C1")
      )
    )
    class(model[["model"]]) <- c("list", "glmnet")
    pred_dat <- data.frame("C0" = 1)
    expect_error(
      predict_probability(model, pred_dat),
      "Some of the predictors are missing from the new dataset."
    )
  }
)

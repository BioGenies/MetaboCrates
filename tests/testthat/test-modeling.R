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

path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
dat <- add_group(dat, "group")
dat <- complete_data(dat, "limit", "limit", "limit")

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

test_that("build_model returns correct object with SLOPE model", {
  set.seed(12)
  model <- build_model(dat, "group", "2")
  
  expect_equal(nrow(model[["train"]]), 15)
  expect_equal(nrow(model[["test"]]), 10)
  expect_s3_class(model[["model"]], c("BinomialSLOPE", "SLOPE"))
})

test_that("build_model returns correct object with Lasso model", {
  set.seed(12)
  model <- build_model(dat, "group", "2", model = "Lasso")
  
  expect_equal(nrow(model[["train"]]), 15)
  expect_equal(nrow(model[["test"]]), 10)
  expect_s3_class(model[["model"]], c("lognet", "glmnet"))
})

test_that(
  "build_model returns correct object when the response has two levels",
  {
    mod_test_dat <- dat %>%
      dplyr::mutate(group = sample(c("1", "2"), nrow(dat), replace = TRUE))
    attributes(mod_test_dat) <- attributes(dat)
    mod_test_dat <- add_group(mod_test_dat, "group")
    mod_test_dat <- complete_data(mod_test_dat, "limit", "limit", "limit")
    
    set.seed(12)
    model <- build_model(mod_test_dat, "group")
    
    expect_equal(nrow(model[["train"]]), 15)
    expect_equal(nrow(model[["test"]]), 10)
    expect_s3_class(model[["model"]], c("BinomialSLOPE", "SLOPE"))
  }
)

test_that("split_model_dat returns the correct indexes of train observations", {
  modeling_dat <- MetaboCrates:::get_modeling_data(dat, "group", "2")
  split_dat <- MetaboCrates:::split_model_dat(modeling_dat, 0.6,
                                              get_train_obs_num = TRUE)
  
  expect_equal(
    sort(split_dat[["train_obs_num"]]),
    which(do.call(paste0, modeling_dat) %in% do.call(paste0, split_dat$train))
  )
})

test_that("predict_probability returns the correct test predictions for SLOPE", {
  set.seed(12)
  model <- build_model(dat, "group", "2")
  prediction <- predict_probability(model)
  
  probs <-  1 / (1 + exp(- (model[["test"]][,-(1:2)] %*%
                              model[["model"]][["coefficients"]][["p1"]] +
                              model[["model"]][["intercepts"]][[1]])))
  
  expect_equal(prediction[,1], as.matrix(probs)[,1])
})

test_that("predict_probability returns the correct test predictions for Lasso", {
  set.seed(12)
  model <- build_model(dat, "group", "2", "Lasso")
  prediction <- predict_probability(model)
  
  probs <-  1 / (1 + exp(- (model[["test"]][,-(1:2)] %*%
                              model[["model"]][["beta"]] +
                              model[["model"]][["a0"]])))
  
  expect_equal(prediction[,1], as.matrix(probs)[,1])
})

test_that("predict_probability cleans new data", {
  set.seed(12)
  model <- build_model(dat, "group", "2")
  new_dat <- data.frame(
    `sample identification` = 1:5,
    `sample type` = c(rep("Sample", 3), rep("QC", 2)),
    `C0` = rep(0, 5),
    `C2` = rep(0, 5),
    `C3` = rep(0, 5),
    `C3-DC (C4-OH)` = rep(0, 5),
    `C3-OH` = rep(0, 5),
    `C3:1` = c(2, 0.5, -1, 0.01, 3),
    `C4` = rep(0, 5),
    `C5` = rep(0, 5),
    check.names = FALSE
  )

  prediction <- predict_probability(model, new_dat)
  
  expect_equal(prediction[["sample identification"]], 1:3)
})

test_that("get_model_summary returnes correct object", {
  set.seed(12)
  model <- build_model(dat, "group", "2")
  summary <- get_model_summary(model)
  
  expect_equal(nrow(summary[["train"]]), 15)
  expect_equal(nrow(summary[["test"]]), 10)
  expect_equal(summary[["auc"]], 0.714285714)
})

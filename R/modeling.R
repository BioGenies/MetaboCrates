
#' Get data cleaned for modeling
#' 
#' @keywords internal

get_modeling_data <- function(dat, response, level = NULL) {
  if(is.null(attr(dat, "completed")))
    stop("Complete data first.")
  
  model_dat <- attr(dat, "completed") %>%
    select(!all_of(unlist(attr(dat, "removed")))) %>%
    filter(`sample type` == "Sample")
  
  if(length(unique(model_dat[[response]])) > 2 && is.null(level))
    stop("A specific level must be given when the response variable has three or more levels.")
  
  if(!is.null(level) && !(level %in% model_dat[[response]]))
    stop("Given level wasn't found in a response variable.")
  
  if(length(unique(model_dat[[response]])) == 1)
    stop("Response variable has only one level.")
  
  model_dat <- model_dat %>%
    select(any_of(c("sample identification", attr(dat, "metabolites"),
                    response))) %>%
    na.omit() %>%
    select(where(~ length(unique(.)) > 1))
  
  if(nrow(model_dat) < 2)
    stop("Too many observations with missing values.")
  
  aval_levels <- model_dat %>%
    group_by(across(any_of(response))) %>%
    count() %>%
    filter(n > 1) %>%
    select(any_of(response)) %>%
    unlist()
  
  if(length(aval_levels) < 2)
    stop("Provided response variable can't be used - too small number of unique values in levels.")
  
  if(!is.null(level) && !(level %in% aval_levels))
    stop("Provided level can't be used - too small number of unique values in this level.")
  
  clean_model_dat <- model_dat %>%
    filter(.data[[response]] %in% aval_levels)
  
  if(is.null(level))
    level <- aval_levels[1]
  
  clean_model_dat <- clean_model_dat %>%
    mutate(dummy = as.factor(.data[[response]]), .before = 2) %>%
    mutate(dummy = case_when(dummy == level ~ 1,
                             .default = 0)) %>%
    select(!all_of(response))
  
  clean_model_dat <- clean_model_dat %>%
    mutate(dummy = as.factor(dummy)) %>%
    rename(setNames(c("dummy"), level))
  
  clean_model_dat
}

#' Split data to train and test
#' 
#' @keywords internal

split_model_dat <- function(modeling_dat, train_prop,
                            get_train_obs_num = FALSE){
  response <- setdiff(colnames(modeling_dat),
                      c(attr(modeling_dat, "metabolites"),
                        "sample identification"))
  
  level_1_obs <- modeling_dat %>%
    mutate(n = 1:n()) %>%
    filter(if_all(response, ~ .x == 1)) %>%
    select(n) %>%
    unlist() %>%
    as.numeric()
  
  level_0_obs <- setdiff(1:nrow(modeling_dat), level_1_obs)
  
  train_obs <- c(
    sample(level_1_obs,
           round(length(level_1_obs) * train_prop)),
    sample(level_0_obs,
           round(length(level_0_obs) * train_prop))
  )
  
  split_dat <- list(
    train = modeling_dat[sort(train_obs),],
    test = modeling_dat[setdiff(1:nrow(modeling_dat), sort(train_obs)),]
  )
  
  if(get_train_obs_num)
    c(split_dat,
      list(train_obs_num = train_obs))
  else
    split_dat
}

#' Build folds models
#' 
#' @importFrom SLOPE trainSLOPE
#' @importFrom SLOPE SLOPE
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet glmnet
#' 
#' @keywords internal

get_cv_model <- function(train, model, nfolds){
  response <- colnames(train)[2]
  
  x <- train %>%
    select(!all_of(c(response, "sample identification")))
  
  y <- train[[response]]
  
  if(model == "SLOPE"){
    tune <- trainSLOPE(x, y, family = "binomial", number = nfolds)
    alpha_opt <- tune[["optima"]] %>%
      filter(measure == "mse") %>%
      select(alpha) %>%
      unlist() %>%
      as.numeric()
    SLOPE(x, y, q = 0.2, alpha = alpha_opt,
          lambda = tune[["model"]][["lambda"]])
  }
  else{
    foldid <- sample(rep(1:nfolds, length = nrow(train)))
    tune <- cv.glmnet(as.matrix(x), y, family = "binomial", foldid = foldid)
    glmnet(as.matrix(x), y, family = "binomial", lambda =  tune[["lambda.min"]])
  }
}

#' Build penalized logistic regression model
#'
#' @description
#' `build_models()` fits a penalized logistic regression using the specified
#' grouping column as the response variable. Two types of penalized models are
#' available, with parameters estimated through cross-validation: **SLOPE**
#' ([SLOPE::trainSLOPE()], [SLOPE::SLOPE()]) and **Lasso** ([glmnet::cv.glmnet()],
#' [glmnet::glmnet()]).
#' 
#' @param dat a \code{\link{raw_data}} object, the output of [read_data()],
#' with a group specified using the [add_group()] function. The data must be
#' completed, for example using [complete_data()] function.
#' @param response a string specifying the name of the response variable. Must
#' be one of the grouping variables.
#' @param level a string specifying the level to model. Required
#' when the response has more than two levels.
#' @param model a string specifying the penalization type to use.
#' Can be either `SLOPE` or `Lasso`.
#' @param nfolds a number of folds used in cross-validation.
#' @param train_prop a decimal specifying the proportion of observations in
#' train dataset.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- add_group(dat, "group")
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' build_model(dat, "group", "2")
#' 
#' @export

build_model <- function(dat, response, level = NULL, model = "SLOPE",
                         nfolds = 5, train_prop = 0.6) {
  clean_model_dat <- get_modeling_data(dat, response, level)
  
  split_dat <- split_model_dat(clean_model_dat, train_prop)
  
  model_fit_cv <- get_cv_model(split_dat[["train"]], model, nfolds)
  
  c(split_dat,
    list(model = model_fit_cv))
}

#' Get model summary
#'
#' @description
#' `get_model_summary()` returns a list containing: train dataset, test dataset
#' with predicted values, model coefficients, AUC value and ROC plot, for
#' a model.
#' 
#' @importFrom plotROC geom_roc
#' @importFrom plotROC calc_auc
#' 
#' @param model a model object, the output of [build_model()].
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- add_group(dat, "group")
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' model <- build_model(dat, "group", "2")
#' get_model_summary(model)
#' 
#' @export

get_model_summary <- function(model){
  model_opt <- model[["model"]]
  
  coefs <- if("glmnet" %in% class(model_opt))
    data.frame(
      term = c("(Intercept)", rownames(model_opt[["beta"]])),
      estimate = c(model_opt[["a0"]], model_opt[["beta"]][,1])
    )
  else
    data.frame(
      term = c("(Intercept)", model_opt[["variable_names"]]),
      estimate = c(model_opt[["intercepts"]][[1]],
                   model_opt[["coefficients"]][[1]][,1])
    )
  
  coefs <- filter(coefs, estimate != 0)
  
  response <- colnames(model[["test"]])[2]
  
  test_preds <- predict_probability(model) %>%
    mutate(across(all_of(response), ~ as.numeric(levels(.x)[.x])))
  
  roc_plot <- ggplot(test_preds, aes(d = get(response),
                                     m = get(paste0("probability_", response)))) +
    geom_roc(n.cuts = 5) +
    geom_abline(slope =  1, intercept = 0, color = "darkgrey",
                linetype = "dashed") +
    metabocrates_theme()
  
  auc <- calc_auc(roc_plot)[["AUC"]]
  
  roc_plot <- roc_plot +
    annotate("text", x = .75, y = .25, 
             label = paste("AUC =", round(auc, 3))) +
    labs(x = "false positive rate", y = "true positive rate")
  
  list(
    train = model[["train"]],
    test = test_preds,
    coefficients = coefs,
    auc = auc,
    roc_plot = roc_plot
  )
}

#' Predict probabilities
#' 
#' @description
#' `predict_probability()` returns the provided dataset with predicted
#' probabilities for the response variable.
#' 
#' @importFrom stats predict
#' 
#' @inheritParams get_model_summary
#' 
#' @param new_dat a cleaned metabolomics or compounds matrix. If no dataset is
#' provided, predictions for the test dataset are returned.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- add_group(dat, "group")
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' model <- build_model(dat, "group", "2")
#' predict_probability(model)
#' 
#' @export

predict_probability <- function(model, new_dat = NULL){
  model_opt <- model[["model"]]
  
  var_names <- if("glmnet" %in% class(model_opt))
    rownames(model_opt[["beta"]])
  else
    model_opt[["variable_names"]]
  
  if(is.null(new_dat))
    new_dat <- model[["test"]]
  
  if("sample type" %in% colnames(new_dat))
    new_dat <- filter(new_dat, `sample type` == "Sample")
    
  clean_new_dat <- new_dat %>%
    select(any_of(var_names)) %>%
    na.omit()
  
  if(nrow(clean_new_dat) == 0)
    stop("No observations without missing values found.")
  
  if(ncol(clean_new_dat) < length(var_names))
    stop("Some of the predictors are missing from the new dataset.")
  
  prob <- if("glmnet" %in% class(model_opt))
    predict(model_opt, as.matrix(clean_new_dat), type = "response")
  else
    1 / (1 + exp(-predict(model_opt, clean_new_dat)))
  
  response_lvl <- colnames(model[["train"]])[2]
  
  new_dat %>%
    select(-any_of("sample type")) %>%
    filter(if_all(var_names, ~ !is.na(.x))) %>%
    mutate("probability_{response_lvl}" := prob, .before = 1)
}

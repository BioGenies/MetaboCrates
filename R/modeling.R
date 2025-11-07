
#' Build linear models
#'
#' @description
#' `build_models()` fits full and penalized logistic models using the
#' specified grouping column as the outcome.
#' 
#' @inheritParams add_group
#' 
#' @param response a string specifying the name of the response variable.
#' @param level a string specifying the name of the level to model when
#' the response has more than two levels.
#' 
#' @importFrom SLOPE cvSLOPE
#' @importFrom SLOPE SLOPE
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet glmnet
#' @importFrom stats glm
#' @importFrom stats predict
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- add_group(dat, "group")
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' build_models(dat, "group", "2")
#' 
#' @export

build_models <- function(dat, response, level = NULL){
  if(is.null(attr(dat, "completed")))
    stop("Complete data first.")
  
  model_dat <- attr(dat, "completed") %>%
    filter(`sample type` == "Sample")
  
  if(length(unique(model_dat[[response]])) > 2 && is.null(level))
    stop("A specific level must be given when the response variable has three
          or more levels.")
  
  if(!is.null(level) && !(level %in% model_dat[[response]]))
    stop("Given level wasn't found in a response variable.")
  
  if(length(unique(model_dat[[response]])) == 1)
    stop("Response variable has only one level.")
  
  model_dat <- model_dat %>%
    select(all_of(c(attr(dat, "metabolites"),
                    response))) %>%
    na.omit() %>%
    select(where(~ length(unique(.)) > 1))
  
  if(nrow(model_dat) < 2)
    stop("Too many missing values.")
  
  aval_levels <- model_dat %>%
    group_by(across(any_of(response))) %>%
    count() %>%
    filter(n > 1) %>%
    select(any_of(response)) %>%
    unlist()
  
  if(length(aval_levels) < 2)
    stop("Provided response variable can't be used - too small number of values
         in levels.")
  
  if(!is.null(level) && !(level %in% aval_levels))
    stop("Provided level can't be used - too small number of values in this
         level.")
  
  clean_model_dat <- model_dat %>%
    filter(.data[[response]] %in% aval_levels)
  
  if(is.null(level))
    level <- aval_levels[1]
  
  clean_model_dat <- clean_model_dat %>%
    mutate(dummy = as.factor(.data[[response]]), .before = 1) %>%
    mutate(dummy = case_when(.data[[response]] == level ~ 1,
                             .default = 0)) %>%
    select(!all_of(response))
  
  response <- sub(" ", "_", paste0(response, "_", level))
  
  clean_model_dat <- rename_with(clean_model_dat, ~ paste0(response), dummy) %>%
    mutate(across(all_of(response), as.factor))
  
  set.seed(12)
  
  reduced_model <- tryCatch({
    tune <- cvSLOPE(x = select(clean_model_dat, !all_of(response)),
                    y = clean_model_dat[[response]],
                    family = "binomial",
                    n_folds = 5)
    SLOPE(x = select(clean_model_dat, !all_of(response)),
          y = clean_model_dat[[response]],
          family = "binomial",
          alpha = tune[["optima"]][["alpha"]])
    },
    error = function(e){
      warning("SLOPE regularization cannot be performed -
                  using ridge regression instead")
      
      foldid <- sample(rep(seq(10), length.out = nrow(clean_model_dat)))
                       
      lambda_best <- cv.glmnet(
        x = as.matrix(select(clean_model_dat, !all_of(response))),
        y = clean_model_dat[[response]],
        family = "binomial"
      )[["lambda.min"]]
      
      glmnet(x = as.matrix(select(clean_model_dat, !all_of(response))),
             y = clean_model_dat[[response]],
             family = "binomial", lambda = lambda_best,
             foldid = foldid)
    }
  )
  
  if(nrow(clean_model_dat) <= ncol(clean_model_dat)){
    warning("Design matrix isn't full rank - returning only reduced model")
    list(reduced = reduced_model, data = clean_model_dat)
  }else
    list(
      full = glm(paste0(response, "~ ."), clean_model_dat, family = "binomial"),
      reduced = reduced_model
    )
}

#' Get models summary
#'
#' @description
#' `get_models_info()` returns the summaries for models returned by
#' [build_models()].
#' 
#' @param models the object returned by [build_models()].
#' 
#' @importFrom broom augment
#' @importFrom broom glance
#' @importFrom broom tidy
#' @importFrom stats deviance
#' @importFrom stats predict
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- add_group(dat, "group")
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' models <- build_models(dat, "group", "2")
#' get_models_info(models)
#' 
#' @export

get_models_info <- function(models){
  reduced_model_coefs <- if("glmnet" %in% class(models[["reduced"]]))
    which(models[["reduced"]][["beta"]][, 1] != 0)
  else
    which(models[["reduced"]][["nonzeros"]][[1]][, 1])
  
  reduced_coef_num <- length(reduced_model_coefs)
  
  model_data <- if(is.null(models[["full"]]))
    models[["data"]]
  else
    models[["full"]][["model"]]
  
  reduced_data <- model_data %>%
    select(c(1, reduced_model_coefs + 1)) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.x)))) %>%
    mutate(
      fitted = as.numeric(predict(
        models[["reduced"]],
        as.matrix(model_data[, -1]),
        type = "response"
      )),
      resid = sign(.[[1]] - fitted) *
        sqrt(-2 * (.[[1]] * log(fitted) + (1 - .[[1]]) * log(1 - fitted)))
    )
  
  reduced_deviance <- deviance(models[["reduced"]])
  
  reduced_df <- if("glmnet" %in% class(models[["reduced"]]))
    models[["reduced"]][["df"]]
  else
    sum(models[["reduced"]][["nonzeros"]][[1]])
  
  nobs <- nrow(model_data)
  null_deviance <- if("glmnet" %in% class(models[["reduced"]]))
    models[["reduced"]][["nulldev"]]
  else
    models[["reduced"]][["null_deviance"]]
    
  info <- list(
    data = list(
      full = {
        if(is.null(models[["full"]]))
          model_data
        else
          augment(models[["full"]], type.predict = "response",
                  type.residuals = "deviance") %>%
          select(!.sigma)
      },
      reduced = reduced_data
    ),
    summary = list(
      general = data.frame(
        null.deviance = null_deviance,
        df.null = nobs - 1,
        nobs = nobs
      ),
      reduced = data.frame(
        logLik = reduced_deviance/(-2),
        AIC = 2 * reduced_df + reduced_deviance,
        BIC = reduced_df * nobs + reduced_deviance,
        deviance = reduced_deviance,
        df.residual = max(nobs - reduced_df, 0)
      )
    ),
    coefficients = {
      coefs <- if("glmnet" %in% class(models[["reduced"]]))
        select(tidy(models[["reduced"]]), term, estimate) %>%
        mutate(term = gsub("`", "", term))
      else
        data.frame(
          term = c("(Intercept)", models[["reduced"]][["variable_names"]]),
          estimate = c(models[["reduced"]][["intercepts"]][[1]],
                       models[["reduced"]][["coefficients"]][["p1"]][, 1])
        )
    }
  )
  
  if(!is.null(models[["full"]])){
    info[["summary"]] <- list(
      general = info[["summary"]][["general"]],
      full = glance(models[["full"]]) %>%
        select(logLik, AIC, BIC, deviance, df.residual),
      reduced = info[["summary"]][["reduced"]]
    )
    
    info[["coefficients"]] <- list(
      full = suppressWarnings(tidy(models[["full"]])) %>%
        mutate(term = gsub("`", "", term)),
      reduced = info[["coefficients"]]
    )
  }
  
  info
}

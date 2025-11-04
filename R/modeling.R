
#' Build models
#'
#' @description
#' `build_models()` returns the linear models with and without SLOPE coś tam
#' 
#' @inheritParams add_group
#' 
#' @param response a string specifying the response variable.
#' @param level a string specifying which level of the response variable to
#' consider. Required only if response variable has over two levels.
#' 
#' @importFrom SLOPE trainSLOPE
#' @importFrom glmnet cv.glmnet glmnet
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
  
  if(length(unique(dat[[response]])) > 2 && is.null(level))
    stop("A specific level must be given when the response variable has three
          or more levels.")
  
  if(!is.null(level) && !(level %in% dat[[response]]))
    stop("Given level wasn't found in a response variable.")
  
  if(length(unique(dat[[response]])) == 1)
    stop("Response variable has only one level.")
  
  model_dat <- attr(dat, "completed") %>%
    filter(`sample type` == "Sample") %>%
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
  
  if(!is.null(level)){
    clean_model_dat <- clean_model_dat %>%
      mutate(dummy = as.factor(.data[[response]])) %>%
      mutate(dummy = case_when(.data[[response]] == level ~ 1,
                               .default = 0)) %>%
      select(!all_of(response))
    response <- sub(" ", "_", paste0(response, "_", level))
    clean_model_dat <- rename_with(clean_model_dat, ~ paste0(response), dummy)
  }
  
  clean_model_dat <- clean_model_dat %>%
    mutate(across(all_of(response), as.factor))
  
  list(
    full = glm(paste0(response, "~ ."), clean_model_dat, family = "binomial"),
    reduced = tryCatch(
      trainSLOPE(x = select(clean_model_dat, !all_of(response)),
                 y = clean_model_dat[[response]],
                 family = "binomial"),
      error = function(e){
        warning("SLOPE regularization cannot be performed -
                using ridge regression instead")
        
        lambda_best <- cv.glmnet(
          x = as.matrix(select(clean_model_dat, !all_of(response))),
          y = clean_model_dat[[response]],
          family = "binomial"
        )[["lambda.min"]]
        glmnet(x = as.matrix(select(clean_model_dat, !all_of(response))),
               y = clean_model_dat[[response]],
               family = "binomial", lambda = lambda_best)
      }
    )
  )
}

#' Get information about models
#'
#' @description
#' `build_models()` returns the linear models with and without SLOPE coś tam
#' 
#' @inheritParams add_group
#' 
#' @param response a string specifying the response variable.
#' @param level a string specifying which level of the response variable to
#' consider. Required only if response variable has over two levels.
#' 
#' @importFrom broom augment glance tidy
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
  reduced_data <- models[["full"]][["model"]] %>%
    select(c(1, attr(models[["reduced"]][["beta"]], "i") + 1)) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.x)))) %>%
    mutate(fitted = as.numeric(predict(models[["reduced"]],
                            as.matrix(models[["full"]][["model"]][, -1]),
                            type = "response")),
           resid = sign(.[[1]] - fitted) *
             sqrt(-2 * (.[[1]] * log(fitted) + (1 - .[[1]]) *
                          log(1 - fitted))))
  
  reduced_deviance <- deviance(models[["reduced"]])
    
  list(
    data = list(
      full = augment(models[["full"]], type.predict = "response",
                     type.residuals = "deviance"),
      reduced = reduced_data
    ),
    summary = list(
      general = glance(models[["full"]]) %>%
        select(null.deviance, df.null, nobs),
      full = glance(models[["full"]]) %>%
        select(logLik, AIC, BIC, deviance, df.residual),
      reduced = data.frame(
        logLik = reduced_deviance/(-2),
        AIC = 2 * models[["reduced"]][["df"]] + reduced_deviance,
        BIC = models[["reduced"]][["df"]] * models[["reduced"]][["nobs"]] +
          reduced_deviance,
        deviance = reduced_deviance,
        df.residual = models[["reduced"]][["nobs"]] -
          models[["reduced"]][["df"]]
      )
    ),
    coefficients = list(
      full = tidy(models[["full"]]),
      reduced = tidy(models[["reduced"]])
    )
  )
}

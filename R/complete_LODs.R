
#' Complete not quantified values
#'
#' @description This function completes missing values related to limits of 
#' quantification or detection.
#' 
#' @importFrom tidyr spread
#'
#' @param dat descr
#' @param LOD_method description
#' @param LLOQ_method description
#' @param ULOQ_method description
#' @param LOD_type desc
#'
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat)
#'
#' @export
#' 

complete_data <- function(dat, LOD_method = NULL, LLOQ_method = NULL, 
                          ULOQ_method = NULL, LOD_type = "calc") {
  
  if(nrow(attr(dat, "NA_info")[["counts"]]) == 0)
    message("No missing values found.")
  
  NA_info <- attr(dat, "NA_info")[["counts"]] %>%
    filter(type %in% c("< LOD", "< LLOQ", "> ULOQ")) %>% 
    spread(type, n)
  
  sets <- dat %>% 
    filter(`sample type` == "Sample" | grepl("QC", `sample type`)) %>% 
    pull(`plate bar code`) %>% 
    unique()
  
  LOD_vals <- match_plate_codes(attr(dat, "LOD_table"), sets)
  
  gathered_data <- dat %>% 
    select(`plate bar code`, `sample identification`, `sample type`, 
           all_of(attr(dat, "metabolites"))) %>% 
    mutate(tmp_id = 1:n()) %>% 
    gather(key = "compound", value = "value", -`sample identification`, 
           -`sample type`, -`plate bar code`, -tmp_id)
  
  ### LOD imputation
  if(!is.null(NA_info[["< LOD"]])) {
    if(NA_info[["< LOD"]] > 0 & !is.null(LOD_method)) {
      message(paste0("Completing ", NA_info[["< LOD"]], " < LOD values..."))
      gathered_data <- complete_LOD(gathered_data = gathered_data,  
                                    method = LOD_method, 
                                    LOD_type = LOD_type, 
                                    LOD_vals = LOD_vals)
    } else {
      message("Skipping < LOD imputation.")
    }
  } else if(nrow(attr(dat, "NA_info")[["counts"]]) != 0){
    message("No < LOD values found.")
  }
  
  ### LLOQ imputation
  if(!is.null(NA_info[["< LLOQ"]])) {
    if(NA_info[["< LLOQ"]] > 0 & !is.null(LLOQ_method)) {
      message(paste0("Completing ", NA_info[["< LLOQ"]], " < LLOQ values..."))
      gathered_data <- complete_LLOQ(gathered_data = gathered_data, 
                                     method = LLOQ_method, 
                                     LOD_vals = LOD_vals)
    } else {
      message("Skipping < LLOQ imputation.")
    }
  } else if(nrow(attr(dat, "NA_info")[["counts"]]) != 0){
    message("No < LLOQ values found.")
  }
  
  ### ULOQ imputation
  if(!is.null(NA_info[["> ULOQ"]])) {
    if(NA_info[["> ULOQ"]] > 0 & !is.null(ULOQ_method)) {
      message(paste0("Completing ", NA_info[["> ULOQ"]], " < ULOQ values..."))
      gathered_data <- complete_ULOQ(gathered_data = gathered_data, 
                                     method = ULOQ_method, 
                                     LOD_vals = LOD_vals)
    } else {
      message("Skipping > ULOQ imputation.")
    }
  } else if(nrow(attr(dat, "NA_info")[["counts"]]) != 0){
    message("No > ULOQ values found.")
  }
  
  suppressWarnings({
    completed_dat <- gathered_data %>% 
      spread(key = compound, value = value) %>% 
      arrange(tmp_id) %>% 
      mutate_at(all_of(attr(dat, "metabolites")), as.numeric)
  })
  
  tmp_dat <- dat
  tmp_dat[, colnames(completed_dat)] <- completed_dat
  attr(dat, "completed") <- tmp_dat
  
  dat
}


#' Complete values below limit of detection
#' 
#' @importFrom stringr str_extract
#' @importFrom stats runif
#'
#' @param gathered_data description
#' @param LOD_type a character. Type of LOD values form table ('OP' or 'calc'). 
#' It can be NULL depending on the \code{method} parameter.
#' @param method a character. Imputation method, one of "halfmin",  "random", 
#' "halflimit", "limit", "logspline" or NULL meaning changing missing values into NAs.
#' @param LOD_vals description
#' 
#' @keywords internal
#' 


complete_LOD <- function(gathered_data, LOD_type, method, LOD_vals) {
  
  method <- match.arg(method,
                      c("halfmin", "random", "halflimit", "limit",
                        "limit-0.2min", "logspline", NULL))
  LOD_type <- match.arg(LOD_type, c("OP", "calc"))
  
  if(!any(grepl(LOD_type, LOD_vals[["type"]])))
    stop(paste0("There is no ", LOD_type, " values in LOD table."))
  
  merged_dat <- gathered_data %>% 
    left_join(filter(LOD_vals, grepl(LOD_type, type)), 
              by = c("plate bar code", "compound"))
  
  merged_dat <- switch (
    method,
    halfmin = {
      merged_dat %>% 
        group_by(compound) %>% 
        mutate(value = ifelse(value == "< LOD" & !is.na(value), 
                              0.5 * general_min(value), value)) 
    },
    halflimit = {
      merged_dat %>% 
        mutate(value = ifelse(value == "< LOD" & !is.na(value), 
                              0.5 * thresh_est, value))
    },
    random = {
      merged_dat %>%  
        mutate(value = ifelse(value == "< LOD" & !is.na(value), 
                              runif(
                                sum(value == "< LOD" & !is.na(value)), 0, 
                                thresh_est[value == "< LOD" & !is.na(value)]), 
                              value))
    },
    limit = {
      merged_dat %>% 
        mutate(value = ifelse(value == "< LOD", thresh_est, value))
    },
    `limit-0.2min` = {
      merged_dat %>%
        mutate(value = ifelse(value == "< LOD",
                              thresh_est - 0.2 * general_min(value),
                              value))
    },
    logspline = {
      models <- merged_dat %>%
        group_by(compound) %>%
        mutate(value = as.numeric(value)) %>%
        filter(!is.na(value)) %>%
        summarise(
          model = list({
            if((length(value) < 10) || length(unique(value)) < 3) NA
            else logspline(value)
            })
        )
      
      cos <- merged_dat %>%
        left_join(models) %>%
        rowwise() %>%
        mutate(value = list({
          if(value == "< LOD" & !is.na(value)){
            if(all(is.na(model))) NA
            else qlogspline(0.05, model)
          }else value
        }))
    }
  )
  
  merged_dat %>% 
    select(- type, -thresh_est)
}

#' Find matching plate bar codes in the LOD table
#' 
#' @param LOD_table an LOD_table attribute from \code{\link{raw_data}} object.
#' @param sets an unique character vector of plate bar codes (for example 
#' '1036372116-1 | 1036372121-1') 
#'
#' @returns A long-format table containing limits of detection/quantification 
#' and corresponding plate bar codes
#'
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' sets <- unique(dat[["plate bar code"]])
#' MetaboCrates:::match_plate_codes(attr(dat, "LOD_table"), sets)
#'
#' @keywords internal
#' 

match_plate_codes <- function(LOD_table, sets) {
  LOD_table %>% 
    gather("compound", "thresh_est", -`plate bar code`, -type) %>% 
    mutate(`plate bar code` = str_extract(`plate bar code`, "(\\d)+")) %>% 
    mutate(`plate bar code` = sapply(`plate bar code`, function(ith_code)
      sets[str_detect(sets, ith_code)][1])) %>% 
    group_by(compound, `plate bar code`, type) %>% 
    summarise(thresh_est = sum(thresh_est, na.rm = TRUE))
}



#' Complete values above limit of quantification
#' 
#' @description This function imputes values below limit of quantification 
#' ("> ULOQ")
#' 
#' @importFrom stringr str_extract
#' @inheritParams complete_LOD
#'
#' @param method a character. Imputation method, one of "limit" or NULL meaning 
#' changing missing values into NAs.
#' @param LOD_vals description
#' 
#' @keywords internal
#' 

complete_ULOQ <- function(gathered_data, method, LOD_vals) {
  
  method <- match.arg(method, c("limit", "third quartile"))
  
  merged_dat <- gathered_data %>% 
    merge(filter(LOD_vals, type == "ULOQ"), 
          by = c("plate bar code", "compound"), all = TRUE)
  
  merged_dat <- switch (
    method,
    limit = {
      merged_dat %>% 
        mutate(value = ifelse(value == "> ULOQ", thresh_est, value))
    },
    `third quartile` = {
      merged_dat %>%
        mutate(value = ifelse(value == "> ULOQ",
                              general_third_quartile(value),
                              value))
    }
  )
  merged_dat %>% 
    select(- type, -thresh_est)
}


#' Complete values below limit of quantification
#' 
#' @description This function imputes values below limit of quantification 
#' ("< LLOQ")
#' 
#' @importFrom stringr str_extract
#' @inheritParams complete_LOD
#'
#' @param method a character. Imputation method, one of "limit" or NULL meaning 
#' changing missing values into NAs.
#' @param LOD_vals description
#' 
#' @keywords internal
#' 

complete_LLOQ <- function(gathered_data, method, LOD_vals) {
  
  method <- match.arg(method, c("limit"))
  
  merged_dat <- gathered_data %>% 
    merge(filter(LOD_vals, type == "LLOQ"), 
          by = c("plate bar code", "compound"), all = TRUE)
  
  merged_dat <- switch (
    method,
    limit = {
      merged_dat %>% 
        mutate(value = ifelse(value == "< LLOQ", thresh_est, value))
    }
  )
  merged_dat %>% 
    select(- type, -thresh_est)
}


#' Calculate minimum ignoring character values
#' 
#' @description This function calculates minimum without values such as NA's, 
#' values below or above LOD limit and so on. 
#' 
#' @param x a vector of observations
#' 
#' @examples
#' x <- c("<LOD", 5, 6, NA, 9, 16)
#' general_min(x)
#' 
#' @keywords internal
#' 

general_min <- function(x) {
  suppressWarnings({
    min_val <- min(as.numeric(x[!is.na(as.numeric(x))]))
    ifelse(is.infinite(min_val), NA, min_val)
  })
}

#' Calculate third quartile ignoring character values
#' 
#' @description This function calculates third quartile without values such as
#' NA's, values below or above LOD limit and so on. 
#' 
#' @param x a vector of observations
#' 
#' @examples
#' x <- c("<LOD", 5, 6, NA, 9, 16)
#' general_third_quartile(x)
#' 
#' @keywords internal
#' 

general_third_quartile <- function(x){
  suppressWarnings({
    quantile(as.numeric(x[!is.na(as.numeric(x))]), prob = 0.75, type = 1)
  })
}




#' Creation of validated raw_data class
#' 
#' @description The wrapper function for the constructor of the 
#' \code{\link{raw_data}} and its validator. Used in \code{\link{raw_data}} 
#' function.
#' 
#' @return \code{\link{raw_data}} object.
#' 
#' @keywords internal

new_raw_data <- function(metabolomics_matrix, 
                         LOD_table, 
                         NA_info,
                         metabolites,
                         samples,
                         group,
                         removed,
                         completed,
                         cv){
  
  structure(.Data = metabolomics_matrix,
            "LOD_table" = LOD_table,
            "NA_info" = NA_info,
            "metabolites" = metabolites,
            "samples" = samples,
            "group" = group,
            "removed" = removed,
            "completed" = completed,
            "cv" = cv,
            class = c("raw_data", "data.frame"))
}


#' Validator on raw_data class
#' 
#' @description Validator on the content of an raw_data object.
#' 
#' @param raw_data \code{\link{raw_data}} object.
#' 
#' @keywords internal

validate_raw_data <- function(raw_data) {
  
  # Validate main data matrix
  sample_ids <- raw_data %>% 
    filter(`sample type` == "Sample") %>% 
    pull(`sample identification`)
  
  if(any(duplicated(sample_ids)))
    stop("Sample identification column has to be unique.")
  
  metabolites <- attr(raw_data, "metabolites")
  
  # Validate metabolites names
  
  if(!all(metabolites %in% colnames(raw_data))) {
    warning(paste0("Metabolites ", 
                   paste0(metabolites[!(metabolites %in% colnames(raw_data))], 
                          collapse = ", "), 
                   " cannot be found in the data! We will ignore them."))
    attr(raw_data, "metabolites") <- 
      metabolites[metabolites %in% colnames(raw_data)]
    metabolites <- attr(raw_data, "metabolites")
  }
  
  metabolites_values <- raw_data %>%
    select(all_of(metabolites)) %>% 
    unlist()
  
  values_mode <- metabolites_values
  
  suppressWarnings({ storage.mode(values_mode) <- "numeric" })
  
  wrong_values <- is.na(values_mode) & 
    !(metabolites_values %in% c("< LOD","< LLOQ", "> ULOQ", "NA", "∞", NA))
  
  if(any(wrong_values)) 
    stop("Found incorrect metabolites values!")
  
  required_columns <- c("plate bar code", "sample identification", 
                        "sample type", "measurement time")
  if(!(all(required_columns %in% colnames(raw_data))))
    stop(paste("Data should contain row/s:", 
               which(!(required_columns %in% colnames(raw_data)))))
  
  if(!any(grepl("QC", raw_data[["sample type"]])))
    stop("Data should contain quality control samples.")
  
  if(any(is.na(select(filter(raw_data, grepl("QC", `sample type`)), 
                      all_of(metabolites)))))
    stop("Quality contriol samples should not contain missing values!")
  
  # Validate LOD table
  LOD_table <- attr(raw_data, "LOD_table")
  
  if(!all(sort(setdiff(colnames(LOD_table), 
                       c("plate bar code", "type"))) == sort(metabolites)))
    stop("Provided metabolites do not match LOD table!")
  
  # Validate groups
  group_names <- attr(raw_data, "group")
  
  if(!is.null(group_names)) {
    if(!all(group_names %in% colnames(raw_data)))
      stop(
        paste0("At least one of the provided grouping columns: ", group_name,
               " is not present in the data.")
      )
    
    samples_data <- raw_data %>% 
      filter(`sample type` == "Sample")
    
    if(any(is.na(samples_data[, group_names])))
      stop("Grouping columns should not contain any NA's!")
    
    counts <- samples_data %>% 
      group_by(across(all_of(group_names))) %>% 
      summarise(count = n()) %>% 
      pull(count)
    if(any(counts < 2))
      stop("Some group levels have less than 2 observations.")
  }
  
  raw_data
  
}


#' raw_data class
#' 
#' @importFrom tidyr gather
#' 
#' @param metabolomics_matrix a \code{\link{data.frame}}, matrix containing 
#' biocrates data 
#' 
#' @param LOD_table a \code{\link{list}} containing two elements:
#' - \code{table}: LOD table of limits of detection / quantification,
#' - \code{types}: character vector of types of values, for example calc. 
#' meaning calculated from samples and op, received from operator.
#' 
#' @param metabolites a \code{\link{character}}, vector of metabolites names 
#' contained in the data.
#' 
#' @param group a \code{NULL} indicating no grouping or
#' a vector of \code{\link{character}} names of group columns. Default to NULL.
#' The code will throw an error in the case when:
#' - column of such a name won't be contained in the dataset,
#' - there will be NA's in the grouping column,
#' - any group will have less than 2 observations.
#' 
#' @return \code{\link{raw_data}} object metabolomics matrix with the following 
#' attributes:
#' 
#' - \code{LOD_table}
#' - \code{NA_info}: a \code{\link{list}} related to missing values in the data. 
#' It contains \code{NA_ratios}: fractions of missing values per every 
#' metabolite. When \code{group} parameter (see below) is provided and 
#' \code{counts}: table of types of missing values with their counts (related 
#' to "sample" type samples).
#' - \code{metabolites} 
#' - \code{samples}: a \code{\link{data.frame}} containing names of samples 
#' types and their counts
#' - \code{group} : a character name of group from the table
#' - \code{removed}: a list of removed metabolites
#' - \code{plate bar code}: each measurement has two plate bar codes depending on the method used
#' 
#' @keywords internal

raw_data <- function(metabolomics_matrix, 
                     LOD_table, 
                     metabolites,
                     group = NULL) {
  
  metabolomics_matrix <- metabolomics_matrix %>% 
    as.data.frame(check.names = FALSE)
  
  samples <- metabolomics_matrix %>% 
    group_by(`sample type`) %>% 
    summarise(count = n())
  
  n_samples <- samples %>% 
    filter(`sample type` == "Sample") %>% 
    pull(count)
  
  grouping_variables_tmp <- group
  miss_vals <- c("< LOD","< LLOQ", "> ULOQ", "NA", "∞")
  
  NA_ratios_type <- metabolomics_matrix %>% 
    filter(`sample type` == "Sample") %>% 
    select(any_of(metabolites)) %>% 
    mutate(n_samples = n()) %>% 
    tidyr::gather("metabolite", "value", -"n_samples") %>% 
    filter(value %in% miss_vals) %>% 
    rename(type = "value") %>% 
    group_by(metabolite, type) %>% 
    reframe(NA_frac = n()/n_samples) %>% 
    unique() %>%
    right_join(expand.grid(type = miss_vals,
                           metabolite = metabolites),
               by = c("metabolite", "type")) %>% 
    mutate(NA_frac = ifelse(is.na(NA_frac), 0, NA_frac)) %>% 
    arrange(metabolite) %>% 
    ungroup()
  
  
  if(!is.null(grouping_variables_tmp)) {
    
    grouping_column <- metabolomics_matrix %>%
      filter(`sample type` == "Sample") %>%
      mutate(group_tmp = do.call(paste,
                                 c(across(all_of(grouping_variables_tmp)),
                                   sep = ", "))) %>%
      select(group_tmp) %>%
      unlist()
    
    group_levels <- na.omit(unique(grouping_column))
    
    NA_ratios_group <- metabolomics_matrix %>% 
      filter(`sample type` == "Sample") %>% 
      mutate(grouping_column = grouping_column) %>% 
      select(any_of(metabolites), grouping_column) %>% 
      group_by(grouping_column) %>% 
      mutate(group_size = n()) %>%
      ungroup() %>% 
      tidyr::gather("metabolite", "value", -"grouping_column", -"group_size") %>% 
      filter(value %in% miss_vals) %>% 
      rename(type = "value") %>% 
      group_by(metabolite, grouping_column) %>% 
      reframe(NA_frac = n()/group_size) %>% 
      unique() %>% 
      right_join(expand.grid(metabolite = metabolites,
                             grouping_column = group_levels),
                 by = c("metabolite", 'grouping_column')) %>% 
      mutate(NA_frac = ifelse(is.na(NA_frac), 0, NA_frac)) %>% 
      arrange(metabolite, grouping_column) 
  } else {
    NA_ratios_group <- NULL
  }
  
  counts <- NA_ratios_type %>% 
    group_by(type) %>% 
    reframe(n = sum(NA_frac) * n_samples) %>% 
    filter(n > 0) %>% 
    arrange(-n)
  
  NA_info = list(counts = counts,
                 NA_ratios_type = NA_ratios_type,
                 NA_ratios_group = NA_ratios_group)
  
  
  exclude_cols <- match.arg(c("plate bar code", "type"), 
                            colnames(LOD_table),
                            several.ok = TRUE)
  
  LOD_table <- LOD_table %>% 
    as.data.frame() %>% 
    mutate_at(vars(-all_of(exclude_cols)), as.numeric) %>% 
    mutate(type = ifelse(grepl("LOD", `plate bar code`),
                         ifelse(grepl("(calc.)", `plate bar code`),
                                "LOD (calc.)", "LOD (from OP)"),
                         ifelse(grepl("ULOQ", `plate bar code`),
                                "ULOQ", "LLOQ")))
  
  validate_raw_data(
    new_raw_data(
      metabolomics_matrix = metabolomics_matrix,
      LOD_table = LOD_table,
      NA_info = NA_info,
      metabolites = metabolites,
      samples = samples,
      group = group,
      removed = list(LOD = NULL, QC = NULL, QC_man = NULL),
      completed = NULL,
      cv = NULL
    )
  )
}

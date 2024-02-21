

#' Creation of validated raw_data class
#' 
#' @description The wrapper function for the constructor of the \code{\link{raw_data}} and 
#' its validator. Used in \code{\link{raw_data}} function.
#' 
#' @return \code{\link{raw_data}} object.
#' 
#' @keywords internal

new_raw_data <- function(metabolomics_matrix, 
                         LOD_table, 
                         NA_info,
                         metabolites,
                         samples,
                         group){
  
  structure(.Data = metabolomics_matrix,
            "LOD_table" = LOD_table,
            "NA_info" = NA_info,
            "metabolites" = metabolites,
            "samples" = samples,
            "group" = group,
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
  
  metabolites_values <- raw_data %>%
    select(all_of(metabolites)) %>% 
    unlist()
  
  metabolites_values <- metabolites_values[
    which(!(metabolites_values %in% c("< LOD","< LLOQ", "> ULOQ", "NA", "∞", NA)))
  ]
  storage.mode(metabolites_values) <- "numeric"
  
  if(any(is.na(metabolites_values)))
    stop("Found incorrect metabolites values.")
  
  required_columns <- c("plate bar code", "sample identification", 
                        "sample type", "measurement time")
  if(!(all(required_columns %in% colnames(raw_data))))
    stop(paste("Data should contain row/s:", 
               which(!(required_columns %in% colnames(raw_data)))))
  
  if(!any(grepl("QC", raw_data[["sample type"]])))
    stop("Data should contain quality control samples.")
  
  if(any(is.na(select(filter(raw_data, grepl("QC", `sample type`)), all_of(metabolites)))))
    stop("Quality contriol samples should not contain missing values!")
  
  # Validate metabolites names
  
  if(!all(metabolites %in% colnames(raw_data))) {
    warning(paste0("Metabolites ", 
                   paste0(metabolites[!(metabolites %in% colnames(raw_data))], 
                          collapse = ", "), 
                   " cannot be found in the data! We will ignore them."))
    attr(raw_data, "metabolites") <- metabolites[metabolites %in% colnames(raw_data)]
  }
  
  # Validate LOD table
  LOD_table <- attr(raw_data, "LOD_table")
  
  if(!all(sort(colnames(LOD_table)[-1]) == sort(metabolites)))
    stop("Provided metabolites do not match LOD table!")
  
  # Validate groups
  group <- attr(raw_data, "group")
  
  if(!is.null(group)) {
    if(!(group %in% colnames(raw_data)))
      stop(paste0("Provided group:", group, " is not contained in the data."))
    
    if(!(group %in% colnames(raw_data)))
      stop(paste0("Provided group:", group, " is not contained in the data."))
    
    counts <- raw_data %>% 
      filter(`sample type` == "Sample") %>% 
      group_by(group) %>% 
      summarise(count = n()) %>% 
      pull(count)
    if(any(counts < 2))
      stop("Some of groups have less than 2 observations.")
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
#' meaning caluclated from samples and op, received from operator.
#' 
#' @param metabolites a \code{\link{character}}, vector of metabolites names 
#' contained in the data.
#' 
#' @param group a \code{NULL} indicating no grouping column in the data or
#' a \code{\link{character}} name of column with groups. Default to NA. The code 
#' will throw an error in the case when:
#' - column of such a name won't be contained in the dataset,
#' - there will be NA's in the grouping column,
#' - one of the groups will have less than 2 observations.
#' 
#' @return \code{\link{raw_data}} object metabolomics matrix with the following 
#' attributes:
#' 
#' - \code{LOD_table}
#' - \code{NA_info}: a \code{\link{list}} related to missing values in the data. 
#' It contains \code{NA_ratios}: fractions of missing values per every metabolite. 
#' When \code{group} parameter (see below) is provided and \code{counts}: table of 
#' types of missing values with their counts.
#' - \code{metabolites} 
#' - \code{samples}: a \code{\link{data.frame}} containing names of samples types 
#' and their counts
#' - \code{group}
#' 
#' @keywords internal

raw_data <- function(metabolomics_matrix, 
                     LOD_table, 
                     metabolites,
                     group = NULL) {
  
  metabolomics_matrix <- metabolomics_matrix %>% 
    as.data.frame(check.names = FALSE)
  
  NA_ratios <- metabolomics_matrix %>% 
    filter(`sample type` == "Sample") %>% 
    select(all_of(metabolites), group) %>% 
    tidyr::gather("metabolite", "value", -group) %>% 
    group_by(metabolite, group) %>% 
    summarise(NA_frac = mean(value %in% c("< LOD","< LLOQ", "> ULOQ", "NA", "∞")))
  
  miss_vals <- c("< LOD","< LLOQ", "> ULOQ", "NA", "∞")
  
  tmp <- metabolomics_matrix %>% 
    filter(`sample type` == "Sample")
  
  counts <- lapply(miss_vals, function(i) {
    data.frame(type = i, 
               n = sum(tmp == i, na.rm = TRUE))
  }) %>%  bind_rows()
  
  NA_info = list(NA_ratios = NA_ratios, counts = counts)
  
  samples <- metabolomics_matrix %>% 
    group_by(`sample type`) %>% 
    summarise(count = n())
  
  LOD_table <- LOD_table %>% 
    as.data.frame() %>% 
    mutate_at(vars(-("measurement time")), as.numeric)
  
  validate_raw_data(
    new_raw_data(metabolomics_matrix = metabolomics_matrix,
                 LOD_table = LOD_table,
                 NA_info = NA_info,
                 metabolites = metabolites,
                 samples = samples,
                 group = group)
  )
}

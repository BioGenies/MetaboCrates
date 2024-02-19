

#' Creation of validated raw_data class
#' 
#' @description The wrapper function for the constructor of the \code{\link{raw_data}} and 
#' its validator. Used in \code{\link{raw_data}} function.
#' 
#' @return \code{\link{raw_data}} object.
#' 
#' @keywords internal

new_raw_data <- function(metabolomics_matrix, LOD_table, metabolites){
  
  structure(.Data = metabolomics_matrix,
            "metabolites" = metabolites,
            "LOD_table" = LOD_table,
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
  
  metabolites_values <- raw_data %>%
    select(`measurement time`:last_col(), -`measurement time`) %>% 
    unlist()
  
  metabolites_values <- metabolites_values[
    which(!(metabolites_values %in% c("< LOD","< LLOQ", "> ULOQ", "NA", "∞", NA)))
  ]
  
  withCallingHandlers(
    expr = as.numeric(metabolites_values),
    warning = function(w) stop("Found incorrect metabolites values.")
  )
  
  required_columns <- c("plate bar code", "sample identification", 
                        "sample type", "measurement time")
  if(!(all(required_columns %in% colnames(raw_data))))
    stop(paste("Data should contain row/s:", 
               which(!(required_columns %in% colnames(raw_data)))))
  
  if(!any(grepl("QC", raw_data[["sample type"]])))
    stop("Data should contain quality control samples.")
  
  # Validate metabolites names
  metabolites <- attr(raw_data, "metabolites")
  
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
  
  raw_data
  
}


#' raw_data class
#' 
#' @param metabolomics_matrix \code{\link{data.frame}}, matrix containing 
#' biocrates data 
#' @param LOD_table \code{\link{data.frame}}, the LOD table containing limits of
#' detection / quantification
#' @param metabolites \code{\link{character}}, vector of metabolites names 
#' contained in the data.
#' 
#' @return \code{\link{raw_data}} object.
#' 
#' @keywords internal

raw_data <- function(metabolomics_matrix, LOD_table, metabolites) {
  
  metabolomics_matrix <- metabolomics_matrix %>% 
    as.data.frame(check.names = FALSE)
  
  LOD_table <- LOD_table %>% 
    as.data.frame() %>% 
    mutate_at(vars(-("measurement time")), as.numeric)
  
  validate_raw_data(
    new_raw_data(metabolomics_matrix = metabolomics_matrix,
                 LOD_table = LOD_table,
                 metabolites = metabolites)
  )
}

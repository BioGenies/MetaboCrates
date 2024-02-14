#' @import dplyr
#' @importFrom stringr str_to_sentence

## CONSTRUCTOR

new_raw_data <- function(matrix,
                         LOD_table,
                         metabolites){
  
  structure(.Data = matrix,
            "metabolites" = metabolites,
            "LOD_table" = LOD_table,
            class = c("raw_data", "data.frame"))
}

### VALIDATOR

validate_raw_data <- function(raw_data) {
  
  sample_ids <- raw_data %>% 
    filter(`sample type` == "Sample") %>% 
    pull(`sample identification`)
  
  if(any(duplicated(sample_ids)))
    stop("Sample identification column has to be unique.")
  
  metabolites_values <- raw_data %>%
    select(`measurement time`:last_col(), -`measurement time`) %>% 
    unlist()
  
  metabolites_values <- metabolites_values[which(!(metabolites_values %in% c("< LOD","< LLOQ", "> ULOQ", "NA", NA)))]
  
  withCallingHandlers(
    expr = as.numeric(metabolites_values),
    warning = function(w) stop("Found incorrect metabolites values.")
  )
  
  required_columns <- c("plate bar code", "sample identification", "sample type", "measurement time")
  if(!(all(required_columns %in% colnames(raw_data))))
    stop(paste("Data should contain row/s:",which(!(required_columns %in% colnames(raw_data)))))
  
  if(!any(grepl("QC", raw_data[["sample type"]])))
    stop("Data should contain quality control samples.")
  
  raw_data
  
}

### HELPER

raw_data <- function(raw_data,
                     LOD_table,
                     metabolites) {
  
  metabolomics_matrix <- raw_data %>% 
    as.data.frame(check.names = FALSE) %>% 
    rename_with(tolower)
  
  LOD_table <- LOD_table %>% as.data.frame()
  
  validate_raw_data(
    new_raw_data(matrix = metabolomics_matrix,
                 LOD_table = LOD_table,
                 metabolites = metabolites)
  )
}

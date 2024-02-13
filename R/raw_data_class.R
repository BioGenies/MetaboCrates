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

  matrix <- raw_data
  
  if(any(duplicated(matrix$`Sample identification`))){
    stop("Sample identification column has to be unique.")
  }
  
  metabolites_values <- matrix %>%
    select((which(colnames(matrix) == "Measurement time") + 1):last_col())
  metabolites_values <- unlist(metabolites_values)
  metabolites_values <- metabolites_values[which(!(metabolites_values %in% c("< LOD","< LLOQ", "> ULOQ", "NA", NA)))]
  
  withCallingHandlers(
    expr = as.numeric(metabolites_values),
    warning = function(w) {
      stop("Found incorrect metabolites values.")
    }
  )
  
  required_columns <- c("Plate bar code", "Sample identification", "Sample type", "Measurement time")
  if(!(all(required_columns %in% colnames(matrix))))
    stop(paste("Data should contain row/s:",which(!(required_columns %in% colnames(matrix)))))
  
  if(!any(grepl("QC", matrix$`Sample type`)))
    stop("Data should contain samples of QC type.")

  raw_data

}

### HELPER

raw_data <- function(raw_data,
                     LOD_table,
                     metabolites) {

  matrix <- as.data.frame(raw_data, check.names = FALSE)
  colnames(matrix) <- str_to_sentence(colnames(matrix))
  LOD_table <- as.data.frame(LOD_table)

  validate_raw_data(
    new_raw_data(matrix = matrix,
                 LOD_table = LOD_table,
                 metabolites = metabolites)
  )
}

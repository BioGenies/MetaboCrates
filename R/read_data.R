#' Read Biocrates data
#'
#' @description Imports Biocrates data from the Excel file.
#'
#' @import dplyr
#' @importFrom readxl read_excel
#' @importFrom stringr str_extract
#' @importFrom tools file_ext
#'
#' @param path Path to the file.
#' 
#' @examples
#' # example code
#' 
#' @export
#'

read_data <- function(path) {
  
  if(!file.exists(path)) {
    stop(paste0("The file ", path, " does not exist.
                You probably provided wrong path."))
  }
  
  if(!(file_ext(path) %in% c("xls", "xlsx"))) {
    stop(paste0("File extension should be xls or xlsx, not ", file_ext(path), "."))
  }
  
  data <- as.data.frame(read_excel(path, skip = 1))
  file.col.names <- colnames(data)
  colnames(data) <- tolower(colnames(data))

  metabolites <- file.col.names[(which(colnames(data) == "measurement time") + 1):ncol(data)]

  LOD_table <- data %>%
    select(`measurement time`:last_col()) %>%
    filter(str_extract(`measurement time`, "LOD") == "LOD")
  
  raw_data <- filter(data, !is.na(`plate bar code`))
  colnames(raw_data) <- file.col.names
  raw_data
}

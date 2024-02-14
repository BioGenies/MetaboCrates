#' Read Biocrates data
#'
#' @description Imports Biocrates data from the Excel file.
#'
#' @import dplyr
#' @importFrom readxl read_excel
#' @importFrom tools file_ext
#' @importFrom stringi stri_detect_fixed
#' @param path Path to the file.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' read_data(path)
#' 
#' @export
#'

read_data <- function(path) {
  
  if(!file.exists(path))
    stop(paste0("The file ", path, " does not exist. 
                You probably provided wrong path."))
  
  if(!(file_ext(path) %in% c("xls", "xlsx"))) 
    stop(paste0("File extension should be xls or xlsx, not ", file_ext(path), "."))
  
  dat <- read_excel(path, skip = 1) %>% 
    as.data.frame() %>% 
    rename_with(tolower)
  
  metabolites <- colnames(dat)[(which(colnames(dat) == "measurement time") + 1):ncol(dat)]
  
  LOD_table <- dat %>%
    select(`measurement time`:last_col()) %>%
    filter(stri_detect_fixed(`measurement time`, "LOD") | 
           stri_detect_fixed(`measurement time`, "LLOQ") | 
           stri_detect_fixed(`measurement time`, "ULOQ")) %>% 
    mutate_at(vars(-("measurement time")), as.numeric)
  
  dat %>% 
    filter(!is.na(`plate bar code`)) %>% 
    raw_data(LOD_table, metabolites)
}

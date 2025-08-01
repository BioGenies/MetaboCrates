#' Read Biocrates data
#'
#' @description Imports Biocrates data from the Excel file.
#'
#' @import dplyr
#' @importFrom readxl read_excel
#' @importFrom tools file_ext
#' @importFrom stringi stri_detect_fixed
#' 
#' @param path Path to the file containing Biocrates data imported from MetIDQ 
#' or WebIDQ TODO: description.
#' 
#' @return \code{\link{raw_data}} object.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' read_data(path)
#' 
#' @export
#'

read_data <- function(path) {
  
  if(!file.exists(path))
    stop(
      paste0("The file ", path," does not exist. Check if the path is correct.")
    )
  
  if(!(file_ext(path) %in% c("xls", "xlsx"))) 
    stop(
      paste0("File extension should be xls or xlsx, not ", file_ext(path), ".")
    )
  
  dat <- read_excel(path, skip = 1) %>% 
    as.data.frame() 
  
  last_col <- which(colnames(dat) %in% c("Measurement Time", 
                                         "Measurement time", 
                                         "measurement time"))
  
  metabolites <- colnames(dat)[(last_col + 1):ncol(dat)]
  
  if(length(unique(tolower(colnames(dat)))) != ncol(dat)){
    stop("Found duplicated column names.")
  }
  
  dat <- dat %>% rename_with(tolower, !all_of(metabolites))
  
  LOD_table <- dat %>%
    select(`measurement time`:last_col()) %>%
    rename(`plate bar code` = "measurement time") %>% 
    filter(stri_detect_fixed(`plate bar code`, "LOD") | 
             stri_detect_fixed(`plate bar code`, "LLOQ") | 
             stri_detect_fixed(`plate bar code`, "ULOQ")) %>% 
    mutate_at(vars(-("plate bar code")), as.numeric)
  
  metabolites_extensions <- metabolites[
    apply(LOD_table, 2, function(ith_metabolite) {
      all(is.na(ith_metabolite))
    })
  ]
  
  LOD_table <- LOD_table %>% select(-na.omit(metabolites_extensions))
  
  metabolites <- metabolites[!(metabolites %in% metabolites_extensions)]
  
  dat %>% 
    select(-na.omit(metabolites_extensions)) %>% 
    mutate(across(all_of(metabolites), check_values)) %>% 
    filter(!is.na(`plate bar code`)) %>% 
    raw_data(LOD_table, metabolites)
}


#' Convert weird values into `NA`'s
#'
#' @param metabolite_vals a \code{\link{character}} vector containing measured 
#' metabolite values.
#' @param special_signs a character vector of permitted special signs that 
#' should not be converted into `NA`'s. Default to `"auto"`, which converts
#' `< LOD`,`< LLOQ`, `> ULOQ`, `NA` and \eqn{\infty}, where ULOQ means upper
#' limit of quantification, LLOQ means lower limit of quantification and LOD
#' means limit of detection.
#' 
#' @export
#'

check_values <- function(metabolite_vals, special_signs = "auto"){
  if(special_signs == "auto")
    special_signs <- c("< LOD", "< LLOQ", "> ULOQ", "NA", "\u221E")
  
  values_storage <- metabolite_vals
  suppressWarnings({ storage.mode(values_storage) <- "numeric"})
  to_convert <- is.na(values_storage) & !(metabolite_vals %in% special_signs)
  
  if(any(to_convert)) metabolite_vals[to_convert] <- NA
  
  metabolite_vals
}


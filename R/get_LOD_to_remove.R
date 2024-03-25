#' Get metabolites to remove
#'
#' @description Returns metabolite names having more NA values in each group than the given treshold.
#' 
#' @param NA_info Attribute of the raw_data object.
#' @param treshold Percentage value.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' get_LOD_to_remove(attr(dat, "NA_info"), 0.1)
#' 
#' @export
#'

get_LOD_to_remove <- function(NA_info, treshold){
  names <- NA_info$NA_ratios %>%
    filter(all(NA_frac > treshold)) %>%
    distinct(metabolite)
  names$metabolite
}

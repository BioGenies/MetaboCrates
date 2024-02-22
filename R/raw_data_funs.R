
#' Specify group in the Biocrates data
#' 
#' @param dat a \code{\link{raw_data}} object. Output of [read_data()] function.
#' @param group_name a character name of a column from the data containing group.
#' 
#' @return \code{\link{raw_data}} object.
#' 
#' @seealso [read_data()]
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- add_group(dat, "group")
#' 
#' @export
#'

add_group <- function(dat, group_name) {
  
  if(!(group_name %in% colnames(dat)))
    stop(paste0("Provided column: ", group_name, " can't be found in your data!"))
  
  if(!is.null(attr(dat, "group")))
    warning("You already have group defined in your data. It will be replaced!")
  
  attr(dat, "group") <- group_name
  
  raw_data(as.data.frame(dat), 
           LOD_table = attr(dat, "LOD_table"), 
           metabolites = attr(dat, "metabolites"), 
           group = group_name)
  
}

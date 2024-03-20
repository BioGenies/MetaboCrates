
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

#' Get informations from Biocrates data
#' 
#' @param dat a \code{\link{raw_data}} object. Output of [read_data()] function.
#' 
#' @seealso [read_data()]
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' get_info(dat)
#' cat(get_info(dat))
#' 
#' dat <- add_group(dat, "group")
#' get_info(dat)
#' cat(get_info(dat))
#' 
#' @export
#' 

get_info <- function(dat){
  
  if(any(class(dat) != c("raw_data", "data.frame")))
    stop("dat must be a raw_data object.")
  
  info_str <- paste0("Data contains ", nrow(attr(dat, "samples")), " sample types and ", nrow(attr(dat, "NA_info")$counts), " NA types.")
  
  if(!is.null(attr(dat, "group"))){
    group_lvls <- dat %>%
      select(!!sym(attr(dat, "group"))) %>%
      unique()
    info_str <- paste0(info_str, "\nAdded group \"", attr(dat, "group"), "\" contains ", nrow(group_lvls), " levels.")
  }
  
  info_str
  
}

#' Print Biocrates data
#' 
#' @param dat a \code{\link{raw_data}} object. Output of [read_data()] function.
#' 
#' @seealso [read_data()]
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#'dat <- read_data(path)
#'print(dat)
#' 
#' dat <- add_group(dat, "group")
#' print(dat)
#' 
#' @export
#' 
# print.raw_data <- function(dat){
#   
#   if(any(class(dat) != c("raw_data", "data.frame")))
#     stop("dat must be a raw_data object.")
#   
#   rows <- min(nrow(dat), 10)
#   metabo_num <- min(length(attr(dat, "metabolites")), 10)
#   metabo_start <- which(colnames(dat) == attr(dat, "metabolites")[1])
#   
#   ratio_rows <- min(nrow(attr(dat, "NA_info")$NA_ratios), 10)
#   
#   dat_to_print <- dat %>%
#     select(`sample type`, all_of(metabo_start:(metabo_start + metabo_num - 1)))
#   
#   if(!is.null(attr(dat, "group"))){
#     group_str <- paste0("Specified group: ", attr(dat, "group"), "\n")
#     dat_to_print <- dat_to_print %>%
#       bind_cols(select(dat, !!sym(attr(dat, "group")))) %>%
#       select(1, !!sym(attr(dat, "group")), everything())
#   }else
#     group_str <- NULL
#     
#   
#   cat(paste0(group_str,
#              "Metabolites: ", paste0(attr(dat, "metabolites"), collapse = ", "), "\n",
#              paste0(capture.output(dat_to_print[1:rows,]), collapse = "\n"),
#              "\nShowing ", metabo_num, " out of ", length(attr(dat, "metabolites")), " metabolites\n",
#              "Showing ", rows, " out of ", nrow(dat), " rows\n",
#              "NA ratios:\n",
#              paste0(capture.output(attr(dat, "NA_info")$NA_ratios[1:ratio_rows,]), collapse = "\n"),
#              "\nShowing ", ratio_rows, " out of ", nrow(attr(dat, "NA_info")$NA_ratios), " rows"))
#   
# }


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

# Print Biocrates data
# 
# @param dat a \code{\link{raw_data}} object. Output of [read_data()] function.
# 
# @seealso [read_data()]
# 
# @examples
# path <- get_example_data("small_biocrates_example.xls")
# dat <- read_data(path)
# print(dat)
# 
# dat <- add_group(dat, "group")
# print(dat)
# 
# @export
# 
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
  
  NA_info$NA_ratios %>%
    group_by(metabolite) %>% 
    filter(all(NA_frac > treshold)) %>%
    distinct(metabolite) %>% 
    pull(metabolite)
  
}


#' Adding metabolites to the attribute removed
#' @param raw_data a \code{\link{raw_data}} object. Output of [read_data()] function.
#' @param metabolites_to_remove metabolites to remove
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' test_dat <- read_data(path)
#' attr(remove_LOD(test_dat, "C0"), "removed")
#' 
#' @export
#' 
remove_LOD <- function(raw_data, metabolites_to_remove) {
  
  attr(raw_data, "removed")[1] <- metabolites_to_remove
  
  raw_data
}


#' Setting attribute removed to NULL
#' @param raw_data a \code{\link{raw_data}} object. Output of [read_data()] function.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' test_dat <- read_data(path)
#' attr(remove_LOD(test_dat), "removed")
#' 
#' @export
#' 
unremove_LOD <- function(raw_data) {
  attr(raw_data, "removed")[1] <- NULL
  
  raw_data
}


#' Show metabolites without removed
#' 
#' @description Returns metabolites without those in removed attribute.
#' 
#' @param raw_data A raw_data object.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- remove_LOD(dat, "C0")
#' show_data(dat)
#' 
#' @export
#' 

show_data <- function(data){
  data %>%
    select(!unlist(attr(data, "removed")))
}


#' Show LOD ratios without removed metabolites
#' 
#' @description Returns LOD ratios without metabolites in removed attribute.
#' 
#' @param raw_data A raw_data object.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- remove_LOD(dat, "C0")
#' show_ratios(dat)
#' 
#' @export
#' 

show_ratios <- function(data){
  attr(data, "NA_info")$NA_ratios %>%
    filter(!metabolite %in% unlist(attr(data, "removed")))
}

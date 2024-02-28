
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
#' 
#' dat <- add_group(dat, "group")
#' get_info(dat)
#' 
#' @export
#' 

get_info <- function(dat){
  
  if(any(class(dat) != c("raw_data", "data.frame")))
    stop("dat must be a raw_data object.")
  
  info_str <- function(df){
    
    len <- min(nrow(df), 10)
    paste0(paste0(capture.output(df[1:len,]), collapse = "\n"),
           "\nShowing ", len, " out of ", nrow(df), " rows")
    
  }
  
  types_str <- paste0("Sample types:\n", info_str(attr(dat, "samples")))
  
  NA_types <- attr(dat, "NA_info")$counts
  NA_types_str <- paste0("NA types:\n", info_str(NA_types))
  
  if(!is.null(attr(dat, "group"))){
    group <- dat %>%
      count(!!sym(attr(dat, "group")))
    group_str <- paste0("Group \"", attr(dat, "group"), "\" levels:\n", info_str(group))
  }else
    group_str <- NULL
  
  cat(paste0(c(types_str, NA_types_str, group_str), collapse = "\n"))
  
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

print.raw_data  <- function(dat){

  if(any(class(dat) != c("raw_data", "data.frame")))
    stop("dat must be a raw_data object.")

  rows <- min(nrow(dat), 10)
  metabolites <- attr(dat, "metabolites")
  metabo_num <- min(length(metabolites), 10)

  ratio_rows <- min(nrow(attr(dat, "NA_info")$NA_ratios), 10)

  dat_to_print <- dat %>%
    select(`sample type`, all_of(metabolites))

  if(!is.null(attr(dat, "group"))){
    group_str <- paste0("Specified group: ", attr(dat, "group"), "\n")
    dat_to_print <- dat_to_print %>%
      bind_cols(select(dat, !!sym(attr(dat, "group")))) %>%
      select(1, !!sym(attr(dat, "group")), everything())
  }else
    group_str <- NULL

  cat(paste0(group_str,
             "Metabolites: ", paste0(attr(dat, "metabolites"), collapse = ", "), "\n",
             paste0(capture.output(dat_to_print[1:rows,]), collapse = "\n"),
             "\nShowing ", metabo_num, " out of ", length(attr(dat, "metabolites")), " metabolites\n",
             "Showing ", rows, " out of ", nrow(dat), " rows\n",
             "NA ratios:\n",
             paste0(capture.output(attr(dat, "NA_info")$NA_ratios[1:ratio_rows,]), collapse = "\n"),
             "\nShowing ", ratio_rows, " out of ", nrow(attr(dat, "NA_info")$NA_ratios), " rows"))

}


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
    stop(paste0("Provided column: ", group_name, 
                " can't be found in your data!"))
  
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
  
  info_str <- paste0("Data contains ", 
                     nrow(attr(dat, "samples")), 
                     " sample types and ", 
                     nrow(attr(dat, "NA_info")$counts), 
                     " NA types.")
  
  if(!is.null(attr(dat, "group"))){
    group_lvls <- dat %>%
      select(!!sym(attr(dat, "group"))) %>%
      unique()
    info_str <- paste0(info_str, "\nAdded group \"", 
                       attr(dat, "group"), "\" contains ", 
                       nrow(group_lvls), " levels.")
  }
  
  info_str
  
}


#' Get metabolites to remove
#'
#' @description Returns metabolite names having more NA values in each group 
#' than the given treshold.
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
#' @param raw_data a \code{\link{raw_data}} object. Output of [read_data()] 
#' function.
#' @param metabolites_to_remove metabolites to remove
#' @param type type of metabolites to remove
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' test_dat <- read_data(path)
#' attr(remove_metabolites(test_dat, "C0", "LOD"), "removed")
#' 
#' @export
#' 
remove_metabolites <- function(raw_data, metabolites_to_remove, type) {
  type <- match.arg(arg = type, choices = c("LOD", "LOD_man", "QC", "QC_man"))
  attr(raw_data, "removed")[[type]] <- metabolites_to_remove
  
  raw_data
}


#' Setting attribute removed to NULL
#' @param raw_data a \code{\link{raw_data}} object. Output of [read_data()] 
#' function.
#' @param type type of metabolites to remove
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' test_dat <- read_data(path)
#' attr(remove_metabolites(test_dat, "C0", "LOD"), "removed")
#' attr(unremove_metabolites(test_dat, "LOD"), "removed")
#' 
#' @export
#' 
unremove_metabolites <- function(raw_data, type) {
  type <- match.arg(arg = type, choices = c("LOD", "LOD_man", "QC", "QC_man"))
  attr(raw_data, "removed")[type] <- list(NULL)
  
  raw_data
}


#' Show metabolites without removed
#' 
#' @description Returns metabolites without those in removed attribute.
#' 
#' @param data A raw_data object.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- remove_metabolites(dat, "C0", type = "LOD")
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
#' @param data A raw_data object.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- remove_metabolites(dat, "C0", type = "LOD")
#' show_ratios(dat)
#' 
#' @export
#' 

show_ratios <- function(data){
  attr(data, "NA_info")$NA_ratios %>%
    filter(!metabolite %in% unlist(attr(data, "removed")))
}

#' Calculate CV for different QC samples depending on metabolite
#' 
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#' 
#' @param dat A raw_data object.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat[,18:26] <- apply(dat[,18:26], 2, function(dat){
#' dat[which(dat == "< LOD" | dat == "> ULOQ" | is.na(dat) | dat == "< LLOQ" | dat == "NA")] <- 0
#' dat
#' })
#' calculate_CV(dat)
#' 
#' @export

calculate_CV <- function(dat){
  dat %>%
    select(`sample type`, all_of(attr(dat, "metabolites"))) %>%
    filter(str_detect(`sample type`, "^QC")) %>%
    pivot_longer(!`sample type`, 
                 names_to = "metabolite", 
                 values_to = "value") %>%
    group_by(`sample type`, metabolite) %>%
    mutate(value = as.numeric(value)) %>% 
    summarise(CV = ifelse(n() == 1, NA, sd(value) / mean(value))) %>%
    group_by(`sample type`, metabolite) %>% 
    filter(all(!is.na(CV))) %>%
    ungroup()
}

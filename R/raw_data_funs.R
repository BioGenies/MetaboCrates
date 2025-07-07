
#' Group data
#' 
#' @description
#' `add_group()` groups data by one or more columns.
#' 
#' @param dat a \code{\link{raw_data}} object. Output of [read_data()]
#' function.
#' @param group_names a single character string or character vector
#' specifying the names of the columns to group the data by.
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
#' dat <- add_group(dat, c("group", "species"))
#' 
#' @export
#'

add_group <- function(dat, group_names) {
  
  if(!all(group_names %in% colnames(dat)))
    stop(paste0("Some of the provided columns: ", group_names, 
                ", can't be found in your data!"))
  
  if(!is.null(attr(dat, "group")))
    warning("You already have grouping defined in your data. It will be replaced!")
  
  attr(dat, "group") <- group_names
  
  raw_data(as.data.frame(dat), 
           LOD_table = attr(dat, "LOD_table"), 
           metabolites = attr(dat, "metabolites"),
           group = group_names)
  
}

#' Get information about data
#' 
#' @description
#' `get_info()` returns information about the number of sample types,
#' missing values types and grouping (if applied).
#' 
#' @inheritParams add_group
#' 
#' @seealso [read_data()]
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' cat(get_info(dat))
#' 
#' dat <- add_group(dat, "group")
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
                     nrow(attr(dat, "NA_info")[["counts"]]), 
                     " NA types.")
  
  if(!is.null(attr(dat, "group"))){
    group_lvls <- dat %>%
      select(all_of(attr(dat, "group"))) %>%
      unique() %>%
      nrow()
    
    info_str <- paste0(info_str, "\nGroupping by: \"", 
                       attr(dat, "group"), "\" (", 
                       group_lvls, " levels).")
  }
  
  info_str
  
}


#' Get metabolites to remove based on missing values proportion
#'
#' @description
#' `get_LOD_to_remove()` returns the names of metabolites having the higher
#' proportion of missing values than the given threshold.
#' 
#' @inheritParams add_group
#' 
#' @param threshold a decimal specifying the minimum proportion of missing
#' values a metabolite must have to be removed.
#' @param use_group logical. If `TRUE`, a metabolite will be returned only if
#' the proportion of missing values exceeds the threshold in every group level.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' get_LOD_to_remove(dat, 0.1)
#' 
#' @export
#'

get_LOD_to_remove <- function(dat, threshold = 0.8, use_group = TRUE){
  
  if(is.null(attr(dat, "group")) & use_group) {
    message("No group to use! It will be ignored.
            If you want to use group provide it with add_group function first.")
    use_group <- FALSE
  }

  if(use_group) {
    attr(dat, "NA_info")[["NA_ratios_group"]] %>%
      group_by(metabolite) %>%
      filter(all(NA_frac >= threshold)) %>%
      pull(metabolite) %>% 
      unique()
  } else {
    attr(dat, "NA_info")[["NA_ratios_type"]] %>%
      group_by(metabolite) %>% 
      reframe(NA_frac = sum(NA_frac)) %>% 
      filter(NA_frac >= threshold) %>% 
      pull(metabolite) %>% 
      unique()
  }
}


#' Add metabolites to the attribute `removed`
#' 
#' @description
#' `remove_metabolites()` remove the specified metabolites from data by adding
#' them to the attribute `removed`.
#' 
#' @inheritParams add_group
#' 
#' @param metabolites_to_remove a character string or vector specifying the
#' names of metabolites to remove.
#' @param type a character string specifying the criterion used to evaluate
#' whether a metabolite should be removed. Can be one of `LOD`, `QC` or
#' `QC_man`.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' test_dat <- read_data(path)
#' attr(remove_metabolites(test_dat, "C0", "LOD"), "removed")
#' 
#' @export
#' 
remove_metabolites <- function(dat, metabolites_to_remove, type) {
  if(!all(metabolites_to_remove %in% attr(dat, "metabolites")))
    stop("Some of the provided metabolites are not present in the data.")
  
  if(any(metabolites_to_remove %in% attr(dat, "removed")))
    stop("Some of the provided metabolites have already been removed.")
  
  type <- match.arg(arg = type, choices = c("LOD", "QC", "QC_man"))
  attr(dat, "removed")[[type]] <- 
    c(attr(dat, "removed")[[type]], metabolites_to_remove)
  
  dat
}


#' Setting the specified type in attribute `removed` to `NULL`
#' 
#' @description
#' `unremove_all()` set all the metabolites from the specified type as not
#' removed.
#' 
#' @inheritParams add_group
#' 
#' @param type a character string indicating the type from which to restore
#' all metabolites.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' test_dat <- read_data(path)
#' test_dat <- remove_metabolites(test_dat, "C0", "LOD")
#' attr(test_dat, "removed")
#' attr(unremove_all(test_dat, "LOD"), "removed")
#' 
#' @export
#' 
unremove_all <- function(dat, type) {
  type <- match.arg(arg = type, choices = c("LOD", "QC", "QC_man"))
  attr(dat, "removed")[type] <- list(NULL)
  
  dat
}

#' Removing the specified metabolites from the attribute `removed`
#' 
#' @description
#' `unremove_metabolites()` flags the specified metabolites as not removed. 
#'
#' 
#' @inheritParams add_group
#' 
#' @param metabolites a character string or vector specifying the names of
#' metabolites to restore.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' test_dat <- read_data(path)
#' test_dat <- remove_metabolites(test_dat, c("C0", "C2"), "LOD")
#' test_dat <- remove_metabolites(test_dat, "C0", "QC")
#' attr(test_dat, "removed")
#' attr(unremove_metabolites(test_dat, "C0"), "removed")
#' 
#' @export
#' 
unremove_metabolites <- function(dat, metabolites){
  attr(dat, "removed") <- lapply(
    attr(dat, "removed"), function(type){
      new_removed <- type[which(!(type %in% metabolites))]
      if(length(new_removed) == 0 | is.null(new_removed)) NULL else new_removed
    })
  
  dat
}

#' Show metabolites without removed ones
#' 
#' @description
#' `show_data()` returns metabolites without those in the attribute `removed`.
#' 
#' 
#' @inheritParams add_group
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- remove_metabolites(dat, "C0", type = "LOD")
#' show_data(dat)
#' 
#' @export
#' 

show_data <- function(dat){
  dat %>%
    select(!all_of(unlist(attr(dat, "removed"))))
}


#' Show missing values ratios without those in the removed metabolites
#' 
#' @description
#' `show_ratios()` returns LOD ratios without metabolites in removed attribute.
#' 
#' 
#' @inheritParams add_group
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- remove_metabolites(dat, "C0", type = "LOD")
#' show_ratios(dat)
#' 
#' @export
#' 

show_ratios <- function(dat){
  attr(dat, "NA_info")[["NA_ratios_type"]] %>%
    filter(!metabolite %in% unlist(attr(dat, "removed"))) %>%
    group_by(metabolite) %>%
    summarise(NA_frac = sum(NA_frac))
}

#' Calculate CV for different QC samples for each metabolite
#' 
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#' @importFrom stats sd
#' 
#' @inheritParams add_group
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' calculate_CV(dat)
#' 
#' @export

calculate_CV <- function(dat){
  if(is.null(attr(dat, "completed"))){
    stop("Complete data first.")
  }
  
  attr(dat, "cv") <- attr(dat, "completed") %>%
    select(`sample type`, all_of(attr(dat, "metabolites"))) %>%
    filter(str_detect(`sample type`, "^QC")) %>%
    pivot_longer(!`sample type`, 
                 names_to = "metabolite", 
                 values_to = "value") %>%
    group_by(`sample type`, metabolite) %>%
    mutate(value = as.numeric(value)) %>% 
    summarise(
      CV = ifelse(n() == 1, NA, sd(value, na.rm = T) / mean(value, na.rm = T))
    ) %>%
    group_by(`sample type`, metabolite) %>% 
    filter(all(!is.na(CV))) %>%
    ungroup()
  
  dat
}

#' Get CV to remove
#'
#' @description
#' `get_CV_to_remove()` returns metabolite names having more CV value 
#' than the given threshold.
#' 
#' @inheritParams add_group
#' 
#' @param threshold a decimal specifying the minimum CV a metabolite must have
#' to be removed.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' dat <- calculate_CV(dat)
#' get_CV_to_remove(dat, 0.3)
#' 
#' @export
#'
get_CV_to_remove <- function(dat, threshold){
  if(is.null(attr(dat, "cv"))){
    stop("First, calculate the coefficient of variation using calculate_CV().")
  }
  attr(dat, "cv") %>%
    filter(CV > threshold) %>%
    distinct(metabolite) %>% 
    pull(metabolite)
}

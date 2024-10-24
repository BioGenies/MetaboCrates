
#' Specify group in the Biocrates data
#' 
#' @param raw_data a \code{\link{raw_data}} object. Output of [read_data()] 
#' function.
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

add_group <- function(raw_data, group_name) {
  
  if(!(group_name %in% colnames(raw_data)))
    stop(paste0("Provided column: ", group_name, 
                " can't be found in your data!"))
  
  if(!is.null(attr(raw_data, "group")))
    warning("You already have group defined in your data. It will be replaced!")
  
  attr(raw_data, "group") <- group_name
  
  raw_data(as.data.frame(raw_data), 
           LOD_table = attr(raw_data, "LOD_table"), 
           metabolites = attr(raw_data, "metabolites"), 
           group = group_name)
  
}

#' Get informations from Biocrates data
#' 
#' @inheritParams add_group
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

get_info <- function(raw_data){
  
  if(any(class(raw_data) != c("raw_data", "data.frame")))
    stop("dat must be a raw_data object.")
  
  info_str <- paste0("Data contains ", 
                     nrow(attr(raw_data, "samples")), 
                     " sample types and ", 
                     nrow(attr(raw_data, "NA_info")[["counts"]]), 
                     " NA types.")
  
  if(!is.null(attr(raw_data, "group"))){
    group_lvls <- raw_data %>%
      select(!!sym(attr(raw_data, "group"))) %>%
      unique()
    info_str <- paste0(info_str, "\nAdded group \"", 
                       attr(raw_data, "group"), "\" contains ", 
                       nrow(group_lvls), " levels.")
  }
  
  info_str
  
}


#' Get metabolites to remove
#'
#' @description Returns metabolite names having more NA values in each group 
#' than the given threshold.
#' 
#' @inheritParams add_group
#' 
#' @param threshold Percentage value.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' get_LOD_to_remove(raw_data, 0.1)
#' 
#' @export
#'

get_LOD_to_remove <- function(raw_data, threshold = 0.8, use_group = TRUE){
  
  if(is.null(attr(raw_data, "group")) & use_group) {
    message("No group to use! It will be ignored. 
If you want to use group provide it with add_group function first.")
    use_group <- FALSE
  }

  if(use_group) {
    to_remove <- attr(raw_data, "NA_info")[["NA_ratios_group"]] %>% 
      group_by(metabolite) %>% 
      filter(all(NA_frac >= threshold)) %>%
      pull(metabolite) %>% 
      unique()
  } else {
    to_remove <- attr(raw_data, "NA_info")[["NA_ratios_type"]] %>% 
      group_by(metabolite) %>% 
      reframe(NA_frac = sum(NA_frac)) %>% 
      filter(NA_frac >= threshold) %>% 
      pull(metabolite) %>% 
      unique()
  }
  
  to_remove
}


#' Adding metabolites to the attribute removed
#' 
#' @inheritParams add_group
#' 
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
  type <- match.arg(arg = type, choices = c("LOD", "QC", "QC_man"))
  attr(raw_data, "removed")[[type]] <- 
    c(attr(raw_data, "removed")[[type]], metabolites_to_remove)
  
  raw_data
}


#' Setting attribute removed to NULL
#' 
#' @inheritParams add_group
#' 
#' @param type type of metabolites to remove
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' test_dat <- read_data(path)
#' attr(remove_metabolites(test_dat, "C0", "LOD"), "removed")
#' attr(unremove_all(test_dat, "LOD"), "removed")
#' 
#' @export
#' 
unremove_all <- function(raw_data, type) {
  type <- match.arg(arg = type, choices = c("LOD", "QC", "QC_man"))
  attr(raw_data, "removed")[type] <- list(NULL)
  
  raw_data
}

#' Removing given metabolites from the attribute removed
#' 
#' @inheritParams add_group
#' 
#' @param metabolites a vector of metabolites to unremove
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' test_dat <- read_data(path)
#' test_dat <- remove_metabolites(test_dat, c("C0", "C1", "C2"), "LOD")
#' test_dat <- remove_metabolites(test_dat, "C0", "QC")
#' attr(test_dat, "removed")
#' attr(unremove_metabolites(test_dat, c("C0", "C1")), "removed")
#' 
#' @export
#' 
unremove_metabolites <- function(raw_data, metabolites){
  attr(raw_data, "removed") <- lapply(
    attr(raw_data, "removed"), function(type){
      new_removed <- type[which(!(type %in% metabolites))]
      if(length(new_removed) == 0 | is.null(new_removed)) NULL else new_removed
    })
  
  raw_data
}

#' Show metabolites without removed
#' 
#' @description Returns metabolites without those in removed attribute.
#' 
#' @param dat A raw_data object.
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
    select(!unlist(attr(dat, "removed")))
}


#' Show LOD ratios without removed metabolites
#' 
#' @description Returns LOD ratios without metabolites in removed attribute.
#' 
#' @param dat A raw_data object.
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

#' Calculate CV for different QC samples depending on metabolite
#' 
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#' @importFrom stats sd
#' 
#' @param dat A raw_data object.
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
#' @description Returns metabolite names having more CV value 
#' than the given threshold.
#' 
#' @param dat object with CV attribute
#' @param threshold
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
    stop("First calculate the coefficient of variation using calculate_CV().")
  }
  attr(dat, "cv") %>%
    filter(CV > threshold) %>%
    distinct(metabolite) %>% 
    pull(metabolite)
}

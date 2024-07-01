#' Metabocrates ggplot theme
#' 
#' @keywords internal

metabocrates_theme <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(colour = "#D0D0D0", linewidth = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y =  element_line(colour = "#D0D0D0", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.text.x = element_text(margin = margin(b = 5)),
    axis.text.y = element_text(margin = margin(l = 5))
  )
}


#' Barplot of groups sizes
#' 
#' @import ggplot2
#' 
#' @param dat a \code{\link{raw_data}} object. Output of [read_data()] function
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- add_group(dat, "group")
#' plot_groups(dat)
#' 
#' @export

plot_groups <- function(dat){
  if(is.null(attr(dat, "group"))){
    stop("No groups column specified in data. You can add grouping using add_group() function.")
  }

dat %>%
  filter(`sample type` == "Sample") %>%
  ggplot(aes(x = as.character(get(attr(dat, "group"))))) +
    geom_bar() +
    labs(x = "Groups",
         y = "Count",
         title = paste0("Number of elements in groups of column \"",
                        attr(dat, "group"),
                        "\"")) +
    metabocrates_theme()
}


#' Barplot of metabolomics numbers
#' 
#' @import ggplot2
#' 
#' @param dat a \code{\link{raw_data}} object. Output of [read_data()] function
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' test_dat <- read_data(path)
#' plot_mv_types(test_dat)
#' 
#' @export

plot_mv_types <- function(dat) {
  NA_info <- attr(dat, "NA_info")
  counts <- NA_info[["counts"]]
  
  counts %>%
    rename(count = "n") %>%
    filter(count > 0) %>%
    ggplot(aes(x = type, y = count)) +
    geom_col(fill = "#2A528A") +
    geom_label(aes(x = type, y = count, label = count)) +
    metabocrates_theme()
}

#' Barplot of missing metabolites values
#' 
#' @importFrom scales percent
#' 
#' @inheritParams plot_mv_types
#' 
#' @param type NULL, "Type" or "group"
#' 
#' @export

plot_NA_percent <- function(dat, type = NULL){
  NA_percent <- dat %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(c(attr(dat, "metabolites"), attr(dat, "group")))) %>%
    pivot_longer(attr(dat, "metabolites"),
                 names_to = "Metabolite",
                 values_to = "Type") %>%
    group_by(Metabolite, attr(dat, "group")) %>%
    mutate(`Number of values` = n()) %>%
    filter(Type %in% c("< LOD","< LLOQ",
                        "> ULOQ", "NA", "∞")) %>%
    ungroup() %>%
    group_by(Metabolite, attr(dat, "group"), Type) %>%
    summarise(`% Missing` = n()/first(`Number of values`))
  
  if(is.null(type)){
    plt <- ggplot(NA_percent, aes(x = Metabolite, y = `% Missing`,
                                  alpha = type))
  }else{
    if(type == "group"){
      if(is.null(attr(dat, "group"))) stop("No group defined")
      type <- attr(dat, "group")
    }
      
    plt <- ggplot(NA_percent, aes(x = Metabolite, y = `% Missing`,
                                  fill = get(type)))
  }
  
  plt +
    geom_col() +
    scale_y_continuous(labels = scales::percent) +
    geom_label(data =
                 attr(dat, "NA_info")[["NA_ratios"]]["metabolite", "NA_frac"],
               aes(x = metabolite, y = NA_frac,
                   label = paste0(round(NA_frac*100), "%"),
                   fill = NULL)) +
    coord_flip() +
    metabocrates_theme()
}

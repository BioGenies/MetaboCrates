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
    plot.title = element_text(face = "bold", hjust = 0.5,
                              margin = margin(b = 10)),
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

#' Barplot of missing metabolites percents
#' 
#' @importFrom scales percent
#' 
#' @param type NULL, "NA_type" or "group"
#' 
#' @examples 
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' plot_NA_percent(dat)
#' plot_NA_percent(dat, "NA_type")
#' dat <- add_group(dat, "group")
#' plot_NA_percent(dat, "group")
#' @inheritParams plot_mv_types
#' 
#' @param type NULL, "Type" or "group"
#' 
#' @export

plot_NA_percent <- function(dat, type = NULL){
  if(is.null(type)){
    if(!is.null(attr(dat, "group"))){
      ratios <- attr(dat, "NA_info")[["NA_ratios"]] %>%
        group_by(metabolite) %>%
        summarise(NA_frac = mean(NA_frac))
    }else ratios <- attr(dat, "NA_info")[["NA_ratios"]]
      
    labels <- unlist(as.character(
      paste0(round(ratios[["NA_frac"]]*100), "%")
      ))
      
    ggplot(ratios,
           aes(x = metabolite, y = NA_frac)) +
      geom_col(width = 0.3) +
      scale_y_continuous(labels = scales::percent) +
      geom_label(aes(label = labels)) +
      coord_flip() +
      metabocrates_theme()
  }else if(type == "NA_type"){
    
    
    NA_percent <- dat %>%
      filter(`sample type` == "Sample") %>%
      select(all_of(c(attr(dat, "metabolites")))) %>%
      pivot_longer(attr(dat, "metabolites"),
                   names_to = "Metabolite",
                   values_to = "Type") %>%
      group_by(Metabolite) %>%
      mutate(`Number of values` = n()) %>%
      filter(Type %in% attr(dat, "NA_info")[["counts"]][["type"]]) %>%
      ungroup() %>%
      group_by(Metabolite, Type) %>%
      summarise(`% Missing` = n()/first(`Number of values`)) %>%
      ungroup()
    
    all_NA_percent <- expand.grid(
      Metabolite = attr(dat, "metabolite"),
      Type = attr(dat, "NA_info")[["counts"]][["type"]]) %>%
      left_join(NA_percent, by = c("Metabolite", "Type")) %>%
      mutate(`% Missing` = ifelse(is.na(`% Missing`), 0, `% Missing`))
    
    labels <- unlist(as.character(
      paste0(round(all_NA_percent[["% Missing"]]*100), "%")
    ))
    
    ggplot(all_NA_percent, aes(x = Type, y = `% Missing`,
                           fill = Type)) +
      geom_col(width = 1.2, position = "dodge", color = "white") +
      scale_y_continuous(labels = scales::percent) +
      geom_label(aes(label = labels), size = 2.6,
                 position = position_dodge(width = 1.2)) +
      coord_flip() +
      facet_wrap(~ Metabolite, ncol = 1) +
      metabocrates_theme()
  }else if(type == "group"){
    labels <- unlist(as.character(
      paste0(round(attr(dat, "NA_info")[["NA_ratios"]][["NA_frac"]]*100), "%")
    ))
    
    ggplot(attr(dat, "NA_info")[["NA_ratios"]],
           aes(x = metabolite, y = NA_frac, fill = as.factor(`get("group")`))) +
      geom_col(width = 0.6, position = "dodge", color = "white") +
      scale_y_continuous(labels = scales::percent) +
      geom_label(aes(label = labels), size = 2.6,
                 position = position_dodge(width= 0.6)) +
      coord_flip() +
      metabocrates_theme()
  }
}

#' Heatmap of missing metabolites values
#' 
#' @examples 
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' plot_heatmap(dat)
#' 
#' @export

plot_heatmap <- function(dat){
  dat %>%
    select(all_of(attr(dat, "metabolites"))) %>%
    mutate("Sample" = 1:n()) %>%
    pivot_longer(!Sample, names_to = "Metabolite", values_to = "Value") %>%
    mutate(`Is missing` =
             Value %in% c("< LOD","< LLOQ", "> ULOQ", "NA", "∞", NA)) %>%
    ggplot(aes(x = Sample, y = Metabolite, fill = `Is missing`)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c(`FALSE` = "grey", `TRUE` = "black")) +
    metabocrates_theme()
}

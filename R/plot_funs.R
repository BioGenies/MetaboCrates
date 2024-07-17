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
  group_by(Groups = get(attr(dat, "group"))) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Groups, y = Count)) +
  geom_bar(stat = "identity") +
  labs(title = paste0("Number of elements in groups of column \"",
                        attr(dat, "group"),
                        "\"")) +
  geom_label(aes(label = Count)) +
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

#' ggplot object for plot_NA_percent with type "joint"
#'
#' @keywords internal

create_joint_NA_plot <- function(dat){
    if(!is.null(attr(dat, "group"))){
      NA_percent <- attr(dat, "NA_info")[["NA_ratios"]] %>%
        group_by(metabolite) %>%
        summarise(NA_frac = mean(NA_frac))
    }else NA_percent <- attr(dat, "NA_info")[["NA_ratios"]]
    
  labels <- unlist(as.character(
    paste0(round(NA_percent[["NA_frac"]]*100), "%")
  ))
    
  ggplot(NA_percent, aes(x = metabolite, y = NA_frac, label = labels))
}
  
#' ggplot object for plot_NA_percent with type "NA_type"
#'
#' @keywords internal
 
create_NA_type_NA_plot <- function(dat){
  NA_percent <- dat %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(attr(dat, "metabolites"))) %>%
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
  
  labels <- unlist(as.character(
    paste0(round(NA_percent[["% Missing"]]*100), "%")
  ))
  
  ggplot(NA_percent,
         aes(x = Metabolite, y = `% Missing`, fill = Type, label = labels)) +
    labs(fill = "Missing values types")
}

#' ggplot object for plot_NA_percent with type "group"
#'
#' @keywords internal

create_group_NA_plot <- function(dat){
  NA_percent <- dat %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(c(attr(dat, "metabolites"), attr(dat, "group")))) %>%
    pivot_longer(attr(dat, "metabolites"),
                 names_to = "Metabolite",
                 values_to = "Type") %>%
    group_by(Metabolite) %>%
    mutate(`Number of values` = n(), Group = get(attr(dat, "group"))) %>%
    filter(Type %in% attr(dat, "NA_info")[["counts"]][["type"]]) %>%
    ungroup() %>%
    group_by(Metabolite, Group) %>%
    summarise(`% Missing` = n()/first(`Number of values`)) %>%
    ungroup()
  
  labels <- unlist(as.character(
    paste0(round(NA_percent[["% Missing"]]*100), "%")
  ))
  
  ggplot(NA_percent,
         aes(x = Metabolite, y = `% Missing`, fill = as.factor(Group),
             label = labels)) +
    labs(fill = "Groups")
}

#' Barplot of missing metabolites percents
#' 
#' @importFrom scales percent
#' 
#' @inheritParams plot_mv_types
#' 
#' @param type a character denoting which type of plot should be made. This 
#' function accepts either "joint", "NA_type" or "group". TODO: what does it mean?
#' 
#' @examples 
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' plot_NA_percent(dat)
#' plot_NA_percent(dat, "NA_type")
#' dat <- add_group(dat, "group")
#' plot_NA_percent(dat, "group")
#' 
#' @export

plot_NA_percent <- function(dat, type = "joint"){
  ggplot_obj <- switch(type,
    "joint" = create_joint_NA_plot(dat),
    "NA_type" = create_NA_type_NA_plot(dat),
    "group" = create_group_NA_plot(dat))
  
  ggplot_obj +
    geom_col(width = 0.4, position = "stack", color = "white") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Metabolites", y = "% Missing") +
    geom_text(size = 2.6, position = position_stack(vjust = 0.5)) +
    coord_flip() +
    metabocrates_theme()
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
    mutate(Sample = 1:n()) %>%
    pivot_longer(!Sample, names_to = "Metabolite", values_to = "Value") %>%
    mutate(Metabolite = factor(Metabolite, ordered = TRUE),
           `Is missing` =
             Value %in% c("< LOD","< LLOQ", "> ULOQ", "NA", "∞", NA)) %>%
    ggplot(aes(x = Sample, y = Metabolite, fill = `Is missing`)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c(`FALSE` = "grey", `TRUE` = "black")) +
    metabocrates_theme()
}

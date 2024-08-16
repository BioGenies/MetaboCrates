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

#' Long tibble with metabolites
#' 
#' @keywords internal

create_long_metabolites_tibble <- function(dat){
  dat %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(attr(dat, "metabolites"))) %>%
    pivot_longer(attr(dat, "metabolites"),
                 names_to = "Metabolite",
                 values_to = "Type")
}
  
#' ggplot object for plot_NA_percent with type "NA_type"
#'
#' @keywords internal
 
create_NA_type_NA_plot <- function(dat){
  NA_percent <- create_long_metabolites_tibble(dat) %>%
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
  NA_percent <- create_long_metabolites_tibble(dat) %>%
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

#' Barplot of missing values percents
#' 
#' @importFrom scales percent
#' 
#' @inheritParams plot_mv_types
#' 
#' @param type a character denoting which type of plot should be made. This 
#' function accepts either "joint", "NA_type" or "group".
#' Default type is "joint", which creates plot of missing values percents
#' in each metabolite. Types "NA_type" and "group" add the division into
#' all missing values types and levels in grouping column respectively.
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

#' Histogram of individual metabolite values
#' 
#' @param metabolite The name of metabolite of interest.
#' @param bins_num Number of bins on a histogram. Defaults to 30.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_histogram(attr(dat, "completed"), "C0")
#' 
#' @export

create_histogram <- function(dat, metabolite, bins_num = 30){
  dat %>%
    filter(`sample type` == "Sample") %>%
    select(metabolite) %>%
    ggplot(aes(x = get(metabolite))) +
    geom_histogram(bins = bins_num) +
    labs(x = metabolite, y = "Count") +
    metabocrates_theme()
}

#' Boxplot of individual metabolite values
#' 
#' @param metabolite The name of metabolite of interest.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_boxplot(attr(dat, "completed"), "C0")
#' 
#' @export

create_boxplot <- function(dat, metabolite){
  dat %>%
    filter(`sample type` == "Sample") %>%
    select(metabolite) %>%
    ggplot(aes(x = metabolite, y = get(metabolite))) +
    geom_boxplot() +
    labs(x = NULL, y = "Value") +
    metabocrates_theme()
}

#' Qqplot of individual metabolite values
#' 
#' @param metabolite The name of metabolite of interest.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_qqplot(attr(dat, "completed"), "C0")
#' 
#' @export

create_qqplot <- function(dat, metabolite){
  dat %>%
    filter(`sample type` == "Sample") %>%
    select(metabolite) %>%
    ggplot(aes(sample = get(metabolite))) +
    geom_qq() +
    geom_qq_line() +
    labs(x = "Normal quantiles", y =  paste0(metabolite, " quantiles")) +
    metabocrates_theme()
}

#' Heatmap of correlations between metabolites
#' 
#' @importFrom stringr str_trunc
#' @importFrom reshape2 melt
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_correlations_heatmap(attr(dat, "completed"))
#' 
#' @export

create_correlations_heatmap <- function(dat){
  dat %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(attr(dat, "metabolites"))) %>%
    cor(use = "na.or.complete") %>%
    melt() %>%
    ggplot(aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_x_discrete(labels = function(x) str_trunc(x, 10)) +
    scale_y_discrete(labels = function(x) str_trunc(x, 10)) +
    labs(x = "Metabolites", y = "Metabolites") +
    metabocrates_theme() +
    theme(axis.text.x = element_text(angle = 90))
}

#' PCA plot
#' 
#' @import ggfortify
#' @importFrom tidyr drop_na
#' @importFrom stringr str_to_title
#' 
#' @param type a character denoting which type of PCA plot should be created.
#' Default is "sample_type", which makes a plot for quality control. Type
#' "group" creates a PCA plot with respect to the groups of samples with type
#' 'Sample'.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_PCA_plot(attr(dat, "completed"))
#' create_PCA_plot(attr(dat, "completed"), type = "group")
#' 
#' @export

create_PCA_plot <- function(dat, type = "sample_type"){
  dat <- switch(type,
                "sample_type" = rename(dat, sample_type = `sample type`),
                "group" = filter(dat, `sample type` == "Sample"))
  
  dat_without_na <- dat %>%
    drop_na(all_of(attr(dat, "metabolites"))) %>%
    mutate(across(all_of(type), ~ factor(., ordered = TRUE)))
  
  dat_without_na %>%
    select(all_of(attr(dat, "metabolites"))) %>%
    select(where(~ sd(.) != 0)) %>%
    prcomp(scale. = TRUE) %>%
    autoplot(data = dat_without_na, color = type,
             frame = TRUE, frame.type = "norm") +
    scale_color_discrete(name = str_to_title(gsub("_", " ", type))) +
    scale_fill_discrete(name = str_to_title(gsub("_", " ", type))) +
    metabocrates_theme()
}

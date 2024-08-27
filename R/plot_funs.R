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

#' Histograms of individual metabolites values
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_histograms(dat)
#' 
#' @export

create_histograms <- function(dat){
  create_long_metabolites_tibble(dat) %>%
    mutate(Type = ifelse(is.na(Type), 0, as.integer(Type))) %>%
    ggplot(aes(x = Type)) +
    geom_histogram() +
    facet_wrap(~ Metabolite, ncol = 1, scales = "free_y") +
    labs(x = "Value", y = "Count") +
    metabocrates_theme()
}

#' Boxplots of individual metabolites values
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_boxplots(dat)
#' 
#' @export

create_boxplots <- function(dat){
  create_long_metabolites_tibble(dat) %>%
    mutate(Type = ifelse(is.na(Type), 0, as.integer(Type))) %>%
    ggplot(aes(x = Type, y = Metabolite)) +
    geom_boxplot() +
    labs(x = "Value", y = "Metabolite") +
    metabocrates_theme()
}

#' Qqplots of individual metabolites values
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_qqplots(dat)
#' 
#' @export

create_qqplots <- function(dat){
  create_long_metabolites_tibble(dat) %>%
    mutate(Type = ifelse(is.na(Type), 0, as.integer(Type))) %>%
    ggplot(aes(sample = Type)) +
    geom_qq() +
    geom_qq_line() +
    facet_wrap(~ Metabolite, ncol = 1, scales = "free_y") +
    labs(x = "Normal quantiles", y = "Sample quantiles") +
    metabocrates_theme()
}

#' Heatmap of correlations between metabolites
#' 
#' @importFrom reshape2 melt
#' 
#' @export

create_correlations_heatmap <- function(dat){
  create_long_metabolites_tibble(dat) %>%
    cor() %>%
    melt() %>%
    ggplot(aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    labs(x = "Metabolites", y = "Metabolites")
    metabocrates_theme()
}


#' Estimated density of metabolite with LOD cut-off
#' 
#' @export
create_histogram_with_lod <- function(dat, metabolite_name) {
  metabolites <- attr(dat, "metabolites")
  
  lod_info <- attr(dat, "LOD_table") %>%
    filter(type == "LOD (calc.)") %>%
    pull(metabolite_name)  %>%
    sum()
  
  plot_data <- attr(dat, "completed") %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(metabolite_name)) %>%
    pivot_longer(cols = all_of(metabolite_name), names_to = "Metabolite", values_to = "Value") %>%
    mutate(Value = as.numeric(Value))
  
  ggplot(plot_data, aes(x = Value)) +
    geom_density() +
    geom_vline(xintercept = lod_info, color = "red", linetype = "dashed", size = 1) +
    labs(x = paste(metabolite_name), y = "Count",
         title = paste("Histogram of", metabolite_name, "with LOD Cutoff")) +
    metabocrates_theme()
}


#' Plot of two metabolites
#' 
#' @export
create_plot_of_2_metabolites <- function(dat, metabolite1, metabolite2) {
  plot_data <- attr(dat, "completed") %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(c("plate bar code", metabolite1, metabolite2)))
  
  LOD <- attr(dat, "LOD_table") %>%
    filter(type == "LOD (calc.)") %>%
    select(all_of(c("plate bar code", metabolite1, metabolite2))) %>%
    filter(if_all(everything(), ~ . != 0))
  
  LOD["plate bar code"] <- sapply(LOD["plate bar code"], function(x) gsub("^.*\\s([0-9]+-[0-9]+)\\s.*$", "\\1", x))
  
  grouped_data <- plot_data %>%
    group_by(`plate bar code`)
  
  
  ggplot(grouped_data, aes(x = get(metabolite1), y = get(metabolite2), color = `plate bar code`)) +
    geom_point() +
    geom_hline(data = LOD, aes(yintercept = .data[[paste0(metabolite2)]], color = `plate bar code`), linetype = "dashed") +
    geom_vline(data = LOD, aes(xintercept = .data[[paste0(metabolite1)]], color = `plate bar code`), linetype = "dashed") +
    labs(x = paste(metabolite1), y = paste(metabolite2)) +
    metabocrates_theme()
}


#' pca
#' 
#' @export
pca_variance <- function(dat) {
  data <- attr(dat, "completed") %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(attr(dat, "metabolites")))
  
  data <- data[complete.cases(data), sapply(data, function(col) var(col, na.rm = TRUE) > 0)]

  pca_result <- prcomp(data, scale. = TRUE, center = TRUE)
  
  variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  
  cumulative_variance <- cumsum(variance_explained)
  
  variance_df <- data.frame(
    Component = paste0("PC", 1:length(variance_explained)),
    Variance_Explained = variance_explained,
    Cumulative_Variance = cumulative_variance
  )
  
  ggplot(variance_df, aes(x = Component, y = Variance_Explained)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_line(aes(y = Cumulative_Variance), group = 1, color = "red") +
    geom_point(aes(y = Cumulative_Variance), color = "red") +
    labs(title = "Variance Explained by Principal Components",
         x = "Principal Component",
         y = "Variance Explained") +
    metabocrates_theme()
}


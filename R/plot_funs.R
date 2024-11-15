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
    ggtitle("Groups counts") +
    xlab("Levels") +
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
  
  ggplot_obj <- 
    switch(type,
           "joint" = {
             attr(dat, "NA_info")[["NA_ratios_type"]] %>% 
               filter(NA_frac > 0) %>% 
               group_by(metabolite) %>% 
               summarise(NA_frac = sum(NA_frac)) %>% 
               mutate(labels = paste0(round(NA_frac * 100, 1), " %")) %>% 
               ggplot(aes(x = NA_frac, y = metabolite, label = labels))
           },
           "NA_type" = {
             attr(dat, "NA_info")[["NA_ratios_type"]] %>% 
               filter(NA_frac > 0) %>% 
               mutate(labels = paste0(round(NA_frac * 100, 1), " %")) %>% 
               ggplot(aes(x = NA_frac, y = metabolite, fill = type, label = labels))
           },
           "group" = {
             attr(dat, "NA_info")[["NA_ratios_group"]] %>% 
               filter(NA_frac > 0) %>% 
               mutate(labels = paste0(round(NA_frac * 100, 1), " %"),
                      grouping_column = as.character(grouping_column),
                      NA_frac = NA_frac/length(unique(grouping_column))) %>% 
               ggplot(aes(x = NA_frac, y = metabolite, fill = grouping_column, label = labels))
           })
  
  ggplot_obj +
    geom_col(width = 0.5) +
    geom_text(size = 2.6, position = position_stack(vjust = 0.5)) +
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

#' This function creates a density plot for a specified metabolite, overlaying a 
#' vertical dashed line indicating the Limit of Detection (LOD) cutoff. The LOD 
#' value is derived from the `LOD_table` attribute of the `dat` object.
#'
#' @param dat A `raw_data` object, the output of the [read_data()] function. The 
#' data should contain the metabolite values and LOD information.
#' @param metabolite_name A character string specifying the name of the 
#' metabolite for which the histogram should be created.
#'
#'
#' @importFrom ggplot2 ggplot geom_density geom_vline labs aes
#' @importFrom dplyr filter select mutate
#'
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_histogram_with_lod(dat, "C0")
#'
#' @export
create_density_with_lod <- function(dat, metabolite_name) {
  metabolites <- attr(dat, "metabolites")
  
  lod_info <- attr(dat, "LOD_table") %>%
    filter(type == "LOD (calc.)") %>%
    pull(metabolite_name)  %>%
    sum(na.rm = TRUE)
  
  plot_data <- attr(dat, "completed") %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(metabolite_name)) %>%
    pivot_longer(cols = all_of(metabolite_name), 
                 names_to = "Metabolite", 
                 values_to = "Value") %>%
    mutate(Value = as.numeric(Value))
  
  ggplot(plot_data, aes(x = Value)) +
    geom_density() +
    geom_vline(xintercept = lod_info, color = "red", linetype = "dashed", 
               size = 1) +
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
  
  LOD["plate bar code"] <- sapply(
    LOD["plate bar code"], 
    function(x) gsub("^.*\\s([0-9]+-[0-9]+)\\s.*$", "\\1", x)
  )
  
  grouped_data <- plot_data %>%
    group_by(`plate bar code`)
  
  
  ggplot(grouped_data, aes(x = get(metabolite1), y = get(metabolite2), 
                           color = `plate bar code`)) +
    geom_point() +
    geom_hline(data = LOD, aes(yintercept = .data[[paste0(metabolite2)]], 
                               color = `plate bar code`), linetype = "dashed") +
    geom_vline(data = LOD, aes(xintercept = .data[[paste0(metabolite1)]], 
                               color = `plate bar code`), linetype = "dashed") +
    labs(x = paste(metabolite1), y = paste(metabolite2)) +
    metabocrates_theme()
}


#' Plot of Variance Explained by Principal Components
#'
#' This function creates a barplot showing the minimum number of the greatest
#' variances explained by each principal component from a Principal Component
#' Analysis (PCA) on metabolomics data, which cumulative sum is less or equal
#' than given threshold. The plot also includes a line graph representing
#' the cumulative variance explained by the components.
#' 
#' @importFrom ggplot2 ggplot geom_bar geom_line geom_point aes labs
#'
#' @param dat A `raw_data` object, the output of the [read_data()] function. 
#' The data should be completed and filtered to include only samples of type 
#' "Sample".
#' @param threshold A value indicating the maximum cumulative variance
#' of components to display.
#' #' @param max_num An optional parameter indicating the maximum number
#' of components to display.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' pca_variance(dat, 0.8, 5)
#'
#' @export
pca_variance <- function(dat, threshold, max_num = NULL) {
  data <- attr(dat, "completed") %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(attr(dat, "metabolites")))
  
  data <- data[complete.cases(data),
               sapply(data, function(col) var(col, na.rm = TRUE) > 0)]
  
  pca_result <- prcomp(data, scale. = TRUE, center = TRUE, rank. = max_num)
  
  variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  
  cumulative_variance <- cumsum(variance_explained)
  
  variance_df <- data.frame(
    Component = paste0("PC", 1:length(variance_explained)),
    Variance_Explained = variance_explained,
    Cumulative_Variance = cumulative_variance
  )
  
  rel_comp_num <- max(which(variance_df[["Cumulative_Variance"]] <= threshold))
  
  ggplot(variance_df, aes(x = Component, y = Variance_Explained)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_line(aes(y = Cumulative_Variance), group = 1, color = "red") +
    geom_point(aes(y = Cumulative_Variance), color = "red") +
    coord_cartesian(xlim = c(1, rel_comp_num)) +
    labs(title = "Variance Explained by Principal Components",
         x = "Principal Component",
         y = "Variance Explained") +
    metabocrates_theme()
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
#' create_PCA_plot(dat)
#' dat <- add_group(dat, "group")
#' create_PCA_plot(dat, type = "group")
#' 
#' @export

create_PCA_plot <- function(dat, type = "sample_type"){
  if(type == "group" & is.null(attr(dat, "group")))
    stop("Provide a group to see the PCA plot.")
  
  if(is.null(attr(dat, "completed")))
    stop("Complete the missing values in data first.")
  
  mod_dat <- attr(dat, "completed") %>%
    drop_na(all_of(attr(dat, "metabolites")))
  
  mod_dat <- switch(type,
                    "sample_type" = rename(mod_dat,
                                           sample_type = `sample type`),
                    "group" = mod_dat %>%
                      filter(`sample type` == "Sample") %>%
                      mutate(group = as.factor(get(attr(dat, "group")))))
  
  metabo_dat <- mod_dat %>%
    mutate(across(all_of(type), ~ factor(., ordered = TRUE))) %>%
    select(all_of(attr(dat, "metabolites"))) %>%
    select(where(~ n_distinct(.) > 1))
  
  colnames(metabo_dat) <- paste0("V", 1:ncol(metabo_dat))
  
  prcomp(~., data = metabo_dat, scale. = TRUE, na.action = na.omit) %>%
    autoplot(data = mod_dat, color = type,
             frame = TRUE, frame.type = "norm") +
    scale_color_discrete(name = str_to_title(gsub("_", " ", type))) +
    scale_fill_discrete(name = str_to_title(gsub("_", " ", type))) +
    metabocrates_theme()
}

#' Beeswarm Plot of Metabolite Values
#'
#' This function creates a beeswarm plot for a specified metabolite, allowing 
#' visualization of the distribution of metabolite values across different plate 
#' bar code.
#'
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom ggplot2 ggplot aes labs
#'
#' @param dat A `raw_data` object, the output of the [read_data()] function. 
#' The data should contain metabolite values and sample information.
#' @param metabolite A character string specifying the name of the metabolite 
#' for which the beeswarm plot should be created.
#'
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' path <- get_example_data("two_sets_example.xlsx")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_beeswarm_plot(dat, "C0")
#'
#' @export

create_beeswarm_plot <- function(dat, metabolite) {
  plot_data <- attr(dat, "completed") %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(c("plate bar code", metabolite))) %>%
    group_by(`plate bar code`)
  
  ggplot(plot_data, aes(x = `plate bar code`, y = get(metabolite), 
                        color = `plate bar code`)) +
    geom_beeswarm(cex = 2) +
    labs(title = paste("Beeswarm Plot of", metabolite),
         y = metabolite) +
    metabocrates_theme()
}


#' Venn diagram for group levels
#'
#' This function creates Venn diagram, showing counts of metabolites having
#' ratios of missing values larger than the given threshold for each group level.
#' Function works only when group has up to 5 levels.
#' 
#' @importFrom ggvenn ggvenn
#' @importFrom tidyr pivot_wider
#' 
#' @param dat A grouped `raw_data` object - the output of the [read_data()]
#' function with group specified with [add_group()].
#' @param threshold A minimum ratio of metabolite missing values in one group
#' level for metabolite to be included in the diagram, given as decimal.
#' 
#' @examples#'
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- add_group(dat, "group")
#' create_venn_diagram(dat, 0.1)
#' 
#' @export

create_venn_diagram <- function(dat, threshold){
  if(is.null(attr(dat, "group"))){
    stop("No group specified.")
  }
  
  if(length(unique(dat[[attr(dat, "group")]])) > 5){
    stop("Group has more than 4 levels.")
  }
  
  NA_metabo_group <- attr(dat, "NA_info")[["NA_ratios_group"]] %>%
    filter(!(metabolite %in% attr(dat, "removed")[["LOD"]])) %>% 
    pivot_wider(names_from = "grouping_column", values_from = "NA_frac") %>%
    mutate(across(!metabolite, ~ .x >= threshold)) %>%
    select(!metabolite)
  
  venn_colors <-
    c("red", "green", "blue", "purple", "orange")[1:ncol(NA_metabo_group)]
  
  ggvenn(NA_metabo_group, show_outside = "none", stroke_color = "white",
         fill_alpha = 0.4) +
    scale_fill_manual(values = venn_colors) +
    scale_colour_manual(values = venn_colors)
}


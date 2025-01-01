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

#' Metabocrates ggplot palette
#' 
#' @keywords internal

metabocrates_palette <- function(n){
  colors <- c("#54F3D3", "#2B2A29", "#09edfd", "#DCFFDB", "#FFEA8F", "#BA7B28",
              "#C1BE3C", "#00894E", "#731CA2", "#FF7B00")
  
  colorRampPalette(colors)(n)
}

#' Metabocrates scale color
#' 
#' @keywords internal

scale_color_metabocrates_discrete <- function(n = 12, ...){
  scale_color_manual(values = metabocrates_palette(n), ...)
}

#' Metabocrates scale fill discrete
#' 
#' @keywords internal

scale_fill_metabocrates_discrete <- function(n = 12, ...){
  scale_fill_manual(values = metabocrates_palette(n), ...)
}

#' Metabocrates scale color continuous
#' 
#' @keywords internal

scale_color_metabocrates_continuous <- function(){
  scale_color_gradient2(low = "#eafffd",
                        mid = "#54f3d3",
                        high = "#2B2A29")
}

#' Metabocrates scale fill continuous
#' 
#' @keywords internal

scale_fill_metabocrates_continuous <- function(){
  scale_fill_gradient2(low = "#eafffd",
                       mid = "#54f3d3",
                       high = "#2B2A29")
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
  
  group_dat <- dat %>%
    filter(`sample type` == "Sample") %>%
    group_by(Groups = get(attr(dat, "group"))) %>%
    summarise(Count = n())
  
  ggplot(group_dat, aes(x = Groups, y = Count)) +
    geom_bar(stat = "identity", fill = "#2B2A29") +
    labs(x = "Level", y = "Count") +
    geom_label(aes(label = Count)) +
    metabocrates_theme()
}


#' Barplot of metabolomics numbers
#' 
#' @import ggplot2
#' 
#' @param dat A \code{\link{raw_data}} object. Output of [read_data()] function
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' test_dat <- read_data(path)
#' plot_mv_types(test_dat)
#' 
#' @export

plot_mv_types <- function(dat) {
  if(nrow(attr(dat, "NA_info")[["counts"]]) == 0)
    stop("No missing values found.")
    
  NA_info <- attr(dat, "NA_info")
  counts <- NA_info[["counts"]]
  
  counts %>%
    rename(count = "n") %>%
    filter(count > 0) %>%
    ggplot(aes(x = type, y = count)) +
    geom_col(fill = "#2B2A29") +
    geom_label(aes(x = type, y = count, label = count)) +
    labs(x = "Missing value type", y = "Count") +
    metabocrates_theme()
}


#' Barplot of missing values percents
#' 
#' @importFrom scales percent
#' @importFrom plotly ggplotly
#' @importFrom plotly style
#' 
#' @inheritParams plot_mv_types
#' 
#' @param type A character denoting which type of plot should be made. This 
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
  if(nrow(attr(dat, "NA_info")[["counts"]]) == 0)
    stop("No missing values found.")
  
  ggplot_obj <- 
    switch(type,
           "joint" = {
             attr(dat, "NA_info")[["NA_ratios_type"]] %>% 
               filter(NA_frac > 0) %>% 
               group_by(metabolite) %>% 
               summarise(total_NA_frac = sum(NA_frac)) %>% 
               mutate(labels = paste0(round(total_NA_frac * 100, 1), " %")) %>% 
               ggplot(aes(x = total_NA_frac,
                          y = reorder(metabolite, total_NA_frac))) +
               geom_col(width = 0.5, fill = "#2B2A29") +
               geom_text(aes(label = labels, x = total_NA_frac + 0.01), 
                         size = 3, hjust = 0, color = "black")
           },
           "NA_type" = {
             dat_NA_type <- attr(dat, "NA_info")[["NA_ratios_type"]] %>%
               filter(NA_frac > 0) %>%
               group_by(metabolite) %>%
               mutate(total_NA_frac = sum(NA_frac),
                      labels = paste0(round(total_NA_frac * 100, 1), " %")) %>%
               group_by(metabolite, type) %>%
               mutate(tooltip = paste0("Type: ", type, "<br>NA Fraction: ",
                                       round(NA_frac * 100, 1), "%"))
             
             ggplot(dat_NA_type,
                    aes(x = NA_frac, y = reorder(metabolite, total_NA_frac),
                        fill = type, text = tooltip)) +
               geom_col(width = 0.5) +
               geom_text(data = distinct(dat_NA_type, metabolite,
                                         total_NA_frac, labels, tooltip), 
                         aes(label = labels, x = total_NA_frac + 0.01), 
                         size = 3, color = "black") +
               labs(fill = "Missing values types")
           },
           "group" = {
             total_NA_dat <- attr(dat, "NA_info")[["NA_ratios_type"]] %>%
               filter(NA_frac > 0) %>% select(metabolite, NA_frac) %>%
               group_by(metabolite) %>%
               mutate(total_NA_frac = sum(NA_frac),
                      labels = paste0(round(total_NA_frac * 100, 1), " %")) %>%
               distinct(metabolite, total_NA_frac, labels)
             
             dat_group <- attr(dat, "NA_info")[["NA_ratios_group"]] %>% 
               filter(NA_frac > 0) %>%
               left_join(total_NA_dat, by = "metabolite") %>%
               mutate(tooltip = paste0("Group level: ", grouping_column,
                                       "<br>NA Fraction: ",
                                       round(NA_frac * 100, 1), " %"),
                      grouping_column = as.character(grouping_column),
                      NA_frac = NA_frac/length(unique(grouping_column)))
             
             ggplot(dat_group,
                    aes(x = NA_frac, y = reorder(metabolite, total_NA_frac),
                          fill = grouping_column, text = tooltip)) +
               geom_col(width = 0.5) +
               geom_text(data = distinct(dat_group, metabolite, total_NA_frac,
                                         labels, grouping_column, tooltip), 
                         aes(label = labels, x = total_NA_frac + 0.01), 
                         size = 3, hjust = 0, color = "black") +
               labs(fill = "Group levels")
           })
  
  plt <- ggplot_obj +
    labs(x = "% Missing in metabolite", y = "Metabolite") +
    scale_x_continuous(labels = scales::percent, expand = c(0, 0.18)) +
    scale_fill_metabocrates_discrete() +
    metabocrates_theme()
  
  if(type == "joint"){
    ggplotly(plt, tooltip = NULL) %>%
      style(textposition = "right")
  }else{
    ggplotly(plt, tooltip = "text") %>%
    style(textposition = "right", hoverinfo = "none", traces = c(4:6))
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
    mutate(Sample = 1:n()) %>%
    pivot_longer(!Sample, names_to = "Metabolite", values_to = "Value") %>%
    mutate(Metabolite = factor(Metabolite, ordered = TRUE),
           `Is missing` =
             Value %in% c("< LOD","< LLOQ", "> ULOQ", "NA", "∞", NA)) %>%
    ggplot(aes(x = Sample, y = Metabolite, fill = `Is missing`)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c(`FALSE` = "#BBBBBB", `TRUE` = "#2B2A29")) +
    metabocrates_theme()
}

#' Histograms or density plots of individual metabolite values before and after imputation
#' 
#' This function creates density plots of metabolite values before and after
#' imputation, with the sample limit of detection, or histogram of all values
#' after imputation against the histogram of only imputed ones.
#' 
#' @importFrom patchwork inset_element
#' 
#' @param metabolite A name of metabolite of interest.
#' @param bins The number of bins for the histogram plot, 30 if not specified.
#' @param type A type of the plot. Can be "histogram" (default) or "density".
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_distribution_plot(dat, "C0")
#' create_distribution_plot(dat, "C0", type = "density")
#' 
#' @export

create_distribution_plot <- function(dat, metabolite, type = "histogram", bins = 30){
  if(is.null(attr(dat, "completed")))
    stop("Complete data first.")
  
  uncomp_metabo_vals <- dat %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(metabolite)) %>%
    unlist() %>%
    as.numeric() %>%
    as.data.frame()
  
  colnames(uncomp_metabo_vals) <- metabolite
  
  comp_metabo_vals <- attr(dat, "completed") %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(metabolite))
  
  switch(type,
         "histogram" = {
           comp_metabo_vals %>%
             bind_cols("Uncompleted" = uncomp_metabo_vals[[metabolite]]) %>%
             filter(!is.numeric(Uncompleted) | is.na(Uncompleted)) %>%
             ggplot() +
             geom_histogram(aes(x = get(metabolite), y = -after_stat(count),
                                fill = "Only imputed values"), bins = bins,
                            color = "#2B2A29", alpha = 0.6) +
             geom_histogram(data = uncomp_metabo_vals,
                            aes(x = get(metabolite), y = after_stat(count),
                                fill = "Uncompleted"),
                            bins = bins, color = "#54F3D3", alpha = 0.6) +
             labs(y = "Count") +
             scale_fill_manual(name = NULL,
                               values = c("Only imputed values" = "#2B2A29",
                                     "Uncompleted" = "#54F3D3")) +
             labs(x = metabolite) +
             metabocrates_theme()
         },
         "density" = {
           vline_data <- data.frame(
             xintercept = min(na.omit(uncomp_metabo_vals)),
             linetype = "sample LOD"
           )
           
           max_comp_density <- comp_metabo_vals %>%
             na.omit() %>%
             summarise(max_density = max(density(get(metabolite))[["y"]])) %>%
             pull(max_density)
           
           plt_comp <- ggplot(comp_metabo_vals) +
             geom_density(dat = comp_metabo_vals,
                          aes(x = get(metabolite), y = after_stat(density),
                                fill = "Completed"),
                            color = "#54F3D3", alpha = 0.6) +
             geom_vline(data = vline_data,
                        aes(xintercept = xintercept, linetype = linetype),
                        color = "#676767") +
             labs(y = "Density") +
             scale_fill_manual(name = NULL,
                               values = c("Uncompleted" = "#2B2A29",
                                          "Completed" = "#54F3D3")) +
             scale_linetype_manual(name = NULL,
                                   values = c("sample LOD" = "dashed")) +
             coord_cartesian(ylim = c(-max_comp_density, max_comp_density)) +
             labs(x = metabolite) +
             metabocrates_theme()
           
           plt_uncomp <- ggplot(uncomp_metabo_vals) +
             geom_density(
               aes(x = get(metabolite), y = after_stat(-density),
                   fill = "Uncompleted"),
               color = "#2B2A29", alpha = 0.6) +
             geom_vline(data = vline_data,
                        aes(xintercept = xintercept),
                        color = "#676767", linetype = "dashed") +
             scale_fill_manual(name = NULL,
                               values = c("Uncompleted" = "#2B2A29",
                                          "Completed" = "#54F3D3")) +
             metabocrates_theme() +
             coord_cartesian(xlim = c(min(na.omit(comp_metabo_vals)),
                                      max(na.omit(comp_metabo_vals)))) +
             scale_y_continuous(expand = c(0, 0)) +
             labs(x = metabolite , y = NULL) +
             theme(plot.margin = margin(t = 0))
           
           plt_comp +
             inset_element(plt_uncomp,
                           left = 0, right = 1,
                           top = 0.5, bottom = 0)
         }
  )
}

#' Boxplots of individual metabolite values before and after imputation
#' 
#' @param metabolite A name of metabolite of interest.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_boxplot(dat, "C0")
#' 
#' @export

create_boxplot <- function(dat, metabolite){
  if(is.null(attr(dat, "completed")))
    stop("Complete data first.")
  
  uncomp_metabo_vals <- dat %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(metabolite)) %>%
    unlist() %>%
    as.numeric() %>%
    na.omit() %>%
    as.data.frame()
  
  colnames(uncomp_metabo_vals) <- metabolite
  
  comp_metabo_vals <- attr(dat, "completed") %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(metabolite))
  
  uncomp_metabo_vals %>%
    bind_rows(comp_metabo_vals) %>%
    mutate(Type = rep(c("Uncompleted", "Completed"),
                           c(nrow(uncomp_metabo_vals),
                             nrow(comp_metabo_vals)))) %>%
    ggplot(aes(x = Type, y = get(metabolite),
               fill = Type, color = Type)) +
    geom_boxplot(alpha = 0.4) +
    labs(x = metabolite, y = "Value") +
    metabocrates_theme() +
    scale_fill_metabocrates_discrete() +
    scale_color_metabocrates_discrete()
}

#' Qqplots of individual metabolite values before and after imputation
#' 
#' @param metabolite A name of metabolite of interest.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_qqplot(dat, "C0")
#' 
#' @export

create_qqplot <- function(dat, metabolite){
  if(is.null(attr(dat, "completed")))
    stop("Complete data first.")
  
  uncomp_metabo_vals <- dat %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(metabolite)) %>%
    unlist() %>%
    as.numeric() %>%
    na.omit() %>%
    as.data.frame()
  
  colnames(uncomp_metabo_vals) <- metabolite
  
  comp_metabo_vals <- attr(dat, "completed") %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(metabolite))
  
  uncomp_metabo_vals %>%
    bind_rows(comp_metabo_vals) %>%
    mutate(Type = rep(c("Uncompleted", "Completed"),
                           c(nrow(uncomp_metabo_vals),
                             nrow(comp_metabo_vals)))) %>%
    ggplot(aes(sample = get(metabolite), color = Type)) +
    geom_qq() +
    geom_qq_line() +
    labs(x = "Normal quantiles", y =  paste0(metabolite, " quantiles")) +
    facet_wrap(~ Type) +
    metabocrates_theme() +
    scale_color_metabocrates_discrete()
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
#' create_correlations_heatmap(dat)
#' 
#' @export

create_correlations_heatmap <- function(dat){
  if(is.null(attr(dat, "completed")))
    stop("Complete data first.")
  
  attr(dat, "completed") %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(attr(dat, "metabolites"))) %>%
    select(where(~ !is.na(sd(., na.rm = TRUE)) & sd(., na.rm = TRUE) != 0)) %>%
    cor(use = "na.or.complete") %>%
    melt() %>%
    ggplot(aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_x_discrete(labels = function(x) str_trunc(x, 10)) +
    scale_y_discrete(labels = function(x) str_trunc(x, 10)) +
    labs(x = "Metabolite", y = "Metabolite") +
    metabocrates_theme() +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_metabocrates_continuous()
}

#' This function creates a density plot for a specified metabolite, overlaying a 
#' vertical dashed line indicating the Limit of Detection (LOD) cutoff. The LOD 
#' value is derived from the `LOD_table` attribute of the `dat` object.
#' 
#' @importFrom ggplot2 ggplot geom_density geom_vline labs aes
#' @importFrom dplyr filter select mutate
#'
#'
#' @param dat A `raw_data` object, the output of the [read_data()] function. The 
#' data should contain the metabolite values and LOD information.
#' @param metabolite_name A character string specifying the name of the 
#' metabolite for which the histogram should be created.
#'
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_density_with_lod(dat, "C0")
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
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_plot_of_2_metabolites(dat, "C0", "C2")
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
    metabocrates_theme() +
    scale_color_metabocrates_discrete()
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
  ) %>%
    filter(Component %in% Component[1:(max(which(Cumulative_Variance <= threshold))+1)])
  
  ggplot(variance_df, aes(x = Component, y = Variance_Explained)) +
    geom_bar(stat = "identity", fill = "#2B2A29") +
    geom_line(aes(y = Cumulative_Variance), group = 1, color = "#54F3D3") +
    geom_point(aes(y = Cumulative_Variance), color = "#54F3D3") +
    labs(x = "Principal component", y = "% Variance explained") +
    scale_y_continuous(labels = scales::percent) +
    metabocrates_theme()
}

#' PCA plot
#' 
#' @import ggfortify
#' @importFrom tidyr drop_na
#' 
#' @param type A character denoting which type of PCA plot should be created.
#' Default is "sample_type", which makes a plot for quality control. Type
#' "group" creates a PCA plot with respect to the groups of samples with type
#' 'Sample'. Type "biplot" adds eigenvectors to the PCA for quality control.
#' @param threshold A value indicating the minimum correlation between
#' a variable and any component, required for this  variable to be included
#' on the PCA biplot.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_PCA_plot(dat)
#' create_PCA_plot(dat, type = "biplot", 0)
#' dat <- add_group(dat, "group")
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_PCA_plot(dat, type = "group")
#' 
#' @export

create_PCA_plot <- function(dat, type = "sample_type", threshold = NULL){
  if(type == "group" & is.null(attr(dat, "group")))
    stop("Provide a group to see the PCA plot.")
  
  if(type == "biplot" & is.null(threshold))
    stop("Provide a threshold.")
  
  if(is.null(attr(dat, "completed")))
    stop("Complete the missing values in data first.")
  
  mod_dat <- attr(dat, "completed") %>%
    drop_na(all_of(attr(dat, "metabolites")))
  
  if(type == "group"){
    mod_dat <- mod_dat %>%
      filter(`sample type` == "Sample") %>%
      mutate(group = as.factor(get(attr(dat, "group"))))
  }else{
    mod_dat <- rename(mod_dat, sample_type = `sample type`)
  }
  
  col_type <- ifelse(type == "biplot", "sample_type", type)
  
  metabo_dat <- mod_dat %>%
    mutate(across(all_of(col_type), ~ factor(., ordered = TRUE))) %>%
    select(all_of(attr(dat, "metabolites"))) %>%
    select(where(~ n_distinct(.) > 1))
  
  if(ncol(metabo_dat) == 0)
    stop("No samples without missing values found.")
  
  pca_metabolites <- colnames(metabo_dat)
  
  colnames(metabo_dat) <- paste0("V", 1:ncol(metabo_dat))
  
  pca_colors <- c("#54F3D3", "#2B2A29", "#F39C12", "#E74C3C", "#8E44AD",
                  "#2980B9", "#27AE60", "#D35400")
  
  
  pca_res <- prcomp(~., data = metabo_dat, scale. = TRUE, na.action = na.omit)
  
  pca_plot <- pca_res %>%
    autoplot(data = mod_dat, color = col_type,
             frame = TRUE, frame.type = "norm", loadings = TRUE,
             loadings.label = TRUE) +
    scale_color_manual(values = pca_colors,
      name = ifelse(type == "sample_type", "Sample types", "Group levels")
      ) +
    scale_fill_manual(values = pca_colors,
      name = ifelse(type == "sample_type", "Sample types", "Group levels")
      ) +
    metabocrates_theme()
  
  if(type == "biplot"){
    loadings <- as.data.frame(pca_res[["rotation"]]) %>%
      select(PC1, PC2) %>%
      mutate(Variable = pca_metabolites,
             `Variance explained` = pca_res[["sdev"]]) %>%
      filter(if_any(PC1:PC2, ~ abs(.) >= threshold))
#    %>%
#      mutate(across(PC1:PC2, ~ . * `Variance explained`))
    
    pca_plot +
      geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
                   arrow = arrow(length = unit(0.1, "cm")),
                   colour = "black", inherit.aes = FALSE) +
      geom_text(data = loadings,
                aes(x = PC1 * 1.1, y = PC2 * 1.1, label = Variable),
                colour = "black", size = 4, inherit.aes = FALSE)
  }
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
         x = "Plate bar code",
         y = metabolite) +
    metabocrates_theme() +
    scale_color_metabocrates_discrete()
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
  
  ggvenn(NA_metabo_group, show_outside = "none", stroke_color = "white",
         fill_alpha = 0.6) +
    scale_fill_metabocrates_discrete() +
    scale_color_metabocrates_discrete()
}

#' MetaboCrates ggplot theme
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

#' MetaboCrates ggplot palette
#' 
#' @keywords internal

metabocrates_palette <- function(n){
  colors <- c("#54F3D3", "#2B2A29", "#09EDFD", "#DCFFDB", "#FFEA8F", "#BA7B28",
              "#C1BE3C", "#00894E", "#731CA2", "#FF7B00")
  
  if(n <= 10) colors[1:n]
  else colorRampPalette(colors)(n)
}

#' MetaboCrates scale color
#' 
#' @keywords internal

scale_color_metabocrates_discrete <- function(n = 12, ...){
  scale_color_manual(values = metabocrates_palette(n), ...)
}

#' MetaboCrates scale fill discrete
#' 
#' @keywords internal

scale_fill_metabocrates_discrete <- function(n = 12, ...){
  scale_fill_manual(values = metabocrates_palette(n), ...)
}

#' MetaboCrates scale color continuous
#' 
#' @keywords internal

scale_color_metabocrates_continuous <- function(){
  scale_color_gradient2(low = "white",
                        mid = "#54f3d3",
                        high = "#2B2A29")
}

#' MetaboCrates scale fill continuous
#' 
#' @keywords internal

scale_fill_metabocrates_continuous <- function(){
  scale_fill_gradientn(
    colours = c("#685009", "#c0a948", "white", "#54f3d3", "#2B2A29"),
    limits = c(-1,1)
  )
}

#' Barplot of levels sizes
#' 
#' @import ggplot2
#' 
#' @param dat a \code{\link{raw_data}} object. Output of [read_data()] function.
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
    rowwise() %>%
    mutate(group_tmp = do.call(paste,
                               c(across(all_of(attr(dat, "group"))),
                                 sep = ",\n"))) %>%
    group_by(group_tmp) %>%
    summarise(Count = n()) %>%
    ggplot(aes(x = Count, y = reorder(group_tmp, Count))) +
    geom_bar(stat = "identity", fill = "#2B2A29") +
    labs(x = "Count", y = paste0(attr(dat, "group"), collapse = ", ")) +
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
    
  counts <- attr(dat, "NA_info")[["counts"]] %>%
    mutate(type = factor(type, levels = type))
  
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
#' @importFrom ggiraph girafe geom_col_interactive opts_tooltip opts_toolbar opts_sizing opts_zoom
#' 
#' @inheritParams plot_mv_types
#' 
#' @param type A character denoting which type of plot should be made. This 
#' function accepts either "joint", "NA_type" or "group".
#' Default type is "joint", which creates plot of missing values percents
#' in each metabolite. Types "NA_type" and "group" add the division into
#' all missing values types and levels in grouping column respectively.
#' @param interactive If TRUE, the plot includes interactive tooltips.
#' 
#' @examples 
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' print(plot_NA_percent(dat))
#' print(plot_NA_percent(dat, "NA_type"))
#' dat <- add_group(dat, "group")
#' print(plot_NA_percent(dat, "group"))
#' 
#' @export

plot_NA_percent <- function(dat, type = "joint", interactive = TRUE){
  if(nrow(attr(dat, "NA_info")[["counts"]]) == 0)
    stop("No missing values found.")
  
  filtered_dat <- attr(dat, "NA_info")[["NA_ratios_type"]] %>%
    filter(NA_frac > 0, !(metabolite %in% c(unlist(attr(dat, "removed")))))
  
  ggplot_obj <- 
    switch(type,
           "joint" = {
             filtered_dat %>% 
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
             dat_NA_type <- filtered_dat %>%
               group_by(metabolite) %>%
               mutate(total_NA_frac = sum(NA_frac),
                      labels = paste0(round(total_NA_frac * 100, 1), " %")) %>%
               group_by(metabolite, type) %>%
               mutate(tooltip = paste0("Type: ", type, "<br>NA Fraction: ",
                                       round(NA_frac * 100, 1), "%"))
             
             ggplot(dat_NA_type,
                    aes(x = NA_frac, y = reorder(metabolite, total_NA_frac),
                        fill = type, tooltip = tooltip)) +
               geom_col_interactive(width = 0.5) +
               geom_text(aes(label = labels, x = total_NA_frac + 0.01),
                         hjust = 0, size = 3, color = "black") +
               labs(fill = "Missing values types")
           },
           "group" = {
             total_NA_dat <- filtered_dat %>%
               select(metabolite, NA_frac) %>%
               group_by(metabolite) %>%
               mutate(total_NA_frac = sum(NA_frac),
                      labels = paste0(round(total_NA_frac * 100, 1), " %")) %>%
               distinct(metabolite, total_NA_frac, labels)
             
             dat_group <- attr(dat, "NA_info")[["NA_ratios_group"]] %>% 
               filter(NA_frac > 0) %>%
               filter(NA_frac > 0,
                      !(metabolite %in% c(unlist(attr(dat, "removed"))))) %>%
               left_join(total_NA_dat, by = "metabolite") %>%
               mutate(tooltip = paste0("Group level: ", grouping_column,
                                       "<br>NA Fraction: ",
                                       round(NA_frac * 100, 1), " %"),
                      NA_frac = NA_frac/length(unique(grouping_column)))
             
             ggplot(dat_group,
                    aes(x = NA_frac, y = reorder(metabolite, total_NA_frac),
                          fill = grouping_column, tooltip = tooltip)) +
               geom_col_interactive(width = 0.5) +
               geom_text(aes(label = labels, x = total_NA_frac + 0.01), 
                         size = 3, hjust = 0, color = "black") +
               labs(fill = "Group levels")
           })
  
  plt <- ggplot_obj +
    labs(x = "% Missing in metabolite", y = "Metabolite") +
    scale_x_continuous(labels = scales::percent, expand = c(0, 0.18)) +
    scale_fill_metabocrates_discrete() +
    metabocrates_theme()
  
  if(interactive)
    girafe(
      ggobj = plt,
      height_svg = 4 + length(unique(filtered_dat[["metabolite"]]))/20,
      options = list(
        opts_tooltip(css = "background-color:black;color:white;padding:10px;border-radius:10px;font-family:Arial;font-size:11px;",
                     opacity = 0.9),
        opts_toolbar(saveaspng = FALSE),
        opts_zoom(min = 0.5, max = 5)
      )
    )
  else plt
}

#' Heatmap of missing metabolites values
#' 
#' @param dat A `raw_data` object, the output of the [read_data()].
#' @param plate_bar_code A single plate bar code for selecting observations to
#' include in the plot. If NULL (default), a grid of plots is returned, where
#' each plot corresponds to a different plate bar code.
#' @param include_title Logical. Indicates whether the title with the
#' plate bar code should be included (only if `plate_bar_code` is not NULL).
#' Defaults to `FALSE`.
#' @param show_colors Logical. If `TRUE`, applies distinct colors to different
#' types of missing values, following the conventions used in Biocrates® files.
#'
#' @examples 
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' plot_heatmap(dat, "1036372116-1 | 1036372121-1")
#' 
#' @export

plot_heatmap <- function(dat, plate_bar_code = NULL, include_title = FALSE,
                         show_colors = TRUE){
  if(is.null(plate_bar_code))
    plt_dat <- dat %>%
      filter(`sample type` == "Sample") %>%
      select(all_of(c(setdiff(attr(dat, "metabolites"),
                              unlist(attr(dat, "removed"))),
                      "plate bar code"))) %>%
      mutate(Sample = 1:n()) %>%
      pivot_longer(!c(Sample, `plate bar code`),
                   names_to = "Metabolite", values_to = "Value")
  else
    plt_dat <- dat %>%
      filter(`sample type` == "Sample") %>%
      filter(`plate bar code` == plate_bar_code) %>%
      select(all_of(setdiff(attr(dat, "metabolites"),
                            unlist(attr(dat, "removed"))))) %>%
      mutate(Sample = 1:n()) %>%
      pivot_longer(!Sample,
                   names_to = "Metabolite", values_to = "Value")
    
  plt_dat <- plt_dat %>%
    rowwise() %>%
    mutate(Metabolite = factor(Metabolite, levels = unique(Metabolite)),
           Missing = case_when(
             Value %in% c("< LOD", "< LLOQ", "> ULOQ") & show_colors ~ Value,
             Value %in% c("< LOD", "< LLOQ", "> ULOQ") & !show_colors ~ "True",
             Value %in% c("NA", "∞") | is.na(Value) ~ "True",
             TRUE ~ "False"
           ))
  
  plt <- plt_dat %>%
    ggplot(aes(x = Sample, y = Metabolite, fill = Missing)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("False" = "#BBBBBB", "True" = "#2B2A29",
                                 "< LOD" = "#A28CA1", "< LLOQ" = "#B2D1DE",
                                 "> ULOQ"  = "#7FB1C5")) +
    scale_y_discrete(limits = rev) +
    metabocrates_theme() +
    theme(legend.justification.right = "top")
  
  if(is.null(plate_bar_code))
    plt +
      facet_wrap(~ `plate bar code`, ncol = 1, scales = "free_x")
  else{
    if(include_title)
      plt +
      labs(title = paste0("Plate bar code: ", plate_bar_code))
    else plt
  }
}

#' Histogram of individual metabolite values before and after imputation
#' 
#' @keywords internal

create_histogram <- function(uncomp_metabo_vals, comp_metabo_vals, metabolite,
                             bins, type){
  if(type == "imputed"){
    comp_metabo_vals <- comp_metabo_vals %>%
      bind_cols("Observed" = uncomp_metabo_vals[[metabolite]]) %>%
      filter(!is.numeric(Observed) | is.na(Observed))
    
    fill_name <- "Only imputed values"
  }else
    fill_name <- "Completed"
  
  if(all(is.na(uncomp_metabo_vals[[metabolite]])))
    fill_vals <- c("Only imputed values" = "#54F3D3")
  
  else if(all(is.na(comp_metabo_vals[[metabolite]])))
    fill_vals <- c("Observed" = "#2B2A29")
  
  else
    fill_vals <- c("Only imputed values" = "#54F3D3",
                   "Observed" = "#2B2A29")
  
  if(type == "all" && !all(is.na(comp_metabo_vals[[metabolite]])))
    names(fill_vals)[1] <- "Completed"
  
   comp_metabo_vals %>%
    ggplot() +
    geom_histogram(aes(x = get(metabolite), y = after_stat(count),
                       fill = fill_name), bins = bins,
                   color = "#54F3D3", alpha = 0.6) +
    geom_histogram(data = uncomp_metabo_vals,
                   aes(x = get(metabolite), y = -after_stat(count),
                       fill = "Observed"),
                   bins = bins, color = "#2B2A29", alpha = 0.6) +
    labs(y = "Count") +
    scale_fill_manual(name = NULL,
                      values = fill_vals) +
    labs(x = metabolite) +
    metabocrates_theme()
}

#' Density of individual metabolite values before and after imputation
#' 
#' @keywords internal

create_density <- function(uncomp_metabo_vals, comp_metabo_vals, metabolite){
  vline_data <- data.frame(
    xintercept = min(na.omit(uncomp_metabo_vals[[metabolite]])),
    linetype = "sample LOD"
  )
  
  plt_comp <- ggplot(comp_metabo_vals) +
    geom_density(aes(x = get(metabolite), y = after_stat(density),
                     fill = "Completed"), color = "#54F3D3", alpha = 0.6) +
    scale_fill_manual(name = NULL,
                      values = c("Completed" = "#54F3D3")) +
    labs(x = metabolite, y = "Density") +
    metabocrates_theme()
  
  plt_uncomp <- ggplot(uncomp_metabo_vals) +
    geom_density(
      aes(x = get(metabolite), y = after_stat(-density),
          fill = "Observed"),
      color = "#2B2A29", alpha = 0.6) +
    geom_vline(data = vline_data,
               aes(xintercept = xintercept),
               color = "red", linetype = "dashed") +
    scale_fill_manual(name = NULL,
                      values = c("Observed" = "#2B2A29")) +
    labs(x = metabolite , y = "Density") +
    metabocrates_theme()
  
  if(!is.infinite(vline_data[["xintercept"]])){
    plt_comp <- plt_comp +
      geom_vline(data = vline_data,
                 aes(xintercept = xintercept, linetype = linetype),
                 color = "red") +
      scale_linetype_manual(name = NULL,
                            values = c("sample LOD" = "dashed"))
    
    plt_uncomp <- plt_uncomp +
      geom_vline(data = vline_data,
                 aes(xintercept = xintercept, linetype = linetype),
                 color = "red") +
      scale_linetype_manual(name = NULL,
                            values = c("sample LOD" = "dashed"))
  }
  
  if(all(is.na(uncomp_metabo_vals[[metabolite]])))
    plt_comp
  
  else if(all(is.na(comp_metabo_vals[[metabolite]])))
    plt_uncomp
  
  else{
    max_uncomp_density <- uncomp_metabo_vals %>%
      na.omit() %>%
      summarise(max_density = max(density(get(metabolite))[["y"]])) %>%
      pull(max_density)
    
    max_comp_density <- comp_metabo_vals %>%
      na.omit() %>%
      summarise(max_density = max(density(get(metabolite))[["y"]])) %>%
      pull(max_density)
    
    plt_comp <- plt_comp +
      coord_cartesian(ylim = c(0, max_comp_density*1.05),
                      expand = FALSE) +
      theme(plot.margin=margin(b=-1,unit="cm"))
    
    plt_uncomp <- plt_uncomp +
      coord_cartesian(xlim =
                        c(min(na.omit(comp_metabo_vals[[metabolite]])),
                          max(na.omit(comp_metabo_vals[[metabolite]]))),
                      ylim = c(max_uncomp_density*-1.05, 0),
                      expand = FALSE) +
      theme(plot.margin=margin(t=-0.001,unit="cm"))
    
    custom_layout <- c(area(1, 1), area(2, 1))
    
    (plt_comp + plt_uncomp) + plot_layout(design = custom_layout,
                                          axes = "collect",
                                          axis_titles ="collect",
                                          guides = "collect")
  }
}

#' Beeswarm of individual metabolite values before and after imputation
#' 
#' @keywords internal

create_beeswarm <- function(uncomp_metabo_vals, comp_metabo_vals, metabolite){
  plt_dat <- uncomp_metabo_vals %>%
    bind_rows(comp_metabo_vals) %>%
    mutate(Type = c(rep("Observed", nrow(uncomp_metabo_vals)),
                    rep("Completed", nrow(comp_metabo_vals))),
           tooltip = paste0("Sample id: ", `sample identification`,
                            "<br>Value: ", get(metabolite)))
  
  plt <- ggplot(plt_dat, aes(x = Type, y = get(metabolite),
                             tooltip = tooltip, color = Type)) +
    labs(x = "", y = metabolite) +
    metabocrates_theme() +
    scale_color_metabocrates_discrete(2)
}

#' Histograms or density plots of individual metabolite values before and after imputation
#' 
#' This function creates density plots of metabolite values before and after
#' imputation, with the sample limit of detection, or histogram of all values
#' after imputation against the histogram of only imputed ones.
#' 
#' @importFrom patchwork area plot_layout
#' @importFrom ggbeeswarm position_quasirandom geom_quasirandom
#' 
#' @param metabolite A name of metabolite of interest.
#' @param type A type of the plot. Can be "histogram" (default), "density"
#' or "beeswarm".
#' @param bins The number of bins for the histogram plot, 30 if not specified.
#' @param histogram_type If `all` (default), the histogram displays all values
#' after imputation. If `imputed`, it shows only the values that were imputed.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' print(create_distribution_plot(dat, "C3"))
#' print(create_distribution_plot(dat, "C3", type = "density"))
#' print(create_distribution_plot(dat, "C3", type = "beeswarm"))
#' 
#' @export

create_distribution_plot <- function(dat, metabolite, type = "histogram",
                                     bins = 30, histogram_type = "all",
                                     interactive = TRUE){
  if(is.null(attr(dat, "completed")))
    stop("Complete data first.")
  
  uncomp_metabo_vals <- dat %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(c(metabolite, "sample identification"))) %>%
    mutate(across(all_of(metabolite), ~ as.numeric(.)))
  
  comp_metabo_vals <- attr(dat, "completed") %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(c(metabolite, "sample identification")))
  
  if(all(is.na(uncomp_metabo_vals)) & all(is.na(comp_metabo_vals))) stop()
  
  plt <- switch(type,
         "histogram" = create_histogram(uncomp_metabo_vals, comp_metabo_vals,
                                        metabolite, bins, histogram_type),
         "density" = create_density(uncomp_metabo_vals, comp_metabo_vals,
                                    metabolite),
         "beeswarm" = create_beeswarm(uncomp_metabo_vals, comp_metabo_vals,
                                      metabolite)
  )
  
  if(type == "beeswarm"){
    if(interactive){
      int_plt <- plt +
        geom_point_interactive(position = position_quasirandom(),
                               show.legend = FALSE)
      
      girafe(ggobj = int_plt,
             options = list(
               opts_tooltip(css = "background-color:black;color:white;padding:10px;border-radius:10px;font-family:Arial;font-size:11px;",
                            opacity = 0.9),
               opts_toolbar(saveaspng = FALSE),
               opts_zoom(min = 0.5, max = 5)
             ))
    }else
      plt +
        geom_quasirandom(show.legend = FALSE)
  }else if(interactive)
    girafe(ggobj = plt,
           options = list(
             opts_toolbar(saveaspng = FALSE),
             opts_zoom(min = 0.5, max = 5)
           ))
  else plt
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

create_boxplot <- function(dat, metabolite, interactive = TRUE){
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
  
  plt <- uncomp_metabo_vals %>%
    bind_rows(comp_metabo_vals) %>%
    mutate(Type = rep(c("Observed", "Completed"),
                           c(nrow(uncomp_metabo_vals),
                             nrow(comp_metabo_vals)))) %>%
    ggplot(aes(x = Type, y = get(metabolite),
               fill = Type, color = Type)) +
    geom_boxplot(alpha = 0.4, outlier.alpha = 1) +
    labs(x = metabolite, y = "Value") +
    metabocrates_theme() +
    scale_fill_metabocrates_discrete() +
    scale_color_metabocrates_discrete()
  
  if(interactive)
    girafe(ggobj = plt,
           options = list(
             opts_toolbar(saveaspng = FALSE),
             opts_zoom(min = 0.5, max = 5)
           )
    )
  else plt
    
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

create_qqplot <- function(dat, metabolite, interactive = TRUE){
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
  
  plt <- uncomp_metabo_vals %>%
    bind_rows(comp_metabo_vals) %>%
    mutate(Type = rep(c("Observed", "Completed"),
                           c(nrow(uncomp_metabo_vals),
                             nrow(comp_metabo_vals)))) %>%
    ggplot(aes(sample = get(metabolite), color = Type)) +
    geom_qq() +
    geom_qq_line() +
    labs(x = "Normal quantiles", y =  paste0(metabolite, " quantiles")) +
    facet_wrap(~ Type) +
    metabocrates_theme() +
    scale_color_metabocrates_discrete()
  
  if(interactive)
    girafe(ggobj = plt,
           options = list(
             opts_toolbar(saveaspng = FALSE),
             opts_zoom(min = 0.5, max = 5)
           )
    )
  else plt
}

#' Heatmap of correlations between metabolites
#' 
#' @importFrom stringr str_trunc
#' @importFrom reshape2 melt
#' @importFrom ggiraph geom_tile_interactive
#' 
#' @param type Default to `completed`, which creates a heatmap of correlations
#' between the metabolites after imputation. If `both`, then correlations
#' between the metabolites before and after imputation are shown.
#' @param threshold A numeric value specifying the minimum absolute correlation
#' to display (only metabolites specified in metabolites_to_display are taken
#' into account).
#' @param metabolites_to_display A vector of names or number of metabolites to
#' display. If a number is provided, the first metabolites are selected.
#' Defaults to `all`.
#' @param interactive If TRUE, the plot includes interactive tooltips.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' print(create_correlations_heatmap(dat))
#' print(create_correlations_heatmap(dat, type = "both"))
#' 
#' @export

create_correlations_heatmap <- function(dat, type = "completed",
                                        threshold = 0.3,
                                        metabolites_to_display = "all",
                                        interactive = TRUE){
  if(is.null(attr(dat, "completed")))
    stop("Complete data first.")
  
  filter_data <- function(dat){
    dat %>%
      filter(`sample type` == "Sample") %>%
      select(all_of(setdiff(attr(dat, "metabolites"),
                            unlist(attr(dat, "removed"))))) %>%
      mutate(across(everything(), as.numeric)) %>%
      select(where(~ !is.na(sd(., na.rm = TRUE)) & sd(., na.rm = TRUE) != 0))
  }
  
  filtered_dat <- attr(dat, "completed") %>%
    filter_data()
  
  if(type == "both"){
    filtered_dat_observed <- dat %>%
      filter_data()
    
    metabo_to_disp_filtered <- intersect(colnames(filtered_dat),
                                         colnames(filtered_dat_observed))
  }else{
    filtered_dat_observed <- NULL
    metabo_to_disp_filtered <- colnames(filtered_dat)
  }

  if(length(metabolites_to_display) == 1 && is.numeric(metabolites_to_display))
    metabo_to_disp_filtered <-
      metabo_to_disp_filtered[1:min(metabolites_to_display,
                                    length(metabo_to_disp_filtered))]
  else if(all(is.character(metabolites_to_display)) & all(metabolites_to_display != "all"))
    metabo_to_disp_filtered <- intersect(metabo_to_disp_filtered,
                                         metabolites_to_display)
  else if(is.null(metabolites_to_display))
    return(NULL)
  
  if(type == "both")
    filtered_dat_observed <- filtered_dat_observed %>%
      select(all_of(metabo_to_disp_filtered))
  
  plt_dat <- filtered_dat %>%
    select(all_of(metabo_to_disp_filtered)) %>%
    cor(y = filtered_dat_observed, use = "pairwise.complete.obs") %>%
    melt()
  
  if(type == "completed")
    to_display <- plt_dat %>%
    group_by(Var1) %>%
    filter(Var1 != Var2) %>%
    filter(abs(value) >= threshold) %>%
    select(Var1) %>%
    unlist() %>%
    as.vector() %>%
    unique()
  else
    to_display <- plt_dat %>%
    group_by(Var1, Var2) %>%
    filter(Var1 != Var2) %>%
    filter(abs(value) >= threshold) %>%
    select(Var1, Var2) %>%
    unlist() %>%
    as.vector() %>%
    unique()
  
  if(length(to_display) == 0) return(NULL)
  
  plt <- plt_dat %>%
    rowwise() %>%
    filter(all(c(Var1, Var2) %in% to_display)) %>%
    mutate(tooltip = paste0(ifelse(type == "both",
                                   "Metabolite after imputation: ",
                                   "Metabolite 1: "),
                            Var1,
                            ifelse(type == "both",
                                   "<br>Metabolite before imputation: ",
                                   "<br>Metabolite 2: "),
                            Var2,
                            "<br>Correlation coefficient: ", round(value, 3))) %>%
    ggplot(aes(x = Var1, y = Var2, fill = value, tooltip = tooltip)) +
    geom_tile_interactive() +
    scale_x_discrete(labels = function(x) str_trunc(x, 10)) +
    scale_y_discrete(limits = rev, labels = function(x) str_trunc(x, 10)) +
    labs(x = ifelse(type == "both",
                    "Metabolites after imputation",
                    "Metabolites"),
         y = ifelse(type == "both",
                    "Metabolites before imputation",
                    "Metabolites")) +
    metabocrates_theme() +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_metabocrates_continuous()
  
  if(interactive)
    girafe(ggobj = plt,
         options = list(
           opts_tooltip(css = "background-color:black;color:white;padding:10px;border-radius:10px;font-family:Arial;font-size:11px;",
                        opacity = 0.9),
           opts_toolbar(saveaspng = FALSE),
           opts_zoom(min = 0.5, max = 5)
         ))
  else plt
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
#' @importFrom stringr str_extract
#' @importFrom ggiraph geom_point_interactive
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' print(create_plot_of_2_metabolites(dat, "C0", "C2"))
#' 
#' @export
create_plot_of_2_metabolites <- function(dat, metabolite1, metabolite2,
                                         interactive = TRUE){
  p_b_codes <- dat %>%
    filter(`sample type` == "Sample") %>%
    select(`plate bar code`) %>%
    unique() %>%
    unlist()
  
  LOD <- attr(dat, "LOD_table") %>%
    filter(type == "LOD (calc.)") %>%
    select(all_of(c("plate bar code", metabolite1, metabolite2))) %>%
    filter(if_any(everything(), ~ . != 0)) %>%
    rowwise() %>%
    mutate(`plate bar code` = {
      pbc <- gsub("^.*\\s([0-9]+-[0-9]+)\\s.*$", "\\1",
                  gsub("/", "-", `plate bar code`))
      p_b_codes[grep(paste0("\\b", pbc, "\\b"), p_b_codes)]
    }) %>%
    group_by(`plate bar code`) %>%
    summarise(across(everything(), ~ na.omit(.)[which(na.omit(.) != 0)]))
  
  plot_data <- attr(dat, "completed") %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(c("sample identification", "plate bar code",
                    metabolite1, metabolite2))) %>%
    mutate(tooltip = paste0("Sample id: ", `sample identification`,
                            "<br>", metabolite1, ": ", get(metabolite1),
                            "<br>", metabolite2, ": ", get(metabolite2)))
  
  palette <- c("#54F3D3", "#2B2A29", "#09edfd", "#DCFFDB", "#FFEA8F", "#BA7B28",
               "#C1BE3C", "#00894E", "#731CA2", "#FF7B00")
  
  values <- setNames(
    palette[1:length(unique(plot_data[["plate bar code"]]))],
    unique(plot_data[["plate bar code"]])
  )
  
  plt <- ggplot(plot_data, aes(x = get(metabolite1), y = get(metabolite2),
                           color = `plate bar code`, tooltip = tooltip)) +
    geom_point_interactive() +
    geom_hline(data = LOD, aes(yintercept = get(metabolite2), 
                               color = `plate bar code`), linetype = "dashed") +
    geom_vline(data = LOD, aes(xintercept = get(metabolite1), 
                               color = `plate bar code`), linetype = "dashed") +
    labs(x = paste(metabolite1), y = paste(metabolite2)) +
    metabocrates_theme() +
    scale_color_manual(values = values)
  
  if(interactive)
    girafe(ggobj = plt,
           options = list(
             opts_tooltip(css = "background-color:black;color:white;padding:10px;border-radius:10px;font-family:Arial;font-size:11px;",
                          opacity = 0.9),
             opts_toolbar(saveaspng = FALSE),
             opts_zoom(min = 0.5, max = 5)
           ))
  else plt
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
#' @param type A character specifying which rows to consider. Default is
#' "sample_type" and all rows are used. When "group", only observations
#' with type sample are considered.
#' @param max_num An optional parameter indicating the maximum number
#' of components to display.
#' @param cumulative A logical value indicating if the cumulative variance
#' should be showed on the plot, defaults to TRUE.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' pca_variance(dat, 0.8, max_num = 5)
#'
#' @export
pca_variance <- function(dat, threshold, type = "sample_type",
                         max_num = NULL, cumulative = TRUE) {
  if(type == "group" && is.null(attr(dat, "group")))
    warning("No group specified.")
  
  filt_dat <- attr(dat, "completed")
  
  if(type == "group")
    filt_dat <- filt_dat %>%
      filter(`sample type` == "Sample")
  
  filt_dat <- filt_dat %>%  
    select(all_of(setdiff(attr(dat, "metabolites"),
                          unlist(attr(dat, "removed"))))) %>%
    select(where(~ n_distinct(na.omit(.)) > 1)) %>%
    na.omit() %>%
    select(where(~ var(.) > 0))
  
  pca_result <- prcomp(filt_dat, scale. = TRUE, center = TRUE)
  
  variance_explained <- pca_result[["sdev"]]^2 / sum(pca_result[["sdev"]]^2)
  
  cumulative_variance <- cumsum(variance_explained)
  
  variance_df <- data.frame(
    Component = factor(paste0("PC", 1:length(variance_explained)),
                       levels = paste0("PC", 1:length(variance_explained))),
    Variance_Explained = variance_explained,
    Cumulative_Variance = cumulative_variance
  )
  
  if(max(variance_df[["Cumulative_Variance"]]) > threshold)
    variance_df <- filter(
      variance_df,
      Component %in% Component[1:(min(which(Cumulative_Variance > threshold)))]
    )
  
  if(!is.null(max_num)){
    variance_df <- variance_df %>%
      filter(row_number() <= max_num)
  }
  
  plt <- ggplot(variance_df, aes(x = Component, y = Variance_Explained)) +
    geom_bar(stat = "identity", fill = "#2B2A29") +
    labs(x = "Principal component", y = "% Variance explained") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(
      labels = paste0(variance_df[["Component"]],
                      "(",
                      round(variance_df[["Variance_Explained"]], 2)*100,
                      "%)")
    ) +
    metabocrates_theme()
  
  if(cumulative){
    plt +
      geom_line(aes(y = Cumulative_Variance, color = "cumulative variance"),
                group = 1) +
      geom_point(aes(y = Cumulative_Variance, color = "cumulative variance")) +
      geom_hline(aes(yintercept = threshold, color = "threshold"),
                 linetype = "dashed") +
      scale_color_manual(name = NULL,
                         values = c("cumulative variance" = "#54F3D3",
                                    "threshold" = "red")) 
  }else
    plt
    
}

#' PCA plot
#' 
#' @import ggfortify
#' @importFrom tidyr drop_na
#' @importFrom ggiraph geom_segment_interactive
#' 
#' @param type A character denoting which type of PCA plot should be created.
#' Default to "scatter". Type "biplot" shows eigenvectors.
#' @param group_by Character; default to "sample_type", which makes a plot for
#' quality control. When set as "group", a PCA plot with respect to the groups
#' of samples with type 'Sample' is created.
#' @param types_to_display A vector of sample type names specifying which types
#' should be shown on the plot when type = "sample_type". Defaults to all.
#' @param threshold A value indicating the minimum correlation between
#' a variable and any component, required for this  variable to be included
#' on the PCA biplot.
#' 
#' @examples
#' path <- get_example_data("small_biocrates_example.xls")
#' dat <- read_data(path)
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_PCA_plot(dat)
#' create_PCA_plot(dat, type = "biplot", threshold = 0.3)
#' dat <- add_group(dat, "group")
#' dat <- complete_data(dat, "limit", "limit", "limit")
#' create_PCA_plot(dat, group_by = "group")
#' create_PCA_plot(dat, type = "biplot", group_by = "group", threshold = 0.3)
#' 
#' @export

create_PCA_plot <- function(dat, type = "sample_type",
                            group_by = "sample_type",
                            types_to_display = "all",
                            threshold = NULL, interactive = TRUE){
  if(group_by == "group" & is.null(attr(dat, "group")))
    stop("Provide a group to see the PCA plot.")
  
  if(type == "biplot" & is.null(threshold))
    stop("Provide a threshold.")
  
  if(is.null(attr(dat, "completed")))
    stop("Complete the missing values in data first.")
  
  if(group_by == "sample_type")
    completed_with_tooltips <- attr(dat, "completed") %>%
      group_by(`sample type`) %>%
      mutate(tooltip = paste0("Type: ", `sample type`,
                              "<br>Sample id: ", `sample identification`)) %>%
      select(-all_of(attr(dat, "metabolites")))
  else(type == "group")
    completed_with_tooltips <- attr(dat, "completed") %>%
      mutate(tooltip = paste0("Sample id: ", `sample identification`)) %>%
      select(-all_of(attr(dat, "metabolites")))
  
  mod_dat <- attr(dat, "completed") %>%
    select(all_of(c(attr(dat, "metabolites"), "tmp_id"))) %>%
    select(where(~ n_distinct(na.omit(.)) > 1)) %>%
    na.omit() %>%
    select(where(~ n_distinct(.) > 1)) %>%
    left_join(completed_with_tooltips)
  
  if(group_by == "group"){
    mod_dat <- mod_dat %>%
      filter(`sample type` == "Sample")
    
    metabo_to_remove <- mod_dat %>%
      select(any_of(attr(dat, "metabolites"))) %>%
      select(where(~ n_distinct(.) == 1)) %>%
      colnames()
    
    mod_dat <- mod_dat %>%
      select(!all_of(metabo_to_remove)) %>%
      mutate(group = do.call(paste,
                             c(across(all_of(attr(dat, "group"))),
                               sep = ", ")))
  }else{
    mod_dat <- rename(mod_dat, sample_type = `sample type`)
  }
  
  metabolites <- setdiff(attr(dat, "metabolites"),
                         c(unlist(attr(dat, "removed"))))
  
  metabo_dat <- mod_dat %>%
    mutate(across(all_of(group_by), ~ factor(., levels = unique(.)))) %>%
    select(any_of(metabolites))
  
  if(ncol(metabo_dat) == 0)
    stop("No samples without missing values found.")
  
  pca_metabolites <- colnames(metabo_dat)
  
  colnames(metabo_dat) <- paste0("V", 1:ncol(metabo_dat))
  
  pca_res <- prcomp(~., data = metabo_dat, scale. = TRUE, na.action = na.omit)
  
  if(type == "biplot"){
    plt <- as.data.frame(pca_res[["rotation"]]) %>%
      select(PC1, PC2) %>%
      mutate(Variable = pca_metabolites) %>%
      filter(if_any(PC1:PC2, ~ abs(.) >= threshold)) %>%
      ggplot() +
      geom_segment_interactive(aes(x = 0, y = 0, xend = PC1, yend = PC2), 
                   arrow = arrow(length = unit(0.1, "cm")),
                   colour = "black") +
      geom_vline(aes(xintercept = 0), alpha = 0.3, linetype = "dashed") +
      geom_hline(aes(yintercept = 0), alpha = 0.3, linetype = "dashed") +
      geom_text(aes(x = PC1 + 0.04*ifelse(PC1 < 0, -1, 1),
                    y = PC2 + 0.04*ifelse(PC2 < 0, -1, 1), label = Variable),
                colour = "black", size = 4) +
      labs(x = "PC1", y = "PC2") +
      metabocrates_theme()
  }else{
    pca_colors <- c("#54F3D3", "#2B2A29", "#F39C12", "#E74C3C", "#8E44AD",
                    "#2980B9", "#27AE60", "#D35400")
    
    pca_df <- as.data.frame(pca_res[["x"]]) %>%
      mutate(col_type = mod_dat[[group_by]],
             tooltip = mod_dat[["tooltip"]])
    
    pca_exact_colors <- pca_colors[1:nrow(unique(select(pca_df, col_type)))]
    names(pca_exact_colors) <- unlist(unique(select(pca_df, col_type)))
    
    if(type == "sample_type" & all(types_to_display != "all")){
      pca_df <- filter(pca_df, col_type %in% types_to_display)
      pca_exact_colors <- pca_exact_colors[names(pca_exact_colors) %in%
                                             types_to_display]
    }
    
    plt <- ggplot(pca_df, aes(x = PC1, y = PC2, color = col_type)) +
      geom_point_interactive(aes(tooltip = tooltip), size = 2) +
      stat_ellipse(type = "norm", linetype = 2, linewidth = 1) +
      scale_color_manual(values = pca_exact_colors,
                         name = ifelse(type == "sample_type", "Sample types", "Group levels")) +
      metabocrates_theme()
  }
  
  if(interactive)
    girafe(ggobj = plt,
           options = list(
             opts_tooltip(css = "background-color:black;color:white;padding:10px;border-radius:10px;font-family:Arial;font-size:11px;",
                          opacity = 0.9),
             opts_toolbar(saveaspng = FALSE),
             opts_zoom(min = 0.5, max = 5)
           ))
  else plt
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
  
  attr(dat, "completed") %>%
    filter(`sample type` == "Sample") %>%
    select(all_of(metabolite_name)) %>%
    pivot_longer(cols = all_of(metabolite_name), 
                 names_to = "Metabolite", 
                 values_to = "Value") %>%
    mutate(Value = as.numeric(Value))
  
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
    
  
  if(length(unique(
      attr(dat, "NA_info")[["NA_ratios_group"]][["grouping_column"]]
    )) > 5){
    stop("Group has more than 4 levels.")
  }
  
  NA_metabo_group <- attr(dat, "NA_info")[["NA_ratios_group"]] %>%
    filter(!(metabolite %in% unlist(attr(dat, "removed")))) %>%
    pivot_wider(names_from = "grouping_column", values_from = "NA_frac") %>%
    mutate(across(!metabolite, ~ .x >= threshold)) %>%
    select(!metabolite)
  
  if(length(NA_metabo_group) == 0) return(NULL)
  
  ggvenn(NA_metabo_group, show_outside = "none", stroke_color = "white",
         fill_alpha = 0.6, show_percentage = any(unlist(NA_metabo_group)),
         set_name_size = 3) +
    scale_fill_metabocrates_discrete() +
    scale_color_metabocrates_discrete()
}

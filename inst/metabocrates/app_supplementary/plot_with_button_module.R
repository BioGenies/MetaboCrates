plot_with_button_UI <- function(id, height = "500px"){
  ns <- NS(id)
  
  plt <- if(id %in% c("NA_ratios_plt", "Beeswarm", "sample_type_PCA_plt",
                      "group_PCA_plt")) {
    withSpinner(ggiraph::girafeOutput(ns("plot"), height = height))
  } else
    withSpinner(plotOutput(ns("plot")))
  
  fluidRow(
    column(11,
           helper(
             withSpinner(plt),
             type = "markdown",
             content = id)
           ),
    column(1,
           tagList(
             dropdownButton(
               inputId = id,
               tagList(
                 h4("Plot settings:"),
                 br(),
                 numericInput(ns("plot_h"),
                              label = "Plot height [inch]:",
                              value = 7, min = 1, max = 20),
                 numericInput(ns("plot_w"),
                              label = "Plot width [inch]:",
                              value = 14, min = 1, max = 20),
                 br(),
                 column(12,
                        downloadBttn(ns("download_button_pdf"),
                                     block = TRUE,
                                     label = "Download PDF",
                                     color = "primary"),
                        br(),
                        downloadBttn(ns("download_button_png"),
                                     block = TRUE,
                                     label = "Download PNG",
                                     color = "primary"),
                        br())
               ),
               circle = TRUE, status = "primary", right = TRUE,
               icon = icon("download"), width = "300px",
               tooltip = tooltipOptions(title = "Click to download!",
                                        placement = "bottom")
             )
           )
           )
  )
}

plot_with_button_SERVER <- function(id, plot_reactive, height = "auto",
                                    full_plt = NULL){
  moduleServer(id, function(input, output, session){
    if(!file.exists(paste0("./texts/", id, ".md"))){
      file.create(paste0("./texts/", id, ".md"))
    }
    
    height_val <- function(){
      ifelse(is.reactive(height),
             max(height() * 22, 400),
             height)
    }
    
    if(!is.null(full_plt)){
      output[["plot"]] <- ggiraph::renderGirafe(plot_reactive())
    }else{
      output[["plot"]] <- renderPlot(plot_reactive(),
                                     height = function() height_val(),
                                     res = 96)
    }
    
    name <- switch(id,
                   groups_plt = "groups_sizes_barplot",
                   mv_types_plt = "missing_values_barplot",
                   NA_ratios_plt = "missing_values_counts",
                   venn_diagram = "venn_diagram",
                   missing_heatmap = "missing_values_heatmap",
                   Histogram = "histogram",
                   Density = "density",
                   Beeswarm = "beeswarm",
                   Boxplot = "boxplot",
                   `Theoretical Q-Q plot` = "theoretical_qq_plot",
                   `Empirical Q-Q plot` = "empirical_qq_plot",
                   sample_type_PCA_plt = "sample_type_PCA_plot",
                   group_PCA_plt = "group_PCA_plot",
                   sample_type_PCA_variance_plt = "sample_type_variance_explained_plot",
                   group_PCA_variance_plt = "group_variance_explained_plot")
    
    output[["download_button_pdf"]] <- downloadHandler(
      filename = function() paste0(name, ".pdf"),
      content = function(file){
        if(is.null(full_plt)) plt <- plot_reactive()
        else plt <- full_plt()
        
        ggplot2::ggsave(filename = file,
                        plot = plt,
                        width = input[["plot_w"]], height = input[["plot_h"]])
      }
    )
    
    output[["download_button_png"]] <- downloadHandler(
      filename = function() paste0(name, ".png"),
      content = function(file){
        if(is.null(full_plt)) plt <- plot_reactive()
        else plt <- full_plt()
        
        ggplot2::ggsave(filename = file,
                        plot = plt,
                        width = input[["plot_w"]], height = input[["plot_h"]])
      }
    )
  })
}
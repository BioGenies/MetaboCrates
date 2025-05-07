plot_with_button_UI <- function(id){
  ns <- NS(id)
  
  plt <- if(id %in% c("NA_ratios_plt", "corr_heatmap", "dist_plt", "PCA_plt",
                      "corr_heatmap_both")) {
    withSpinner(ggiraph::girafeOutput(ns("plot")))
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
                        downloadBttn(ns("download_button"),
                                     block = TRUE,
                                     label = "Download",
                                     color = "primary")),
                 br(),
                 br()
               ),
               circle = TRUE, status = "primary", right = TRUE,
               icon = icon("download"), width = "300px",
               tooltip = tooltipOptions(title = "Click to download!",
                                        placement = "left")
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
    
    output[["download_button"]] <- downloadHandler(
      filename = function(){
        paste0(switch(id,
                      groups_plt = "groups_sizes_barplot",
                      mv_types_plt = "missing_values_barplot",
                      NA_ratios_plt = "missing_values_counts",
                      corr_heatmap = "correlations_heatmap_after_imputation",
                      venn_diagram = "venn_diagram",
                      missing_heatmap = "missing_values_heatmap",
                      dist_plt = "distribution_plot",
                      PCA_plt = "PCA_plot",
                      PCA_variance = "variance_explained_plot",
                      corr_heatmap_both = "correlations_heatmap"),
               ".pdf")
      },
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
plot_with_button_UI <- function(id){
  ns <- NS(id)
  
  if(id %in% c("NA_ratios_plt", "corr_heatmap")) plt <- ggiraph::girafeOutput(ns("plot"))
  else plt <- plotOutput(ns("plot"))
  
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

plot_with_button_SERVER <- function(id, plot_reactive, height = "auto", full_plt = NULL){
  moduleServer(id, function(input, output, session){
    height_val <- function(){
      ifelse(is.reactive(height),
             max(height() * 22, 400),
             height)
    }
    
    if(id %in% c("NA_ratios_plt", "corr_heatmap")){
      output[["plot"]] <-
        ggiraph::renderGirafe(plot_reactive())
    }else{
      output[["plot"]] <- renderPlot(plot_reactive(),
                                     height = function() height_val(),
                                     res = 96)
    }
    
    output[["download_button"]] <- downloadHandler(
      filename = function(){
        paste0(switch(id,
                      plot_groups = "groups_sizes_barplot",
                      plot_mv_types = "missing_values_barplot"), ".pdf")
      },
      content = function(file){
        pdf(file, width = input[["plot_w"]], height = input[["plot_h"]])
        plot(ifelse(is.null(full_plt), plot_reactive(), full_plt()))
        dev.off()
      }
    )
  })
}
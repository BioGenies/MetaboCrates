plot_with_button_UI <- function(id){
  ns <- NS(id)
  
  fluidRow(
    column(11,
           helper(
             withSpinner(plotOutput(ns("plot"))),
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

plot_with_button_SERVER <- function(id, plot_reactive, height = "auto"){
  moduleServer(id, function(input, output, session){
    height_val <- function(){
      ifelse(is.reactive(height),
             max(height() * 22, 400),
             height) 
    }
      
    output[["plot"]] <- renderPlot(plot_reactive(),
                                   height = function() height_val(),
                                   res = 96)
    
    output[["download_button"]] <- downloadHandler(
      filename = function(){
        paste0(switch(id,
                      plot_groups = "groups_sizes_barplot",
                      plot_mv_types = "missing_values_barplot"), ".pdf")
      },
      content = function(file){
        pdf(file, width = input[["plot_w"]], height = input[["plot_h"]])
        plot(plot_reactive())
        dev.off()
      }
    )
  })
}
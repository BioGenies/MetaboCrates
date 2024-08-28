download_plot_UI <- function(id){
  ns <- NS(id)
  
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
    ),
  )
}

download_plot_SERVER <- function(id, plot_reactive){
  moduleServer(id, function(input, output, session){
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
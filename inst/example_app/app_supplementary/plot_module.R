plot_UI <- function(id){
  helper(plotOutput(NS(id, "plot")),
         type = "markdown",
         content = id)
}

plot_SERVER <- function(id, plot_reactive){
  moduleServer(id, function(input, output, session){
    output[["plot"]] <- renderPlot(plot_reactive(), res = 96)
  })
}
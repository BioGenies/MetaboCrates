plot_groups_UI <- function(id, dat){
  helper(plotOutput(NS(id, "plot_groups")),
         type = "markdown",
         content = "plot_groups")
}

plot_groups_SERVER <- function(id, dat){
  moduleServer(id, function(input, output, session){
    output$plot_groups <- renderPlot({
      plot_groups(dat)
    }, res = 96)
  })
}
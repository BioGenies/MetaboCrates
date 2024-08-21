library(MetaboCrates)
library(shinyhelper)

source("app_supplementary/plot_groups_module.R")

path <- get_example_data("small_biocrates_example.xlsx")
dat <- read_data(path)
dat <- add_group(dat, "group")

plotApp <- function(){
  ui <- fluidPage(
    plot_groups_UI("plot_groups", dat)
  )
  
  server <- function(input, output, session){
    observe_helpers(help_dir = "texts")
    
    plot_groups_SERVER("plot_groups", dat)
  }
  
  shinyApp(ui, server)  
}


plotApp()
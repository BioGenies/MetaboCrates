library(MetaboCrates)
library(shinyhelper)
library(shinyWidgets)
library(DT)

source("app_supplementary/plot_with_button_module.R")
source("app_supplementary/table_with_button_module.R")

path <- get_example_data("small_biocrates_example.xlsx")
metabo_dat <- read_data(path)
metabo_dat <- add_group(metabo_dat, "group")

plotApp <- function(){
  ui <- fluidPage(
    tags$head(
      tags$style(HTML(
        ".dataTables_wrapper .dataTables_filter{
            float: left !important;
            padding-left: 550px !important;
          }"
      ))
    ),
    plot_with_button_UI("plot_groups"),
    table_with_button_UI("NA_counts")
  )

  server <- function(input, output, session){
    observe_helpers(help_dir = "texts")
   
    plot_groups_reactive <- reactive(plot_groups(metabo_dat))
    plot_with_button_SERVER("plot_groups", plot_groups_reactive)
    
    table_with_button_SERVER("NA_counts", attr(dat, "NA_info")[["counts"]])
 }

  shinyApp(ui, server)
}


plotApp()

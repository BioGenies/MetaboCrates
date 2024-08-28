library(MetaboCrates)
library(shinyhelper)
library(shinyWidgets)
library(DT)

source("app_supplementary/download_plot_module.R")
source("app_supplementary/plot_module.R")
source("app_supplementary/table_module.R")
source("app_supplementary/download_table_module.R")

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
    fluidRow(
      column(11, plot_UI("plot_groups")),
      column(1, download_plot_UI("plot_groups")) 
    ),
    fluidRow(
      column(11, table_UI("NA_counts")),
      column(1, download_table_UI("NA_counts"))
      ),
    fluidRow(
      column(11, plot_UI("plot_mv_types"))
    )
  )

  server <- function(input, output, session){
    observe_helpers(help_dir = "texts")
   
    plot_groups_reactive <- reactive(plot_groups(metabo_dat))
    plot_SERVER("plot_groups", plot_groups_reactive)
    download_plot_SERVER("plot_groups", plot_groups_reactive)
    
    table_SERVER("NA_counts", attr(dat, "NA_info")[["counts"]])
    download_table_SERVER("NA_counts", attr(dat, "NA_info")[["counts"]])
    
    plot_mv_types_reactive <- reactive(plot_mv_types(metabo_dat))
    plot_SERVER("plot_mv_types", plot_mv_types_reactive)
 }

  shinyApp(ui, server)
}


plotApp()

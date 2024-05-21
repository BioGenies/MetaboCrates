library(MetaboCrates)

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(shinyhelper)


ui <- navbarPage(
  theme = shinytheme("slate"),
  title = "metabocrates",
  tabPanel("About",
           column(1, HTML("<img src='logo.jpg' height='120px'>")),
           column(9,
                  h1("Welcome! This is metabocrates app!"),
                  h3("You can do here some cool stuff with your biocrates data"),
                  h3("Click `start` button and start your analysis!")),
           column(2, 
                  br(),
                  actionBttn(
                    inputId = "Id105",
                    label = "START",
                    style = "pill", 
                    color = "success",
                    icon = icon("arrow-right"),
                    size = "lg"
                  ))
  ),
  tabPanel("Uploading data"),
  tabPanel("Missing values analysis"),
  tabPanel("Quality control"),
  tabPanel("Summary"),
  tabPanel("Download")
  
  
)


server <- function(input, output, session) {
  
}

shinyApp(ui, server)
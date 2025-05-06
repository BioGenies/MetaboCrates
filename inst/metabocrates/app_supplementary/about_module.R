

about_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$footer(
      align = "right",
      style = "position:absolute; bottom:0; width:95%; height:20px; padding: 0px 0px 100px 100px;",
      HTML("<img src='funding.png' style='height: 90px'>"),
    ),
    fluidRow(
      column(2,
             align = "center",
             HTML("<img src='logo.png' height='120px'>"),
      ),
      column(10,
             h2("Welcome to MetaboCrates!", style = "font-size:23px;"),
             h2("Designed for analysis of data obtained from Biocrates® kits.", 
                style = "font-size:20px;"),
      ),
    ),
    HTML('<hr style="border-color: black;">'),
    column(11,
           htmlOutput(ns("content_about"))
    )
  )
}

about_SERVER <- function(id){
  moduleServer(id, function(input, output, session){
    output[["content_about"]] <- renderUI({
      temp_html <- tempfile(fileext = ".html")
      
      rmarkdown::render(
          input = "./app_supplementary/content_about.md",
          output_file = temp_html,
          output_format = "html_fragment",
          quiet = TRUE
      )
      
      HTML(readLines(temp_html, warn = FALSE))
    })
  })
}

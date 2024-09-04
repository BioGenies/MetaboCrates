
table_with_button_UI <- function(id){
  ns <- NS(id)
  
  fluidRow(
    column(11,
           helper(DTOutput(ns("table")),
                  type = "markdown",
                  content = id)
           ),
    column(1,
           downloadBttn(
             ns("download_button"),
             style = "jelly",
             label = NULL,
             color = "primary",
             icon = icon("download")
           )
           )
    )
}

table_with_button_SERVER <- function(id, dat_reactive){
  moduleServer(id, function(input, output, session){
    
    output[["table"]] <- renderDT(dat_reactive())
    
    output[["download_button"]] <- downloadHandler(
      filename = function(){
        paste0(switch(id,
                      NA_counts = "NA_counts"), ".csv")
      },
      content =
        function(file) write.csv(dat_reactive(), file, row.names = FALSE)
    )
  })
}
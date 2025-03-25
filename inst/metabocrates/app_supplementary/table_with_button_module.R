
table_with_button_UI <- function(id){
  helper(
    withSpinner(DTOutput(NS(id, "table"))),
    type = "markdown",
    content = id
  )
}

table_with_button_SERVER <- function(id, dat_reactive){
  moduleServer(id, function(input, output, session){
    if(!file.exists(paste0("./texts/", id, ".md"))){
      file.create(paste0("./texts/", id, ".md"))
    }
    
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
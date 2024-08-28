download_table_UI <- function(id){
  ns <- NS(id)
  
  downloadBttn(
    ns("download_button"),
    style = "jelly",
    label = NULL,
    color = "primary",
    icon = icon("download")
  )
}

download_table_SERVER <- function(id, dat){
  moduleServer(id, function(input, output, session){
    output[["download_button"]] <- downloadHandler(
      filename = function(){
        paste0(switch(id,
                      NA_counts = "NA_counts"), ".csv")
      },
      content = function(file) write.csv(dat, file, row.names = FALSE)
    )
  })
}
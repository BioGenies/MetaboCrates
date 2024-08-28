table_UI <- function(id){
  helper(DTOutput(NS(id, "table")),
         type = "markdown",
         content = id)
}

table_SERVER <- function(id, dat){
  moduleServer(id, function(input, output, session){
    output[["table"]] <- renderDT(dat)
  })
}


about_UI <- function() {
  
  tagList(
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
           markdown(readLines("texts/content_about.md")),
           markdown(MetaboCrates_citation()),
           markdown(MetaboCrates_contact()),
           markdown(MetaboCrates_funding())
    )
  )
}

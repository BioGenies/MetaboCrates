

ui_content_about <- function() {
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
           h4("About"),
           h5("MetaboCrates is an advanced application designed for the 
              comprehensive analysis of data derived from Biocrates® kits, 
              providing seamless integration with medIDQ data and offering 
              flexible data download options during the analysis process."),
           br(),
           br(),
           h4("To get started, follow these steps:"),
           h5(HTML("<b> 1. Input Data:</b> Make sure your input data is sourced 
                   from the MedIDQ application. This ensures compatibility and 
                   accurate analysis within MetaboCrates.")),
           h5(HTML("Your data must be in .xls or .xlsx format and contain the
                   following columns: plate bar code, sample type,
                   sample identification.")),
           h5(HTML("<b> 2. Performing Analysis:</b> Navigate to the <b> 
                   Analysis</b> tab to begin your data analysis. This section 
                   provides all the tools and features necessary to process and 
                   interpret your data effectively.")),
           h5(HTML("<b> 3. Downloading Data:</b> You can access the 
           <b>Download</b> tab at any point during your analysis. This allows 
                   you to download your data even while you're in the midst of 
                   working, ensuring flexibility and convenience in your 
                   workflow.")),
           br(),
           h5("We appreciate your choice to use our application. If you have any
       questions, feedback, or suggestions, please submit them as an issue on
       GitHub. Your input is valuable to us!"),
    )
  )
}
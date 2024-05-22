library(MetaboCrates)
library(dplyr)

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(shinyhelper)


source("app_supplementary/nav_module.R")
source("app_supplementary/custom_dt.R")

panels_vec <- c("About", "Uploading data", "Group selection",
                "Missing values analysis",  "Quality control", "Summary", 
                "Download")


ui <- navbarPage(
  theme = shinytheme("sandstone"),
  title = "metabocrates",
  id = "run",
  tabPanel("About",
           column(1, HTML("<img src='logo.png' height='140px'>")),
           column(9,
                  h1("Welcome! This is metabocrates app!"),
                  h3("You can do here some cool stuff with your biocrates data"),
                  h3("Click `start` button and start your analysis!")),
           column(2, 
                  align = "right",
                  br(),
                  actionButton(inputId = "start", 
                               label = "START", 
                               icon = icon("arrow-right"))
           ),
           h2(actionLink("to_start_panel", "metabocrates")),
  ),
  #########
  tabPanel(
    "Uploading data",
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    nav_btns_UI("Uploading data"),
    column(3,
           style = "background-color:#f8f5f0; border-right: 1px solid",
           br(),
           h4("Upload new data"),
           fileInput(
             inputId = 'users_path',
             label = "Upload Biocrates® file.",
             multiple = FALSE,
             accept = c(".xlsx", ".xls")
           ),
           h4("... or load your previous project"),
           fileInput(
             inputId = 'project_path',
             label = "Upload Excel sheet downloaded from MetaboCrates.",
             multiple = FALSE,
             accept = c(".xlsx", ".xls")
           ),
           br(),
           h4("Click below to upload example data!",
              style = "font-size:15px;"),
           actionBttn(
             inputId = "example_dat",
             label = "Example data",
             style = "material-flat",
             color = "success",
             icon = HTML("<i class='fa-solid fa-upload fa-bounce'></i>")
           ),
           br(),
    ),
    column(9,
           h3("Dataset preview"),
           h4("You can see metabolomics matrix and LOD table below:"),
           br(),
           tabsetPanel(
             tabPanel(
               "Data summary",
               br(),
               htmlOutput("raw_data_summary"),
             ),
             tabPanel(
               "Compounds matrix",
               br(),
               withSpinner(DT::dataTableOutput("biocrates_matrix"),
                           color = "#3e3f3a")
             ),
             tabPanel(
               align = "center",
               "Missing values",
               br(),
               br(),
               withSpinner(plotOutput("mv_types_plt", width = "70%"),
                           color = "#3e3f3a")
             )
           )
    ),
  ),
  #############
  tabPanel("Group selection",
           nav_btns_UI("Group selection"),
           column(
             3,
             style = "background-color:#f8f5f0; border-right: 1px solid",
             br(),
             h2("(optional)"),
             br(),
             h4("Select column containing grouping variable from the table.
             Click again to unselect."),
             br(),
             htmlOutput("selected_group"),
             br(),
           ),
           column(7, offset = 1,
                  withSpinner(DT::dataTableOutput("group_columns"),
                              color = "#3e3f3a")
           )
  ),
  tabPanel("Missing values analysis",
           nav_btns_UI("Missing values analysis")
  ),
  tabPanel("Quality control",
           nav_btns_UI("Quality control")
  ),
  tabPanel("Summary",
           nav_btns_UI("Summary")
  ),
  tabPanel("Download",
           nav_btns_UI("Download")
  )
  
)


################################################################################
################################################################################


server <- function(input, output, session) {
  
  ##### reactive variables
  
  dat <- reactiveValues()
  
  
  
  ##### navigation modules
  
  observeEvent(input[["start"]], {
    updateTabsetPanel(session, inputId = "run", selected = "Uploading data")
  })
  
  observeEvent(input[["to_start_panel"]], {
    updateTabsetPanel(session, inputId = "run", selected = "About")
  })
  
  callModule(nav_btns_SERVER, "Uploading data", parent_session = session, 
             panels_vec = panels_vec, panel_id = "Uploading data")
  
  callModule(nav_btns_SERVER, "Group selection", 
             parent_session = session, panels_vec = panels_vec,
             panel_id = "Group selection")
  
  callModule(nav_btns_SERVER, "Missing values analysis", 
             parent_session = session, panels_vec = panels_vec,
             panel_id = "Missing values analysis")
  
  callModule(nav_btns_SERVER, id = "Quality control", parent_session = session, 
             panels_vec = panels_vec, panel_id = "Quality control")
  
  callModule(nav_btns_SERVER, "Summary", parent_session = session, 
             panels_vec = panels_vec, panel_id = "Summary")
  
  callModule(nav_btns_SERVER, "Download", parent_session = session, 
             panels_vec = panels_vec, panel_id = "Download")
  
  
  ##### uploading data
  
  ## users data
  observeEvent(input[["users_path"]], {
    req(input[["users_path"]])
    
    dat[["metabocrates_dat"]]
    
    file <- input[["users_path"]]
    req(file)
    path <- file[["datapath"]]
    
    validate(need(tools::file_ext(path) %in% c("xlsx", "xls"),
                  paste("Please upload an xlsx or xls file!")))
    
    try({ uploaded_data <- read_data(path)})
    dat[["metabocrates_dat"]] <- uploaded_data
    
  })
  
  ## previous project
  
  
  ## example data
  
  observeEvent(input[["example_dat"]], {
    path <- get_example_data("small_biocrates_example.xls")
    dat[["metabocrates_dat"]] <- read_data(path)
  }, ignoreInit = TRUE)
  
  ## display informations
  
  output[["raw_data_summary"]] <- renderUI({
    req(dat[["metabocrates_dat"]])
    
    uploaded_dat <- dat[["metabocrates_dat"]]
    
    n_cmp <- length(attr(uploaded_dat, "metabolites"))
    n_smp <- dat[["metabocrates_dat"]] %>% 
      filter(`sample type` == "Sample") %>% 
      nrow()
    
    sample_types <- pull(attr(uploaded_dat, "samples"), `sample type`)
    
    HTML(paste0(
      "<h4> Data summary:</h4><br/>",
      "<b>Compounds:</b> ", n_cmp, ", <br/> ",
      "<b>Samples:</b> ", n_smp, ", <br/> ",
      "<b>Sample Types:</b> ",  paste0(sample_types, collapse = ", "), ", <br/> ",
      "<b>Material: </b>", paste0(unique(pull(uploaded_dat, "material")), collapse = ", "), ", <br/> ",
      "<b>OP: </b>", paste0(unique(pull(uploaded_dat, "op")), collapse = ", "), ", <br/> ",
      "<b>Plate Bar Code: </b>", paste0(unique(pull(uploaded_dat, "plate bar code")), collapse = ", "),
      "."
    ))
  })
  
  
  output[["biocrates_matrix"]] <- DT::renderDataTable({
    req(dat[["metabocrates_dat"]])
    
    metabolites <- attr(dat[["metabocrates_dat"]], "metabolites")
    
    dat_to_show <- dat[["metabocrates_dat"]] %>% 
      select(all_of(metabolites))
    
    custom_datatable(dat_to_show,
                     scrollY = 400,
                     paging = FALSE)
  })
  
  
  output[["mv_types_plt"]] <- renderPlot({
    req(dat[["metabocrates_dat"]])
    
    plot_mv_types(dat[["metabocrates_dat"]])
  })
  
  
  ######### groups selection
  
  output[["group_columns"]] <- DT::renderDataTable({
    req(dat[["metabocrates_dat"]])
    
    non_metabolites_dat <- dat[["metabocrates_dat"]] %>% 
      select(!all_of(attr(dat[["metabocrates_dat"]], "metabolites")))
    
    custom_datatable(non_metabolites_dat,
                     scrollY = 550,
                     paging = FALSE,
                     selection = list(mode = "single", target = "column"))
  })
  
  
}

shinyApp(ui, server)





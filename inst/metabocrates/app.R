library(MetaboCrates)
library(dplyr)
library(stringr)

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(shinyhelper)


source("app_supplementary/nav_module.R")
source("app_supplementary/custom_dt.R")
source("app_supplementary/ui_supp.R")

panels_vec <- c("About", "Uploading data", "Group selection",
                "Filtering", "Completing",  "Quality control", "Summary", 
                "Download")


ui <- navbarPage(
  theme = shinytheme("sandstone"),
  title = "MetaboCrates",
  tabPanel("About",
           ui_content_about()
  ),
  
  tabPanel(
    "Analysis",
    tabsetPanel(
      id = "run",
      type = "hidden",
      
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
               br(),
               br(),
               br(),
               br()
        ),
        column(9,
               column(6,
                      h3("Dataset preview"),
                      h4("You can see metabolomics matrix and LOD table below:"),
                      br()),
               column(6, align = "right", 
                      h2("Uploading data (step 1/7)"),
                      h3("next: Group selection")),
               column(12, 
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
                          column(4, 
                                 withSpinner(DT::dataTableOutput("mv_types_tbl"),
                                             color = "#3e3f3a")
                          ),
                          column(8,
                                 withSpinner(plotOutput("mv_types_plt", width = "70%"),
                                             color = "#3e3f3a")
                          )
                          
                        )
                      ))
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
               column(9,
                      column(12, align = "right", 
                             h2("Group selection (step 2/7)"),
                             h3("next: Filtering")),
                      column(9, offset = 1, 
                             withSpinner(DT::dataTableOutput("group_columns"),
                                         color = "#3e3f3a"))
               )
      ),
      tabPanel("Filtering",
               nav_btns_UI("Filtering"),
               column(12, align = "right", 
                      h2("Compounds filtering (step 3/7)"),
                      h3("next: Completing")),
      ),
      tabPanel("Completing",
               nav_btns_UI("Completing"),        
               column(3,
                      style = "background-color:#f8f5f0; border-right: 1px solid",
                      br(),
                      h3("Select methods for data imputation."),
                      br(),
                      h4("< LOD values"),
                      selectInput(
                        inputId = 'LOD_method',
                        label = "< LOD imputation method.",
                        choices = c("halfmin", "random", "halflimit", "limit", "none"),
                      ),
                      selectInput(
                        inputId = 'LOD_type',
                        label = "type of < LOD values.",
                        choices = c("OP", "calc"),
                      ),
                      br(),
                      h4("< LLOQ values"),
                      selectInput(
                        inputId = 'LLOQ_method',
                        label = "< LLOQ imputation method.",
                        choices = c("limit", "none"),
                      ),
                      br(),
                      h4("> ULOQ values"),
                      selectInput(
                        inputId = 'ULOQ_method',
                        label = "> ULOQ imputation method.",
                        choices = c("limit", "none"),
                      ),
                      br(),
                      br(),
                      actionButton(inputId = "complete_btn",
                                   label = "Complete data"),
                      br(),
                      br()
               ),
               column(9,
                      column(12, align = "right", 
                             h2("Gaps completing (step 4/7)",
                                h3("next: Quality control"))),
                      column(12, 
                             tabsetPanel(
                               tabPanel("Metabolomic matrix",
                                        withSpinner(DT::dataTableOutput("completed_tbl"),
                                                    color = "#3e3f3a")),
                               tabPanel("Table of limits",
                                        withSpinner(DT::dataTableOutput("LOD_tbl"),
                                                    color = "#3e3f3a"))
                             ),
                      )
               )
               
      ),
      tabPanel("Quality control",
               nav_btns_UI("Quality control"),
               column(12, align = "right", 
                      h2("Quality control (step 5/7)"),
                      h3("next: Summary")),
      ),
      tabPanel("Summary",
               nav_btns_UI("Summary"),
               column(12, align = "right", 
                      h2("Summary (step 6/7)"),
                      h3("next: Download")),
      ),
      
    )
  ),
  
  tabPanel("Download",
           nav_btns_UI("Download"),
           column(8, 
                  h2("Here you can download your results at any step of your work!"),
                  br(),
                  h3("Click the button below to download results."),
                  downloadButton("download_excel", "Download", style = "width:20%;"),
                  br(),
                  br(),
                  fluidRow(column(4,
                                  h4("Removed metabolites:"),
                                  br(),
                                  htmlOutput("to_remove_total"),
                  ))
           ),
           column(4, align = "right", h2("Download (step 7/7)")),
           
  )
)


################################################################################
################################################################################


server <- function(input, output, session) {
  
  ##### reactive variables
  
  dat <- reactiveValues()
  
  
  
  ##### navigation modules
  
  callModule(nav_btns_SERVER, "Uploading data", parent_session = session, 
             panels_vec = panels_vec, panel_id = "Uploading data")
  
  callModule(nav_btns_SERVER, "Group selection", 
             parent_session = session, panels_vec = panels_vec,
             panel_id = "Group selection")
  
  callModule(nav_btns_SERVER, "Filtering", 
             parent_session = session, panels_vec = panels_vec,
             panel_id = "Filtering")
  
  callModule(nav_btns_SERVER, "Completing", 
             parent_session = session, panels_vec = panels_vec,
             panel_id = "Completing")
  
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
      "<h4> Data summary:</h4><br/> <br/> ",
      "<b>Compounds:</b> ", n_cmp, ", <br/>  <br/> ",
      "<b>Samples:</b> ", n_smp, ", <br/> <br/>  ",
      "<b>Sample Types:</b> ",  paste0(sample_types, collapse = ", "), ", <br/>  <br/> ",
      "<b>Material: </b>", paste0(unique(pull(uploaded_dat, "material")), collapse = ", "), ", <br/> <br/>  ",
      "<b>OP: </b>", paste0(unique(pull(uploaded_dat, "op")), collapse = ", "), ", <br/>  <br/> ",
      "<b>Plate Bar Code: </b>", paste0(unique(pull(uploaded_dat, "plate bar code")), collapse = ", "),
      "."
    ))
  })
  
  
  output[["biocrates_matrix"]] <- DT::renderDataTable({
    req(dat[["metabocrates_dat"]])
    
    metabolites <- attr(dat[["metabocrates_dat"]], "metabolites")
    
    dat[["metabocrates_dat"]] %>% 
      select(all_of(metabolites)) %>% 
      mutate_all(as.character) %>% 
      mutate_all(display_short) %>% 
      custom_datatable(scrollY = 400,
                       paging = FALSE)
  })
  
  output[["mv_types_tbl"]] <- DT::renderDataTable({ 
    req(dat[["metabocrates_dat"]])
    
    attr(dat[["metabocrates_dat"]], "NA_info")[["counts"]] %>% 
      custom_datatable(scrollY = 200, paging = FALSE)
  })
  
  
  output[["mv_types_plt"]] <- renderPlot({
    req(dat[["metabocrates_dat"]])
    
    plot_mv_types(dat[["metabocrates_dat"]])
  })
  
  
  ######### groups selection
  
  output[["group_columns"]] <- DT::renderDataTable({
    req(dat[["metabocrates_dat"]])
    
    non_metabolites_dat <- dat[["metabocrates_dat"]] %>% 
      select(!all_of(attr(dat[["metabocrates_dat"]], "metabolites"))) %>% 
      custom_datatable(scrollY = 550,
                       paging = FALSE,
                       selection = list(mode = "single", target = "column"))
  })
  
  ######### imputation
  
  output[["LOD_tbl"]] <- DT::renderDataTable({
    req(dat[["metabocrates_dat"]])
    
    attr(dat[["metabocrates_dat"]], "LOD_table") %>% 
      select(-type) %>% 
      custom_datatable(scrollY = 300,
                       paging = FALSE,
                       selection = list(mode = "single", target = "column"))
  })
  
  
  output[["completed_tbl"]] <- DT::renderDataTable({
    req(dat[["metabocrates_dat"]])
    
    metabolites <- attr(dat[["metabocrates_dat"]], "metabolites")
    
    if(is.null(attr(dat[["metabocrates_dat"]], "completed"))) {
      dat[["metabocrates_dat"]] %>% 
        select(all_of(metabolites)) %>% 
        mutate_all(as.character) %>% 
        mutate_all(display_short) %>% 
        custom_datatable(scrollY = 400,
                         paging = FALSE)
    } else {
      attr(dat[["metabocrates_dat"]], "completed") %>% 
        select(all_of(metabolites)) %>% 
        mutate_all(as.numeric) %>% 
        mutate_all(round) %>% 
        custom_datatable(scrollY = 400,
                         paging = FALSE)
    }
    
  })
  
  
  
  
  
}

shinyApp(ui, server)





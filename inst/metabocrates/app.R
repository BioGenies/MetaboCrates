library(MetaboCrates)
library(dplyr)
library(stringr)
library(openxlsx)

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(shinyhelper)

source("app_supplementary/nav_module.R")
source("app_supplementary/custom_dt.R")
source("app_supplementary/ui_supp.R")
source("app_supplementary/plot_with_button_module.R")
source("app_supplementary/table_with_button_module.R")
source("app_supplementary/update_inputs_module.R")
source("app_supplementary/download_module.R")

panels_vec <- c("About", "Uploading data", "Group selection",
                "Filtering", "Completing",  "Quality control", "Summary", 
                "Download")


ui <- navbarPage(
  id = "main",
  
  includeCSS("www/style.css"),
  
  theme = shinytheme("sandstone"),
  title = "MetaboCrates",
  
  tags$style(HTML("
    .swal2-container {
      z-index: 10000 !important;
    }
    .custom-tabs .nav-tabs > li > a {
      height: 50px;
      display: flex;
      align-items: center;
      justify-content: center;
      text-align: center;
      white-space: normal;
      line-height: 1.2;
    }
  ")),
  
  tabPanel("About",
           ui_content_about()
  ),
  
  tabPanel(
    "Analysis",
    div(class = "custom-tabs",
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
                 label = "Upload RDS file downloaded from MetaboCrates.",
                 multiple = FALSE,
                 accept = c(".rds", ".RDS")
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
                          table_with_button_UI("biocrates_matrix")
                        ),
                        tabPanel(
                          align = "center",
                          "Missing values",
                          br(),
                          br(),
                          column(4, 
                                 table_with_button_UI("mv_types_tbl")
                          ),
                          column(8,
                                 br(),
                                 plot_with_button_UI("mv_types_plt")
                          )
                          
                        )
                      ))
        ),
      ),
      #############
      tabPanel("Group selection",
               nav_btns_UI("Group selection"),
               column(8,
                      div(style = "background-color:#f8f5f0; border-right: 1px solid"),
                      column(2,
                             h2("(optional)"),
                      ),
                      column(10,
                             br(),
                             h4("Select column containing grouping variable from the table.
             Click again to unselect."),
                             h5("A proper group column should contain names of groups. It 
                    can't contain missing values (NA's) for samples."),
                             br()
                      )
               ),
               column(4,
                      column(12, align = "right", 
                             h2("Group selection (step 2/7)"),
                             h3("next: Filtering"),
                             br()),
               ),
               column(2,
                      style = "background-color:#f8f5f0; border-right: 1px solid; height: 400px",
                      br(),
                      br(),
                      div(htmlOutput("columns_info"))
               ),
               column(10,
                      column(6,
                             br(),
                             table_with_button_UI("group_columns")
                      ),
                      column(6,
                             br(),
                             div(htmlOutput("selected_group")),
                             br(),
                             plot_with_button_UI("groups_plt")
                      )
               )
      ),
      ####################
      tabPanel("Filtering",
               nav_btns_UI("Filtering"),
               column(4,
                      style = "background-color:#f8f5f0; border-right: 1px solid",
                      br(),
                      h4("Provide threshold."),
                      h5("All metabolites that contain more missing values than 
                         provided threshold will be removed."),
                      br(),
                      numericInput(
                        inputId = "filtering_threshold",
                        label = "threshold [%]",
                        value = 80,
                        min = 0,
                        max = 100
                      ),
                      br(),
                      
                      column(8, 
                             h4("The following metabolites will be removed:"),
                      ),
                      column(4,
                             align = "right",
                             dropdownButton(
                               multiInput(
                                 inputId = "LOD_to_remove",
                                 label = "Click metabolite name to select or unselect.",
                                 choices = character(0),
                                 width = "90%",
                                 options = list(
                                   enable_search = TRUE,
                                   non_selected_header = "Metabolites:",
                                   selected_header = "Metabolites to remove:"
                                 )
                               ),
                               circle = TRUE, status = "default",
                               icon = icon("gear"), width = "700px",
                               
                               tooltip = tooltipOptions(title = "Click to edit metabolites to remove!")
                             ),
                      ),
                      br(),
                      br(),
                      br(),
                      column(12, htmlOutput("LOD_to_remove_txt")),
                      br(),
                      br(),
                      br(),
                      column(6, align = "center", 
                             actionButton("LOD_remove_btn", label = "Remove")),
                      column(6, align = "center",
                             actionButton("LOD_undo_btn", label = "Undo")),
                      br(),
                      br(),
                      br(),
                      column(12, h4("Removed metabolites:")),
                      column(12, htmlOutput("LOD_removed_txt")),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br()
                      
               ),
               column(8, align = "right",
                      h2("Compounds filtering (step 3/7)"),
                      h3("next: Completing")
               ),
               column(8,
                      tabsetPanel(
                        tabPanel("Ratios of missing values",
                                 column(10, offset = 1,
                                        br(),
                                        br(),
                                        table_with_button_UI("NA_ratios_tbl"))
                        ),
                        tabPanel("Ratios visualization",
                                 br(),
                                 br(),
                                 radioButtons("NA_percent_plt_type", 
                                              "Choose plot type",
                                              choiceValues = c("joint", "NA_type"),
                                              selected = "joint",
                                              choiceNames = c("Joint ratios", "Show NA type"),
                                              inline = TRUE),
                                 column(12,
                                        plot_with_button_UI("NA_ratios_plt"))
                        ),
                        tabPanel("Venn diagram",
                                 column(12,
                                        br(),
                                        h4("Provide a group with more than 1 and up to 4 levels to see Venn diagram."),
                                        br(),
                                        plot_with_button_UI("venn_diagram")
                                  )
                        )
                      )
               )
               
      ),
      #################
      tabPanel("Completing",
               nav_btns_UI("Completing"),        
               column(3,
                      style = "background-color:#f8f5f0; border-right: 1px solid",
                      h3("Select methods for data imputation."),
                      br(),
                      h4("< LOD values"),
                      selectInput(
                        inputId = 'LOD_method',
                        label = "< LOD imputation method",
                        choices = c("halfmin", "random", "halflimit", "limit", "limit-0.2min", "none"),
                      ),
                      selectInput(
                        inputId = 'LOD_type',
                        label = "Type of < LOD values",
                        choices = c("OP", "calc"),
                      ),
                      br(),
                      h4("< LLOQ values"),
                      selectInput(
                        inputId = 'LLOQ_method',
                        label = "< LLOQ imputation method",
                        choices = c("limit", "none"),
                      ),
                      br(),
                      h4("> ULOQ values"),
                      selectInput(
                        inputId = 'ULOQ_method',
                        label = "> ULOQ imputation method",
                        choices = c("limit", "third quartile", "none"),
                      ),
                      br(),
                      br(),
                      column(5, align = "center",
                             actionButton(inputId = "complete_btn",
                                          label = "Complete data")),
                      column(4, align = "center", offset = 1,
                             actionButton("complete_undo_btn", label = "Undo")),
                      br()
               ),
               column(9,
                      column(12, align = "right", 
                             h2("Gaps completing (step 4/7)",
                                h3("next: Quality control"))),
                      column(12, 
                             tabsetPanel(id = "imputation_tabset",
                                         tabPanel("Metabolomic matrix",
                                                  br(),
                                                  table_with_button_UI("completed_tbl")
                                         ),
                                         tabPanel("Table of limits",
                                                  br(),
                                                  table_with_button_UI("LOD_tbl")
                                         ),
                                         tabPanel("Missing values heatmap",
                                                  br(),
                                                  column(12,
                                                         selectInput("pb_codes_heatmap",
                                                                     label = "Select plate bar code",
                                                                     choices = character(0))
                                                  ),
                                                  plot_with_button_UI("missing_heatmap")
                                         ),
                                         tabPanel("Single metabolite distribution",
                                                  br(),
                                                  conditionalPanel(
                                                    condition = "input.dist_plt_type == 'Histogram'",
                                                    h4("Histogram includes all values before imputation (observed) and only those that were missing and then completed (only imputed values).",
                                                             style = "font-size:16px;")
                                                  ),
                                                  br(),
                                                  column(5,
                                                         selectInput("sing_metabo_dist",
                                                                     "Metabolite",
                                                                     choices = character(0))
                                                  ),
                                                  column(6, offset = 1,
                                                         radioButtons("dist_plt_type",
                                                                      "Plot type",
                                                                      choices = c("Histogram", "Density", "Beeswarm", "Boxplot", "Q-Q plot"),
                                                                      inline = TRUE)
                                                  ),
                                                  br(),
                                                  column(9, offset = 1,
                                                         plot_with_button_UI("dist_plt")
                                                  )
                                         ),
                                         tabPanel("Correlations heatmap",
                                                  br(),
                                                  h4("Only up to 10 metabolites are visible. Download the plot to see all chosen metabolites."),
                                                  column(4,
                                                         h5("Select metabolites", style = "font-weight: bold")
                                                  ),
                                                  column(8,
                                                         h5("Absolute correlation threshold [%]", style = "font-weight: bold")
                                                  ),
                                                  column(4,
                                                         pickerInput("corr_heatmap_metabolites",
                                                                     choices = character(0),
                                                                     options = pickerOptions(
                                                                       actionsBox = TRUE,
                                                                       selectedTextFormat = "count > 3",
                                                                       liveSearch = TRUE
                                                                     ),
                                                                     multiple = TRUE),
                                                  ),
                                                  column(8,
                                                         numericInput(
                                                           inputId = "corr_threshold",
                                                           label = NULL,
                                                           value = 0.3,
                                                           min = 0,
                                                           max = 1,
                                                           step = 0.05
                                                         ),
                                                  ),
                                                  column(10,
                                                    plot_with_button_UI("corr_heatmap")
                                                  )
                                         )
                             ),
                      )
               )
      ),
      #################
      tabPanel("Quality control",
               nav_btns_UI("Quality control"),
               tabsetPanel(
                 tabPanel("Remove metabolites",
                          column(4,
                                 style = "background-color:#f8f5f0; border-right: 1px solid",
                                 h4("Provide threshold."),
                                 h5("All metabolites for which the coefficient of variation
                                     value is greater than provided threshold will be
                                     removed."),
                                 numericInput(
                                   inputId = "cv_threshold",
                                   label = "threshold [%]",
                                   value = 20,
                                   min = 0,
                                 ),
                                 column(8, 
                                 h4("The following metabolites will be removed:"),
                                 ),
                                 column(4,
                                        align = "right",
                                        dropdownButton(
                                        multiInput(
                                          inputId = "CV_to_remove",
                                          label = "Click metabolite name to select or unselect.",
                                          choices = character(0),
                                          width = "90%",
                                          options = list(
                                            enable_search = TRUE,
                                            non_selected_header = "Metabolites:",
                                            selected_header = "Metabolites to remove:"
                                          )
                                        ),
                                        circle = TRUE, status = "default",
                                        icon = icon("gear"), width = "700px",
                                        tooltip = tooltipOptions(title = "Click to edit metabolites to remove!")
                                      ),
                                  ),
                          br(),
                          br(),
                          column(12, htmlOutput("CV_to_remove_txt")),
                          br(),
                          br(),
                          br(),
                          br(),
                          column(2, align = "center", offset = 2, 
                                 actionButton("CV_remove_btn", label = "Remove")),
                          br(),
                          br(),
                          br(),
                          column(12, h4("Metabolites removed based on the CV value:")),
                          column(12, htmlOutput("CV_removed_txt")),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          column(2, align = "center", offset = 2,
                                 actionButton("CV_undo_btn", label = "Undo")),
                   ),
                   column(8, align = "right",
                          h2("Quality control (step 5/7)"),
                          h3("next: Summary")
                   ),
                   column(6, offset = 1,
                                            br(),
                                            br(),
                                            table_with_button_UI("CV_tbl"))
                 ),
                 tabPanel("PCA",
                          column(3,
                                 style = "background-color:#f8f5f0; border-right: 1px solid; height: 500px",
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 radioButtons("PCA_type",
                                              label = "Select PCA plot type",
                                              choices = c("sample type", "biplot", "variance"),
                                              inline = TRUE),
                                 conditionalPanel(
                                   condition = "input.PCA_type == `sample type`",
                                   checkboxGroupInput(
                                     inputId = "PCA_types",
                                     label = "Select types to display",
                                     choices = character(0),
                                     inline = TRUE
                                   )
                                   
                                 ),
                                 conditionalPanel(
                                   condition = "input.PCA_type == `biplot`",
                                   numericInput(
                                     inputId = "PCA_threshold",
                                     label = "Absolute correlation threshold [%]",
                                     value = 30,
                                     min = 0,
                                     max = 100)
                                 ),
                                 conditionalPanel(
                                   condition = "input.PCA_type == `variance`",
                                   numericInput(
                                     inputId = "PCA_variance_threshold",
                                     label = "Cumulative variance threshold [%]",
                                     value = 80,
                                     min = 0,
                                     max = 100)
                                 ),
                                 conditionalPanel(
                                   condition = "input.PCA_type == `variance`",
                                   numericInput("PCA_variance_max_num",
                                                label = "maximum number of principal components",
                                                value = 5,
                                                min = 1)
                                 ),
                                 conditionalPanel(
                                   condition = "input.PCA_type == `variance`",
                                   checkboxInput("PCA_variance_cum",
                                                label = "Include cumulative variance")
                                 )
                          ),
                          column(9, align = "right",
                                 h2("Quality control (step 5/7)"),
                                 h3("next: Summary")
                          ),
                          column(9, align = "center",
                           conditionalPanel(
                              condition = "input.PCA_type == `biplot`",
                              h4("The biplot visualizes metabolite contributions to principal components, highlighting groups with similar correlations."),
                              br()
                            )
                          ),
                          column(7, offset = 1,
                                 uiOutput("cond_pca_plt")
                          )
                 )
        )
              
      ),
      #######
      tabPanel("Summary",
               nav_btns_UI("Summary"),
               column(12, align = "right", 
                      h2("Summary (step 6/7)"),
                      h3("next: Download")
               ),
               fluidRow(
                 column(5, offset = 1,
                        h3("Metabolites removed based on the")
                 ),
                 column(4, offset = 1,
                        h3("Analysis summary")
                 )
               ),
               fluidRow(
                 column(2, offset = 1,
                        h4("Limit of detection"),
                        tags$div(
                        style = "height: 350px; overflow-y: scroll; overflow-x: scroll;
                        border: 1px solid #ccc; padding: 11px;",
                        htmlOutput("summary_LOD_removed_txt")
                        ),
                        htmlOutput("LOD_threshold_txt"),
                        htmlOutput("LOD_count_txt")
                 ),
                 column(2,
                        h4("Coefficient of Variation"),
                        tags$div(
                        style = "height: 350px; overflow-y: scroll; overflow-x: scroll;
                        border: 1px solid #ccc; padding: 11px;",
                        htmlOutput("summary_CV_removed_txt")
                        ),
                        htmlOutput("CV_threshold_txt"),
                        htmlOutput("CV_count_txt")
                  ),
                 column(1, offset = 1,
                        tags$div(style = "border-left: 2px solid black; height: 450px;")
                 ),
                 column(3,
                        htmlOutput("summary_txt")
                 )
            )
      )
    )
  )),
  
  tabPanel("Download",
           nav_btns_UI("Download"),
           column(9,
                  h2("Here you can download your results at any step of your work!")
           ),
           column(3, align = "right", h2("Download (step 7/7)")),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           download_UI("download_rds"),
           download_UI("download_matrix"),
           download_UI("download_tables"),
           download_UI("download_zip"),
           download_UI("download_pdf")
  ),
  column(12, div(style = "height: 120px;"))
)


################################################################################
################################################################################
options(shiny.maxRequestSize=100*1024^2)

server <- function(input, output, session) {
  
  #### helpers
  
  observe_helpers(help_dir = "texts")
  
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
             panel_id = "Completing", dat)
  
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
    
    uploaded_data <- try({read_data(path)})
    
    if(inherits(uploaded_data, "try-error")) {
      sendSweetAlert(session, "Error!", "Check validity of your file!", 
                     type = "error")
      dat[["metabocrates_dat"]] <- NULL
      req(NULL)
    }
    
    dat[["metabocrates_dat"]] <- uploaded_data
    
  })
  
  ## previous project
  
  observeEvent(input[["project_path"]], {
    req(input[["project_path"]])
    
    project <- input[["project_path"]]
    req(project)
    project_path <- project[["datapath"]]
    
    validate(need(tools::file_ext(project_path) %in% c("RDS", "rds"),
                  paste("Please upload an RDS file!")))
    
    uploaded_project <- try({readRDS(project_path)})
    
    if(inherits(uploaded_project, "try-error")) {
      sendSweetAlert(session, "Error!", "Check validity of your file!", 
                     type = "error")
      dat[["metabocrates_dat"]] <- NULL
      req(NULL)
    }
    
    dat[["metabocrates_dat"]] <- uploaded_project
  })
  
  ## example data
  
  observeEvent(input[["example_dat"]], {
    path <- get_example_data("Submission_1.xlsx")
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
    
    update_inputs_SERVER("initial", session, input, dat)
    
    info_txt <- paste0(
      "<h4> Data summary:</h4><br/>",
      "<b>Compounds:</b> ", n_cmp, ", <br/>  <br/> ",
      "<b>Samples:</b> ", n_smp, ", <br/> <br/>  ",
      "<b>Sample Types:</b> ",  paste0(sample_types, collapse = ", "), ", <br/>  <br/> ",
      "<b>Material: </b>", paste0(unique(pull(uploaded_dat, "material")), collapse = ", "), ", <br/> <br/>  ",
      "<b>OP: </b>", paste0(unique(pull(uploaded_dat, "op")), collapse = ", "), ", <br/>  <br/> ",
      "<b>Plate Bar Code: </b>", paste0(unique(pull(uploaded_dat, "plate bar code")), collapse = ", ")
    )
    
    removed_metabolites <- c(attr(dat[["metabocrates_dat"]], "removed")[["LOD"]],
                             attr(dat[["metabocrates_dat"]], "removed")[["QC"]])
    
    info_txt_removed <- ifelse(is.null(removed_metabolites), ".",
                               paste0(", <br/> <br/> <b>Removed metabolites: </b>",
                                      paste0(removed_metabolites, collapse = ", "), "."))
    
    HTML(paste0(info_txt, info_txt_removed))
  })
  
  
  biocrates_matrix_reactive <- reactive({
    req(dat[["metabocrates_dat"]])
    
    metabolites <- attr(dat[["metabocrates_dat"]], "metabolites")
    
    dat[["metabocrates_dat"]] %>% 
      select(`sample type`, all_of(setdiff(metabolites,
                                           unlist(attr(dat[["metabocrates_dat"]], "removed"))))) %>%
      mutate_all(display_short) %>% 
      custom_datatable(scrollY = 400,
                       paging = TRUE)
  })
  
  
  table_with_button_SERVER("biocrates_matrix", biocrates_matrix_reactive)
  
  
  mv_types_tbl_reactive <- reactive({
    req(dat[["metabocrates_dat"]])
    
    attr(dat[["metabocrates_dat"]], "NA_info")[["counts"]] %>% 
      custom_datatable(scrollY = 200, paging = FALSE)
  })
  
  
  table_with_button_SERVER("mv_types_tbl", mv_types_tbl_reactive)
  
  
  mv_types_plt_reactive <- reactive({
    req(dat[["metabocrates_dat"]])
    
    plot_mv_types(dat[["metabocrates_dat"]])
  })
  
  plot_with_button_SERVER("mv_types_plt", mv_types_plt_reactive)
  
  
  ######### groups selection
  
  group_columns_DT <- reactive({
    req(dat[["metabocrates_dat"]])
    
    dat[["metabocrates_dat_group"]] <- dat[["metabocrates_dat"]]
    
    dat[["group_candidates"]] <- dat[["metabocrates_dat"]] %>% 
      select(!all_of(attr(dat[["metabocrates_dat"]], "metabolites"))) %>% 
      select(-any_of(c("plate bar code", "sample bar code", "collection date",
                       "sample identification", "op", "org. info", "plate note",
                       "plate production no.", "well position", "sample volume", 
                       "run number", "injection number", "measurement time"))) %>% 
      filter(`sample type` == "Sample") %>% 
      select(-`sample type`, -`species`) %>%
      select(where(~ !any(is.na(.))))
    
    if(!is.null(attr(dat[["metabocrates_dat"]], "group"))){
      group_ind <- which(names(dat[["group_candidates"]]) == attr(dat[["metabocrates_dat"]], "group"))
      
      dat[["group_candidates"]] %>% 
      custom_datatable(scrollY = 300,
                       paging = TRUE,
                       selection = list(mode = "single",
                                        target = "column",
                                        selected = group_ind-1))
    }else{
      dat[["group_candidates"]] %>% 
        custom_datatable(scrollY = 300,
                         paging = TRUE,
                         selection = list(mode = "single",
                                          target = "column"))
    }
  })
  
  table_with_button_SERVER("group_columns", group_columns_DT)
  
  last_selected_group <- reactiveVal(NULL)
  
  observeEvent(input[["group_columns-table_columns_selected"]], {
    req(dat[["metabocrates_dat"]])
    req(dat[["metabocrates_dat_group"]])
    
    if(is.null(input[["group_columns-table_columns_selected"]])){
      attr(dat[["metabocrates_dat_group"]], "group") <- NULL
    
      if(!is.null(last_selected_group())){
        last_selected_group(NULL)
        dat[["metabocrates_dat_group"]] <- unremove_all(dat[["metabocrates_dat_group"]], "LOD")
        dat[["metabocrates_dat_group"]] <- unremove_all(dat[["metabocrates_dat_group"]], "QC")
        attr(dat[["metabocrates_dat_group"]], "completed") <- NULL
      }
    }else{
      if(!is.null(attr(dat[["metabocrates_dat"]], "group")) &&
         is.null(last_selected_group()))
        last_selected_group(attr(dat[["metabocrates_dat"]], "group"))
    
      group_candidates <- dat[["group_candidates"]]
      group_name <- colnames(group_candidates)[input[["group_columns-table_columns_selected"]] + 1]
    
      group_col_samples <- dat[["metabocrates_dat"]] %>% 
        filter(`sample type` == "Sample") %>% 
        pull(group_name)
    
      if (!is.null(last_selected_group()) && 
          group_name == last_selected_group()) {
        req(NULL)
      }
    
      if (group_col_samples %>% is.na() %>% any()) {
        sendSweetAlert(session = session,
                       title = "Invalid group: missing group labels",
                       text = "Make sure that all samples have non-missing group name!",
                       type = "error")
      }
    
      if (any(table(group_col_samples) < 2)) {
        sendSweetAlert(session = session,
                       title = "Invalid group: too many groups",
                       text = "We require at least 2 observations per group.",
                       type = "error")
      }
      
      if (length(unique(group_col_samples)) == 1) {
        sendSweetAlert(session = session,
                       title = "Single group",
                       text = "Provided column contains only one unique label.",
                       type = "warning")
      } else {
        sendSweetAlert(session = session,
                       title = "Great!",
                       text = paste0("Group ", group_name, " selected!"),
                       type = "success")
      }
  
      dat[["metabocrates_dat_group"]] <- add_group(dat[["metabocrates_dat_group"]], group_name)
    
      last_selected_group(group_name)
    }
    
      update_inputs_SERVER("group_update", session, input, dat)
    }, ignoreNULL = FALSE)
  
  output[["columns_info"]] <- renderUI({
    req(dat[["group_candidates"]])
    
    counts <- dat[["group_candidates"]] %>%
      summarise(across(everything(), n_distinct)) %>%
      unlist() %>%
      as.integer()
    
    HTML(
      paste0("<span style = 'font-size:15px; font-weight: bold;'>",
             "Found ",
             ncol(dat[["group_candidates"]]),
             " possible grouping columns.</br></br>Unique levels per column</span></br><span style='font-size:15px'>",
             paste0(colnames(dat[["group_candidates"]]), ": ", counts, collapse = "</br>"),
             "</span>")
    )
  })
  
  output[["selected_group"]] <- renderUI({
    req(dat[["metabocrates_dat_group"]])
    
    group_name <- attr(dat[["metabocrates_dat_group"]], "group")

    if (is.null(group_name)){
      req(NULL)
    }
    
    group_col_samples <- dat[["metabocrates_dat"]] %>% 
      filter(`sample type` == "Sample") %>% 
      pull(group_name)
    
    HTML(
      paste0("<span style = 'font-size:18px; font-weight: bold;'>Selected group:</span><span style='font-size:17px'> ",
             group_name, "</span>")
    )
  })
  
  groups_plt_reactive <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    if(!is.null(attr(dat[["metabocrates_dat_group"]], "group")))
      plot_groups(dat[["metabocrates_dat_group"]])
  })
  
  
  plot_with_button_SERVER("groups_plt", groups_plt_reactive)
  
  
  ######### filtering
  
  filtering_threshold_ex <- reactiveVal(FALSE)
  
  observeEvent(input[["run"]], {
    if(input[["run"]] == "Filtering")
      filtering_threshold_ex(TRUE)
  })
  
  to_remove <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    to_remove_tmp <- setdiff(
      get_LOD_to_remove(dat[["metabocrates_dat_group"]],
                        input[["filtering_threshold"]]/100),
      c(attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]],
        attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]])
    )
    
    updateMultiInput(session, "LOD_to_remove", selected = to_remove_tmp)
    
    to_remove_tmp
  })
  
  
  output[["LOD_to_remove_txt"]] <- renderUI({
    req(to_remove)
    
    ro_remove_display <- unique(c(intersect(to_remove(), input[["LOD_to_remove"]]),
                                  input[["LOD_to_remove"]]))
    
    if(length(ro_remove_display) == 0)
      HTML("None.")
    else {
      HTML(paste0(ro_remove_display, collapse = ", "))
    }
  })
  
  
  output[["LOD_removed_txt"]] <- renderUI({
    req(dat[["metabocrates_dat_group"]])
    
    removed <- attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]]
    
    if(length(removed) == 0)
      HTML("None.")
    else {
      HTML(paste0(removed, collapse = ", "))
    }
  })
  
  
  observeEvent(input[["LOD_remove_btn"]], {
    req(dat[["metabocrates_dat_group"]])
    req(input[["LOD_to_remove"]])
    
    dat[["metabocrates_dat_group"]] <- remove_metabolites(
      dat[["metabocrates_dat_group"]],
      metabolites_to_remove = input[["LOD_to_remove"]],
      type = "LOD"
    )
    
    update_inputs_SERVER("LOD_remove_update", session, input, dat)
  })
  
  
  observeEvent(input[["LOD_undo_btn"]], {
    req(dat[["metabocrates_dat_group"]])
    
    dat[["metabocrates_dat_group"]] <- unremove_all(dat[["metabocrates_dat_group"]],
                                                    type = "LOD")
    
    update_inputs_SERVER("LOD_undo_update", session, input, dat)
  })
  
  
  NA_ratios_tbl <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    NA_table_type <- 
      ifelse(is.null(attr(dat[["metabocrates_dat_group"]], "group")), 
             "NA_ratios_type",
             "NA_ratios_group")
    
    dt <- attr(dat[["metabocrates_dat_group"]], "NA_info")[[NA_table_type]] %>% 
      filter(!(metabolite %in% c(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]],
                                 attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]]))) %>% 
      arrange(-NA_frac) %>% 
      mutate(`NA fraction [%]` = round(NA_frac*100, 3)) %>%
      select(!NA_frac)
    
    if(NA_table_type == "NA_ratios_group")
      dt <- rename(dt,
                   all_of(setNames("grouping_column", attr(dat[["metabocrates_dat_group"]], "group"))))
      
      dt %>% custom_datatable(scrollY = 300, paging = TRUE)
    
  })
  
  
  table_with_button_SERVER("NA_ratios_tbl", NA_ratios_tbl)
  
  NA_ratios_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["NA_percent_plt_type"]])
    
    metabo_num <- length(setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                                 c(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]],
                                   attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]])))
    
    plot_NA_percent(dat[["metabocrates_dat_group"]], 
                    type = input[["NA_percent_plt_type"]],
                    height_svg = max(metabo_num * 22, 400)/96,
                    width_svg = 11)
    
  })
  
  NA_ratios_plt_full <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["NA_percent_plt_type"]])
    
    plot_NA_percent(dat[["metabocrates_dat_group"]], 
                    type = input[["NA_percent_plt_type"]],
                    interactive = FALSE)
    
  })
  
  plot_with_button_SERVER("NA_ratios_plt", NA_ratios_plt,
                          NA_ratios_plt_height, full_plt = NA_ratios_plt_full)
  
  venn_plt <- reactive({
    req(input[["filtering_threshold"]])
    req(dat[["metabocrates_dat_group"]])
    req(attr(dat[["metabocrates_dat_group"]], "group"))
    
    group_len <- dat[["metabocrates_dat_group"]] %>%
      filter(`sample type` == "Sample") %>%
      select(all_of(attr(dat[["metabocrates_dat_group"]], "group"))) %>%
      unlist() %>%
      unique() %>%
      length()
    
    if(!(group_len %in% 2:5))
      req(NULL)
    
    create_venn_diagram(dat[["metabocrates_dat_group"]], input[["filtering_threshold"]]/100)
  })
  
  plot_with_button_SERVER("venn_diagram", venn_plt)
  
  corr_heatmap_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["corr_heatmap_metabolites"]])
    req(input[["corr_threshold"]])
    
    if(is.null(input[["corr_heatmap_metabolites"]]))
      NULL
    else
      create_correlations_heatmap(dat[["metabocrates_dat_group"]],
                                  threshold = input[["corr_threshold"]],
                                  metabolites_to_display =
                                    input[["corr_heatmap_metabolites"]],
                                  width_svg = 10, height_svg = 6)
  })
  
  full_corr_heatmap_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["corr_heatmap_metabolites"]])
    
    create_correlations_heatmap(dat[["metabocrates_dat_group"]],
                                threshold = input[["corr_threshold"]],
                                metabolites_to_display = input[["corr_heatmap_metabolites"]],
                                interactive = FALSE)
  })
  
  plot_with_button_SERVER("corr_heatmap", corr_heatmap_plt, full_plt = full_corr_heatmap_plt)
  
  ######### imputation
  
  LOD_tbl_reactive <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    attr(dat[["metabocrates_dat_group"]], "LOD_table") %>%
      select(-c(type,
                attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]],
                attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]])) %>%
      mutate(across(everything(), display_short)) %>%
      custom_datatable(scrollY = 300,
                       paging = TRUE,
                       selection = list(mode = "single", target = "column"))
  })
  
  table_with_button_SERVER("LOD_tbl", LOD_tbl_reactive)
  
  completed_tbl_reactive <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    metabolites <- setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                           c(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]],
                             attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]]))
    
    if(is.null(attr(dat[["metabocrates_dat_group"]], "completed"))) {
      dat_to_display <- dat[["metabocrates_dat_group"]] %>%
        filter(`sample type` == "Sample") %>%
        select(all_of(metabolites)) %>%
        mutate_all(display_short)
    } else {
      dat_to_display <- attr(dat[["metabocrates_dat_group"]], "completed") %>%
        filter(`sample type` == "Sample") %>%
        select(all_of(metabolites)) %>% 
        mutate_all(as.numeric) %>% 
        mutate_all(round, 5) 
    }
    
    dat_to_display %>% 
      custom_datatable(scrollY = 300, paging = TRUE)
  })
  
  table_with_button_SERVER("completed_tbl", completed_tbl_reactive)
  
  observeEvent(input[["complete_btn"]], {
    req(dat[["metabocrates_dat_group"]])
    
    imp_method <- function(method){
      if(method == "none") NULL
      else method
    }
    
    dat[["metabocrates_dat_group"]] <-
      complete_data(dat[["metabocrates_dat_group"]],
                    LOD_method = imp_method(input[["LOD_method"]]),
                    LLOQ_method = imp_method(input[["LLOQ_method"]]),
                    ULOQ_method = imp_method(input[["ULOQ_method"]]),
                    LOD_type = input[["LOD_type"]])
    
    update_inputs_SERVER("complete_update", session, input, dat)
  })
  
  observeEvent(input[["complete_undo_btn"]], {
    req(dat[["metabocrates_dat_group"]])
    
    attr(dat[["metabocrates_dat_group"]], "completed") <- NULL
    
    update_inputs_SERVER("complete_undo_update", session, input, dat)
  })
  
  missing_heatmap <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["pb_codes_heatmap"]])
    
    plot_heatmap(dat[["metabocrates_dat_group"]],
                 plate_bar_code = input[["pb_codes_heatmap"]])
  })
  
  missing_heatmap_height <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    length(attr(dat[["metabocrates_dat_group"]], "metabolites"))
  })
  
  plot_with_button_SERVER("missing_heatmap", missing_heatmap,
                          missing_heatmap_height)
  
  
  dist_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(attr(dat[["metabocrates_dat_group"]], "completed"))
    req(input[["sing_metabo_dist"]])
    
    switch(input[["dist_plt_type"]],
           "Histogram" = create_distribution_plot(dat[["metabocrates_dat_group"]],
                                                  input[["sing_metabo_dist"]],
                                                  width_svg = 10, height_svg = 6),
           "Density" = create_distribution_plot(dat[["metabocrates_dat_group"]],
                                                input[["sing_metabo_dist"]],
                                                type = "density",
                                                width_svg = 10, height_svg = 6),
           "Beeswarm" = create_distribution_plot(dat[["metabocrates_dat_group"]],
                                                input[["sing_metabo_dist"]],
                                                type = "beeswarm",
                                                width_svg = 10, height_svg = 6),
           "Boxplot" = create_boxplot(dat[["metabocrates_dat_group"]],
                                      input[["sing_metabo_dist"]],
                                      width_svg = 10, height_svg = 6),
           "Q-Q plot" = create_qqplot(dat[["metabocrates_dat_group"]],
                                      input[["sing_metabo_dist"]],
                                      width_svg = 10, height_svg = 6)
       )
  })
  
  full_dist_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(attr(dat[["metabocrates_dat_group"]], "completed"))
    req(input[["sing_metabo_dist"]])
    
    switch(input[["dist_plt_type"]],
           "Histogram" = create_distribution_plot(dat[["metabocrates_dat_group"]],
                                                  input[["sing_metabo_dist"]],
                                                  interactive = FALSE),
           "Density" = create_distribution_plot(dat[["metabocrates_dat_group"]],
                                                input[["sing_metabo_dist"]],
                                                type = "density",
                                                interactive = FALSE),
           "Beeswarm" = create_distribution_plot(dat[["metabocrates_dat_group"]],
                                                 input[["sing_metabo_dist"]],
                                                 type = "beeswarm_interactive",
                                                 interactive = FALSE),
           "Boxplot" = create_boxplot(dat[["metabocrates_dat_group"]],
                                      input[["sing_metabo_dist"]],
                                      interactive = FALSE),
           "Q-Q plot" = create_qqplot(dat[["metabocrates_dat_group"]],
                                      input[["sing_metabo_dist"]],
                                      interactive = FALSE)
       )
  })
  
  plot_with_button_SERVER("dist_plt", dist_plt, full_plt = full_dist_plt)
  
  ######## Quality control
  
  observeEvent(input[["run"]], {
    if(input[["run"]] == "Quality control"){
      
    if(is.null(dat[["metabocrates_dat_group"]])){
      print("NO")
      dat[["metabocrates_dat_group"]] <- dat[["metabocrates_dat"]]
    }
      
    if(is.null(attr(dat[["metabocrates_dat_group"]], "completed"))){
     sendSweetAlert(session,
                     title = "Incompleted data - automatic imputation",
                     text = HTML("<div style='text-align: left;'>
                       Quality control requires imputed data.<br>
                       Automatic imputation has been applied using<br>
                       - LOD method: <b>halfmin</b>,<br>
                       - LOD type: <b>calc</b>,<br>
                       - LLOQ method: <b>limit</b>,<br>
                       - ULOQ method: <b>third quartile</b>.<br>
                       You can go back anytime to modify the imputation."),
                     type = "warning",
                     html = TRUE)
    
      updateSelectInput(session, "LOD_method",
                        selected = "halfmin")
    
      updateSelectInput(session, "LLOQ_method",
                        selected = "limit")
    
      updateSelectInput(session, "ULOQ_method",
                        selected = "third quartile")
    
      updateSelectInput(session, "LOD_type",
                        selected = "calc")
    
      dat[["metabocrates_dat_group"]] <-
        complete_data(dat[["metabocrates_dat_group"]],
                      LOD_method = "halfmin",
                      LLOQ_method = "limit",
                      ULOQ_method = "third quartile",
                      LOD_type = "calc")
    
      update_inputs_SERVER("complete_update", session, input, dat)
    }
      
      dat[["metabocrates_dat_group"]] <-
        calculate_CV(dat[["metabocrates_dat_group"]])
      
      update_inputs_SERVER("cv_update", session, input, dat)
    }
  })
  
  to_remove_CV <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["cv_threshold"]])
    
    to_remove <- setdiff(
      get_CV_to_remove(dat[["metabocrates_dat_group"]],
                       input[["cv_threshold"]]/100),
      c(attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]],
        attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]])
    )
    
    updateMultiInput(session, "CV_to_remove", selected = to_remove)
    
    to_remove
  })
  
  output[["CV_to_remove_txt"]] <- renderUI({
    to_remove_CV_display <- unique(c(intersect(to_remove_CV(),
                                               input[["CV_to_remove"]]),
                                     input[["CV_to_remove"]]))
    
    if(length(to_remove_CV_display) == 0)
      HTML("None.")
    else {
      HTML(paste0(to_remove_CV_display, collapse = ", "))
    }
  })
  
  output[["CV_removed_txt"]] <- renderUI({
    req(dat[["metabocrates_dat_group"]])
    
    removed <- attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]]
    
    if(length(removed) == 0)
      HTML("None.")
    else {
      HTML(paste0(removed, collapse = ", "))
    }
  })
  
  observeEvent(input[["CV_remove_btn"]], {
    req(dat[["metabocrates_dat_group"]])
    req(input[["CV_to_remove"]])
    
    dat[["metabocrates_dat_group"]] <- remove_metabolites(
      dat[["metabocrates_dat_group"]],
      metabolites_to_remove = input[["CV_to_remove"]],
      type = "QC"
    )
    
    update_inputs_SERVER("cv_remove_update", session, input, dat)
  })
  
  
  observeEvent(input[["CV_undo_btn"]], {
    req(dat[["metabocrates_dat_group"]])
    
    dat[["metabocrates_dat_group"]] <- unremove_all(dat[["metabocrates_dat_group"]],
                                                   type = "QC")
    
    update_inputs_SERVER("cv_undo_update", session, input, dat)
  })
  
  
  CV_tbl <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    attr(dat[["metabocrates_dat_group"]], "cv") %>% 
      filter(!(metabolite %in% c(attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]],
                                       attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]]))) %>% 
      arrange(-CV) %>% 
      mutate(`CV [%]` = round(CV*100, 3)) %>%
      select(!CV) %>%
      custom_datatable(scrollY = 300, paging = TRUE)
  })
  
  table_with_button_SERVER("CV_tbl", CV_tbl)
  
  
  PCA_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["PCA_type"]])
    req(length(setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                       unlist(attr(dat[["metabocrates_dat_group"]], "removed")))) > 1)
    
    if(input[["PCA_type"]] == "variance") req(NULL)
    if(input[["PCA_type"]] == "biplot") req(input[["PCA_threshold"]])
    
    if(input[["PCA_type"]] == "sample type" && is.null(input[["PCA_types"]]))
      NULL
    else{
      if(input[["PCA_type"]] == "sample type")
        types_to_display <- input[["PCA_types"]]
      else types_to_display <- "all"
      
      create_PCA_plot(dat[["metabocrates_dat_group"]],
                      type = ifelse(input[["PCA_type"]] == "sample type",
                                    "sample_type", input[["PCA_type"]]),
                      types_to_display = types_to_display,
                      threshold = input[["PCA_threshold"]]/100,
                      width_svg = 10, height_svg = 6)
    }
  })
  
  PCA_plt_full <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["PCA_type"]])
    req(length(setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                       unlist(attr(dat[["metabocrates_dat_group"]], "removed")))) > 1)
    
    if(input[["PCA_type"]] == "biplot") req(input[["PCA_threshold"]])
    if(input[["PCA_type"]] == "sample type") req(input[["PCA_types"]])
    
    if(input[["PCA_type"]] == "sample type")
      types_to_display <- input[["PCA_types"]]
    else types_to_display <- "all"
    
    create_PCA_plot(dat[["metabocrates_dat_group"]],
                    type = ifelse(input[["PCA_type"]] == "sample type",
                                  "sample_type", input[["PCA_type"]]),
                    types_to_display = types_to_display,
                    threshold = input[["PCA_threshold"]]/100,
                    interactive = FALSE
    )
  })
  
  plot_with_button_SERVER("PCA_plt", PCA_plt, full_plt = PCA_plt_full)
  
  PCA_variance_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["PCA_type"]])
    if(input[["PCA_type"]] != "variance") req(NULL)
    req(length(setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                       unlist(attr(dat[["metabocrates_dat_group"]], "removed")))) > 1)
    
    pca_variance(dat[["metabocrates_dat_group"]],
                 threshold = input[["PCA_variance_threshold"]]/100,
                 max_num = input[["PCA_variance_max_num"]],
                 cumulative = input[["PCA_variance_cum"]])
  })
  
  plot_with_button_SERVER("PCA_variance_plt", PCA_variance_plt)
  
  output[["cond_pca_plt"]] <- renderUI({
    if(input[["PCA_type"]] == "variance"){
      plot_with_button_UI("PCA_variance_plt")
    }else{
      plot_with_button_UI("PCA_plt")
    }
  })
  
  ######## Summary
  
  output[["summary_LOD_removed_txt"]] <- renderUI({
    req(dat[["metabocrates_dat_group"]])
    
    HTML(paste(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]],
               collapse = "<br>"))
  })
  
  output[["LOD_threshold_txt"]] <- renderUI({
    req(dat[["metabocrates_dat_group"]])
    
    threshold <- ifelse(
      is.null(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]]),
      "none",
      paste0(input[["filtering_threshold"]], "%")
    )
    
    HTML(paste0("<b>Threshold: ", threshold, "</b>"))
  })
  
  output[["LOD_count_txt"]] <- renderUI({
    req(dat[["metabocrates_dat_group"]])
    
    HTML(paste0("<b>Count: ",
                length(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]]),
                "</b>"))
  })
  
  output[["summary_CV_removed_txt"]] <- renderUI({
    req(dat[["metabocrates_dat_group"]])
    
    HTML(paste(attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]],
               collapse = "<br>"))
  })
  
  output[["CV_threshold_txt"]] <- renderUI({
    req(dat[["metabocrates_dat_group"]])
    
    if(is.null(dat[["metabocrates_dat_group"]])){
      HTML("<b>Treshold: none</b>")
    }
    
    threshold <- ifelse(
      is.null(attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]]),
      "none",
      paste0(input[["cv_threshold"]], "%")
    )
    
    HTML(paste0("<b>Threshold: ", threshold, "</b>"))
  })
  
  output[["CV_count_txt"]] <- renderUI({
    req(dat[["metabocrates_dat_group"]])
    
    if(is.null(attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]])){
      HTML("<b>Count: 0</b>")
    }
    
    HTML(paste0("<b>Count: ",
                length(attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]]),
                "</b>"))
  })
  
  output[["summary_txt"]] <- renderUI({
    req(dat[["metabocrates_dat_group"]])
    
    HTML(paste0(
      "<div style='font-size: 16px;'>",
      "<br><br><b>Grouping column: </b>",
      ifelse(is.null(attr(dat[["metabocrates_dat_group"]], "group")),
             "none",
             paste0(attr(dat[["metabocrates_dat_group"]], "group"),
                    "<br><b>Levels:</b><br>",
                    paste0(sort(unique(attr(dat[["metabocrates_dat_group"]], "NA_info")[["NA_ratios_group"]][["grouping_column"]])),
                           collapse = "<br>")
                    )
             ),
      "<br><br>",
      "<b>Imputation:</b><br>",
      ifelse(is.null(attr(dat[["metabocrates_dat_group"]], "completed")),
             "<span style='margin-left: 2em;'>Skipped</span>",
             paste0(
               "<span style='margin-left: 1em;'><b>LOD method: </b></span>",
               input[["LOD_method"]],
               "<br>",
               "<span style='margin-left: 1em;'><b>LOD type: </b></span>",
               input[["LOD_type"]],
               "<br>",
               "<span style='margin-left: 1em;'><b>LLOQ method: </b></span>",
               input[["LLOQ_method"]],
               "<br>",
               "<span style='margin-left: 1em;'><b>ULOQ method: </b>",
               gsub("\\s+", "&nbsp;", input[["ULOQ_method"]]),
               "</span>")
      ),
      "</div>"
    ))
  })
  
  ###### Downloading
  
  download_SERVER("download_rds", dat, input)
  download_SERVER("download_matrix", dat, input)
  download_SERVER("download_tables", dat, input)
  download_SERVER("download_zip", dat, input)
  download_SERVER("download_pdf", dat, input,
                  filtering_threshold_ex = filtering_threshold_ex)
}

shinyApp(ui, server, options = list(launch.browser = TRUE))

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

addResourcePath("readme_files", system.file("readme_files", package = "MetaboCrates"))

source("app_supplementary/nav_module.R")
source("app_supplementary/custom_dt.R")
source("app_supplementary/about_module.R")
source("app_supplementary/plot_with_button_module.R")
source("app_supplementary/table_with_button_module.R")
source("app_supplementary/update_inputs_module.R")
source("app_supplementary/download_module.R")
source("app_supplementary/message_box.R")

# to obtain funding image
source(system.file("readme_scripts.R", package = "MetaboCrates"))

options(
  shiny.maxRequestSize=100*1024^2,
  spinner.color = "#54F3D3"
)

panels_vec <- c("About", "Uploading data", "Group selection",
                "Filtering", "Completing",  "Quality control",
                "Outlier detection", "Summary", "Download")


ui <- navbarPage(
  id = "main",
  
  includeCSS("www/style.css"),
  footer = HTML("<img src='readme_files/funding.png' style='height: 90px'>"),
  
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
    div.dataTables_processing {
      background-color: white;
    }
    .full-height {
      min-height: 80vh;
      background-color: #f8f5f0;
      border-right: 1px solid #ccc;
    }
  ")),
  
  tabPanel("About",
           about_UI(),
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
        column(12, 
               tabsetPanel(
                 tabPanel(
                   "Data summary",
                   column(3,
                          class = "full-height",
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
                          )
                   ),
                   column(9,
                          column(12, align = "right", 
                                 h2("Uploading data (step 1/8)"),
                                 h3("next: Group selection")),
                          br(),
                          withSpinner(htmlOutput("raw_data_summary"))
                   ),
                 ),
                 tabPanel(
                   "Compounds matrix",
                   column(3,
                          class = "full-height",
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
                          )
                   ),
                   column(9,
                          column(12, align = "right", 
                                 h2("Uploading data (step 1/8)"),
                                 h3("next: Group selection")),
                          br(),
                          column(12,
                                 table_with_button_UI("biocrates_matrix") 
                          )
                   )
                 ),
                 tabPanel(
                   "Missing values",
                   column(3,
                          class = "full-height",
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
                          )
                   ),
                   column(9,
                          column(12, align = "right", 
                                 h2("Uploading data (step 1/8)"),
                                 h3("next: Group selection")),
                          br(),
                          br(),
                          column(4, 
                                 table_with_button_UI("mv_types_tbl")
                          ),
                          column(8,
                                 br(),
                                 uiOutput("mv_types_plt_ui")
                          )      
                   )
                 )
               ))
      ),
      #############
      tabPanel("Group selection",
               nav_btns_UI("Group selection"),
               tabsetPanel(
                 tabPanel("Group selection",
                          column(3,
                                 class = "full-height",
                                 style = "background-color:#f8f5f0; border-right: 1px solid",
                                 br(),
                                 h4("Click to select one or more rows in the table to choose columns for grouping (optional)."),
                                 uiOutput("selected_info"),
                                 br(),
                                 column(6, align = "center",
                                        actionButton("add_group",
                                                     "Apply"
                                        )
                                 ),
                                 column(6, align = "center",
                                        actionButton("remove_group",
                                                     "Undo"
                                        )
                                 ),
                                 br(),
                                 br(),
                                 br(),
                                 uiOutput("group_info")
                          ),
                          column(9, align = "right", 
                                 h2("Group selection (step 2/8)"),
                                 h3("next: Filtering"),
                          ),
                          column(7, offset = 1,
                                 br(),
                                 table_with_button_UI("group_columns")
                          )
               ),
               tabPanel("Group summary",
                        column(12, align = "right",
                               h2("Group selection (step 2/8)"),
                               h3("next: Filtering"),
                        ),
                        uiOutput("groups_plt_ui")
               )
          )
      ),
      ####################
      tabPanel("Filtering",
               nav_btns_UI("Filtering"),
               tabsetPanel(
                 tabPanel("Ratios of missing values",
                          column(4,
                                 class = "full-height",
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
                                   max = 100,
                                   step = 5
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
                                 column(12, htmlOutput("LOD_removed_txt"))
                                 
                          ),
                          column(8,
                                 column(12, align = "right",
                                        h2("Compounds filtering (step 3/8)"),
                                        h3("next: Completing")
                                 ),
                                 br(),
                                 br(),
                                 table_with_button_UI("NA_ratios_tbl"))
                 ),
                 tabPanel("Ratios visualization",
                          column(12, align = "right",
                                 h2("Compounds filtering (step 3/8)"),
                                 h3("next: Completing")
                          ),
                          br(),
                          br(),
                          column(8, offset = 2,
                                 radioButtons(
                                   "NA_percent_plt_type", 
                                   "Choose plot type",
                                   choiceValues = c("joint", "NA_type"),
                                   selected = "joint",
                                   choiceNames = c("Joint ratios", "Show NA type"),
                                   inline = TRUE
                                 ),
                                 uiOutput("NA_ratios_plt_ui")   
                          )
                 ),
                 tabPanel("Venn diagram",
                          column(12, align = "right",
                                 h2("Compounds filtering (step 3/8)"),
                                 h3("next: Completing")
                          ),
                          uiOutput("venn_diagram_ui")
                 )
               )
               
      ),
      #################
      tabPanel("Completing",
               nav_btns_UI("Completing"),
               tabsetPanel(id = "imputation_tabset",
                           tabPanel("Data completing",
                                    column(3,
                                           class = "full-height",
                                           style = "background-color:#f8f5f0; border-right: 1px solid",
                                           h3("Select methods for data imputation"),
                                           br(),
                                           h4("< LOD values"),
                                           selectInput(inputId = 'LOD_method',
                                                       label = "< LOD imputation method",
                                                       choices = c("halfmin", "random", "halflimit", "limit", "limit-0.2min", "none")
                                           ),
                                           selectInput(inputId = 'LOD_type',
                                                       label = "Type of < LOD values",
                                                       choices = c("OP", "calc")
                                           ),
                                           br(),
                                           h4("< LLOQ values"),
                                           selectInput(
                                             inputId = 'LLOQ_method',
                                             label = "< LLOQ imputation method",
                                             choices = c("limit", "none")
                                           ),
                                           br(),
                                           h4("> ULOQ values"),
                                           selectInput(
                                             inputId = 'ULOQ_method',
                                             label = "> ULOQ imputation method",
                                             choices = c("limit", "third quartile", "none")
                                           ),
                                           br(),
                                           br(),
                                           column(5, align = "center",
                                                  actionButton(inputId = "complete_btn",
                                                               label = "Complete data")
                                           ),
                                           column(4, align = "center", offset = 1,
                                                  actionButton("complete_undo_btn", label = "Undo"),
                                                  br()
                                           )
                                    ),
                                    column(9, align = "right",
                                           h2("Gaps completing (step 4/8)"),
                                           h3("next: Quality control")
                                    ),
                                    column(9,
                                           br(),
                                           table_with_button_UI("completed_tbl")
                                    )
                           ),
                           tabPanel("Table of limits",
                                    column(12, align = "right",
                                           h2("Gaps completing (step 4/8)"),
                                           h3("next: Quality control")
                                    ),
                                    br(),
                                    column(9, offset = 1,
                                           table_with_button_UI("LOD_tbl")      
                                    )
                           ),
                           tabPanel("Missing values heatmap",
                                   column(3,
                                          class = "full-height",
                                          style = "background-color:#f8f5f0; border-right: 1px solid",
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          selectInput("pb_codes_heatmap",
                                                      label = "Select plate bar code",
                                                      choices = character(0)
                                          ),
                                          br(),
                                          checkboxInput("missing_heatmap_colors",
                                                        label = HTML("<b>Show colors</b>")
                                          )
                                   ),
                                   column(9, align = "right",
                                          h2("Gaps completing (step 4/8)"),
                                          h3("next: Quality control")
                                   ),
                                   column(9,
                                          plot_with_button_UI("missing_heatmap")
                                   )
                           ),
                           tabPanel("Single metabolite distribution",
                                    column(3,
                                           class = "full-height",
                                           style = "background-color:#f8f5f0; border-right: 1px solid",
                                           br(),
                                           br(),
                                           br(),
                                           br(),
                                           selectInput("sing_metabo_dist",
                                                       "Metabolite",
                                                       choices = character(0)
                                           ),
                                           br(),
                                           selectInput("dist_plt_type",
                                                       "Plot type",
                                                       choices = c("Histogram", "Density", "Beeswarm",
                                                                   "Boxplot", "Theoretical Q-Q plot", "Empirical Q-Q plot")
                                           ),
                                           conditionalPanel(
                                             condition = "input.dist_plt_type == 'Histogram'",
                                             tagList(
                                               br(),
                                               numericInput("hist_bins",
                                                            "Number of bins",
                                                            value = 30,
                                                            min = 5,
                                                            max = 100,
                                                            step = 5
                                               ),
                                               br(),
                                               checkboxInput("hist_type",
                                                             HTML("<b>Show only imputed values</b>")
                                               )
                                             )
                                           )
                                    ),
                                    column(9, align = "right",
                                           h2("Gaps completing (step 4/8)"),
                                           h3("next: Quality control")
                                    ),
                                    conditionalPanel(
                                      condition = "input.hist_type",
                                      column(9,
                                        br(),
                                        create_message_box("Histogram shows all values before imputation
                                                                  (observed) and only those that were missing and
                                                                  then completed<br>&emsp;&emsp;(only imputed values).",
                                                           type = "description"),
                                        br()
                                      )
                                    ),
                                    uiOutput("dist_plt_ui")
                           )
                )
      ),
      #################
      tabPanel("Quality control",
               nav_btns_UI("Quality control"),
                          column(4,
                                 class = "full-height",
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
                                   step = 5
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
                          h2("Quality control (step 5/8)"),
                          h3("next: Outlier detection")
                   ),
                   column(6, offset = 1,
                                            br(),
                                            br(),
                                            table_with_button_UI("CV_tbl")
                   )
      ),
      #######
      tabPanel("Outlier detection",
               nav_btns_UI("Outlier detection"),
               tabsetPanel(id = "outlier_detection",
                           tabPanel("Sample type PCA",
                                    column(3,
                                           class = "full-height",
                                           style = "background-color:#f8f5f0; border-right: 1px solid; height: 500px",
                                           br(),
                                           br(),
                                           br(),
                                           br(),
                                           radioButtons("sample_type_PCA_type",
                                                        label = "Select PCA plot type",
                                                        choices = c("scatterplot", "biplot", "variance"),
                                                        inline = TRUE),
                                           conditionalPanel(
                                             condition = "input.sample_type_PCA_type == `scatterplot`",
                                             checkboxGroupInput(
                                               inputId = "sample_type_PCA_types",
                                               label = "Select types to display",
                                               choices = character(0),
                                               inline = TRUE
                                             )
                                             
                                           ),
                                           conditionalPanel(
                                             condition = "input.sample_type_PCA_type == `biplot`",
                                             numericInput(
                                               inputId = "sample_type_PCA_threshold",
                                               label = "Absolute correlation threshold [%]",
                                               value = 30,
                                               min = 0,
                                               max = 100,
                                               step = 5
                                              )
                                           ),
                                           conditionalPanel(
                                             condition = "input.sample_type_PCA_type == `variance`",
                                             numericInput(
                                               inputId = "sample_type_PCA_variance_threshold",
                                               label = "Cumulative variance threshold [%]",
                                               value = 80,
                                               min = 0,
                                               max = 100,
                                               step = 5
                                              )
                                           ),
                                           conditionalPanel(
                                             condition = "input.sample_type_PCA_type == `variance`",
                                             numericInput("sample_type_PCA_variance_max_num",
                                                          label = "maximum number of principal components",
                                                          value = 5,
                                                          min = 1)
                                           ),
                                           conditionalPanel(
                                             condition = "input.sample_type_PCA_type == `variance`",
                                             checkboxInput("sample_type_PCA_variance_cum",
                                                           label = "Include cumulative variance")
                                           )
                                    ),
                                    column(9, align = "right",
                                           h2("Outlier detection (step 6/8)"),
                                           h3("next: Summary")
                                    ),
                                    column(9, align = "center",
                                           conditionalPanel(
                                             condition = "input.sample_type_PCA_type == `biplot`",
                                             create_message_box("Biplot visualizes metabolite contributions to principal components,
                                                                highlighting groups with similar correlations.",
                                                                type = "description"),
                                             br()
                                           )
                                    ),
                                    column(7, offset = 1,
                                           uiOutput("sample_type_cond_pca_plt")
                                    )
                           ),
                           tabPanel("Group PCA",
                                    column(3,
                                           class = "full-height",
                                           style = "background-color:#f8f5f0; border-right: 1px solid; height: 500px",
                                           br(),
                                           br(),
                                           br(),
                                           br(),
                                           radioButtons("group_PCA_type",
                                                        label = "Select PCA plot type",
                                                        choices = c("scatterplot", "biplot", "variance"),
                                                        inline = TRUE
                                           ),
                                           conditionalPanel(
                                             condition = "input.group_PCA_type == `biplot`",
                                             numericInput(
                                               inputId = "group_PCA_threshold",
                                               label = "Absolute correlation threshold [%]",
                                               value = 30,
                                               min = 0,
                                               max = 100,
                                               step = 5
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.group_PCA_type == `variance`",
                                             numericInput(
                                               inputId = "group_PCA_variance_threshold",
                                               label = "Cumulative variance threshold [%]",
                                               value = 80,
                                               min = 0,
                                               max = 100,
                                               step = 5
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.group_PCA_type == `variance`",
                                             numericInput("group_PCA_variance_max_num",
                                                          label = "maximum number of principal components",
                                                          value = 5,
                                                          min = 1)
                                           ),
                                           conditionalPanel(
                                             condition = "input.group_PCA_type == `variance`",
                                             checkboxInput("group_PCA_variance_cum",
                                                           label = "Include cumulative variance")
                                           )
                                    ),
                                    column(9, align = "right",
                                           h2("Outlier detection (step 6/8)"),
                                           h3("next: Summary")
                                    ),
                                    uiOutput("group_cond_pca_plt")
                           )
               )
      ),
      #######
      tabPanel("Summary",
               nav_btns_UI("Summary"),
               column(12, align = "right", 
                      h2("Summary (step 7/8)"),
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
           column(3, align = "right", h2("Download (step 8/8)")),
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
  
  callModule(nav_btns_SERVER, id = "Outlier detection", parent_session = session, 
             panels_vec = panels_vec, panel_id = "Outlier detection")
  
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
    
    tryCatch(
      uploaded_data <- read_data(path),
      error = function(e){
        if(e[["message"]] == "Found duplicated column names."){
          sendSweetAlert(session, "Error!",
                         "Column names have to be unique, ignoring case!", 
                         type = "error")
        }else{
          sendSweetAlert(session, "Error!", "Check validity of your file!", 
                         type = "error")
        }
        
        dat[["metabocrates_dat"]] <- NULL
        req(NULL)
      }
    )
    
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
      "<b>Compounds:</b> ", n_cmp, ". <br/>  <br/> ",
      "<b>Samples:</b> ", n_smp, ". <br/> <br/>  ",
      "<b>Sample Types:</b> ",  paste0(sample_types, collapse = ", "), ". <br/>  <br/> ",
      "<b>Material: </b>", paste0(unique(pull(uploaded_dat, "material")), collapse = ", "), ". <br/> <br/>  ",
      "<b>OP: </b>", paste0(unique(pull(uploaded_dat, "op")), collapse = ", "), ". <br/>  <br/> ",
      "<b>Plate Bar Code: </b>", paste0(unique(pull(uploaded_dat, "plate bar code")), collapse = ", ")
    )
    
    removed_metabolites <- c(attr(dat[["metabocrates_dat"]], "removed")[["LOD"]],
                             attr(dat[["metabocrates_dat"]], "removed")[["QC"]])
    
    info_txt_removed <- ifelse(is.null(removed_metabolites), ".",
                               paste0(". <br/> <br/> <b>Removed metabolites: </b>",
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
    req(input[["run"]] == "Uploading data")
    
    if(nrow(attr(dat[["metabocrates_dat"]], "NA_info")[["counts"]]) == 0)
      NULL
    else
      plot_mv_types(dat[["metabocrates_dat"]])
  })
  
  output[["mv_types_plt_ui"]] <- renderUI({
    tagList(
      if(is.null(mv_types_plt_reactive()))
        create_message_box("No missing values found", type = "warning")
      else
        plot_with_button_UI("mv_types_plt")
    )
  })
  
  outputOptions(output, "mv_types_plt_ui", suspendWhenHidden = FALSE)
  
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
      select(where(~ !any(is.na(.)))) %>%
      mutate(across(everything(), as.character)) %>%
      tidyr::pivot_longer(everything(),
                          names_to = "column name",
                          values_to = "value") %>%
      group_by(`column name`) %>%
      summarise(`unique levels` = n_distinct(value),
                `values` = paste0(unique(value), collapse = ", "))
    
    if(!is.null(attr(dat[["metabocrates_dat"]], "group"))){
      group_ind <- match(attr(dat[["metabocrates_dat"]], "group"),
                         dat[["group_candidates"]][["column name"]])
      
      dat[["group_candidates"]] %>%
        custom_datatable(scrollY = 300,
                         paging = TRUE,
                         selection = list(mode = "multiple",
                                          target = "row",
                                          selected = group_ind))
    }else{
      dat[["group_candidates"]] %>% 
        custom_datatable(scrollY = 300,
                         paging = TRUE,
                         selection = list(mode = "multiple",
                                          target = "row"))
    }
  })
  
  table_with_button_SERVER("group_columns", group_columns_DT)
  
  last_selected_group <- reactiveVal(NULL)
  
  observeEvent(input[["add_group"]], {
    req(dat[["metabocrates_dat"]])
    req(dat[["metabocrates_dat_group"]])
    
    if(is.null(input[["group_columns-table_rows_selected"]])){
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
      group_names <- group_candidates[["column name"]][input[["group_columns-table_rows_selected"]]]
    
      group_col_samples <- dat[["metabocrates_dat"]] %>% 
        filter(`sample type` == "Sample") %>%
        rowwise() %>%
        mutate(group_tmp = if (any(is.na(across(all_of(group_names))))) {
          NA
        } else {
          do.call(paste, c(across(all_of(group_names)), sep = ", "))
        }) %>%
        pull(group_tmp)
    
      if (!is.null(last_selected_group()) && 
          setequal(group_names, last_selected_group())) {
        req(NULL)
      }
    
      if (group_col_samples %>% is.na() %>% any()) {
        sendSweetAlert(session = session,
                       title = "Invalid group: missing group levels",
                       text = "Make sure that all samples have non-missing values in all grouping columns!",
                       type = "error")
      }
    
      if (any(table(group_col_samples) < 2)) {
        sendSweetAlert(session = session,
                       title = "Invalid group: too many levels",
                       text = "We require at least 2 observations per group level.",
                       type = "error")
      }
      
      if (length(unique(group_col_samples)) == 1) {
        sendSweetAlert(session = session,
                       title = "Single group level",
                       text = "Provided grouping results in only one unique group level.",
                       type = "warning")
      } else {
        sendSweetAlert(session = session,
                       title = "Great!",
                       text = "Grouping successfully applied!",
                       type = "success")
      }
  
      dat[["metabocrates_dat_group"]] <- add_group(dat[["metabocrates_dat_group"]], group_names)
    
      last_selected_group(group_names)
    }
    
    update_inputs_SERVER("group_update", session, input, dat)
  }, ignoreNULL = FALSE)
  
  observeEvent(input[["remove_group"]], {
    attr(dat[["metabocrates_dat_group"]], "group") <- NULL
    
    if(!is.null(last_selected_group())){
      last_selected_group(NULL)
      dat[["metabocrates_dat_group"]] <- unremove_all(dat[["metabocrates_dat_group"]], "LOD")
      dat[["metabocrates_dat_group"]] <- unremove_all(dat[["metabocrates_dat_group"]], "QC")
      attr(dat[["metabocrates_dat_group"]], "completed") <- NULL
    }
    
    update_inputs_SERVER("remove_group_update", session, input, dat)
  })
  
  output[["selected_info"]] <- renderUI({
    if (is.null(input[["group_columns-table_rows_selected"]])) {
     selected_columns_str <- "none"
    } else {
      selected_columns <- dat[["group_candidates"]][["column name"]][input[["group_columns-table_rows_selected"]]]
      selected_columns_str <- paste0(paste0(selected_columns, collapse = ", "))
    }
    
    tagList(
      h5(paste0("Found ", nrow(dat[["group_candidates"]]), " possible grouping columns.")),
      br(),
      br(),
      h4("Selected columns: "),
      h5(selected_columns_str)
    )
  })
  
  output[["group_info"]] <- renderUI({
    req(dat[["metabocrates_dat_group"]])
    
    if (is.null(attr(dat[["metabocrates_dat_group"]], "group")))
      group_columns <- "none"
    else
      group_columns <- paste0(paste0(attr(dat[["metabocrates_dat_group"]], "group"),
                                     collapse = ", "))
    
    tagList(
      h4("Grouping columns: "),
      h5(group_columns)
    )
  })
  
  output[["grouping_columns_summary"]] <- renderUI({
    req(dat[["metabocrates_dat_group"]])
    
    if (is.null(attr(dat[["metabocrates_dat_group"]], "group"))){
      group_columns <- "none"
      total_num <- 0
    }
    else{
      group_columns <- paste0(paste0(attr(dat[["metabocrates_dat_group"]], "group"),
                                     collapse = ",\n"))
      total_num <- length(attr(dat[["metabocrates_dat_group"]], "group"))
    }
    
    tagList(
      h4("Grouping columns: "),
      h5(group_columns),
      br(),
      br(),
      h5(paste0("Total number of unique levels: ", total_num))
    )
  })
  
  group_table_DT <- reactive({
    req(attr(dat[["metabocrates_dat_group"]], "group"))
    req(input[["group_table_column"]])
    
    dat[["metabocrates_dat_group"]] %>%
      filter(`sample type` == "Sample") %>%
      group_by(across(all_of(input[["group_table_column"]]))) %>%
      summarise(Count = n()) %>%
      custom_datatable(scrollY = 300, paging = TRUE)
  })
  
  table_with_button_SERVER("group_table", group_table_DT)
  
  groups_plt_reactive <- reactive({
    req(attr(dat[["metabocrates_dat_group"]], "group"))
    
    plot_groups(dat[["metabocrates_dat_group"]])
  })
  
  plot_with_button_SERVER("groups_plt", groups_plt_reactive) 
  
  output[["groups_plt_ui"]] <- renderUI({
    req(dat[["metabocrates_dat_group"]])
    req(input[["run"]] == "Group selection")
    
    if(is.null(attr(dat[["metabocrates_dat_group"]], "group")))
      column(12,
             create_message_box("Apply grouping to see the summary",
                                type = "warning")
      )
    else
      tagList(
        column(4, offset = 1,
               selectInput("group_table_column",
                           label = "Select grouping column",
                           choices = attr(dat[["metabocrates_dat_group"]], "group")),
               table_with_button_UI("group_table")
        ),
        column(6,
               br(),
               br(),
               br(),
               plot_with_button_UI("groups_plt") 
        )
      )
  })
  
  outputOptions(output, "groups_plt_ui", suspendWhenHidden = FALSE)
  
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
      HTML("none")
    else {
      HTML(paste0(ro_remove_display, collapse = ", "))
    }
  })
  
  
  output[["LOD_removed_txt"]] <- renderUI({
    req(dat[["metabocrates_dat_group"]])
    
    removed <- attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]]
    
    if(length(removed) == 0)
      HTML("none")
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
    
    NA_table_type <- ifelse(is.null(attr(dat[["metabocrates_dat_group"]], "group")),
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
                   all_of(setNames("grouping_column",
                                   paste0(attr(dat[["metabocrates_dat_group"]], "group"), collapse = ", "))))
      
      dt %>% custom_datatable(scrollY = 300,
                              paging = TRUE)
    
  })
  
  table_with_button_SERVER("NA_ratios_tbl", NA_ratios_tbl)
  
  NA_ratios_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["NA_percent_plt_type"]])
    
    metabo_to_display <- attr(dat[["metabocrates_dat_group"]], "NA_info")[["NA_ratios_type"]] %>% 
      filter(!(metabolite %in% c(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]],
                                 attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]]))) %>%
      filter(NA_frac != 0) %>%
      nrow()
    
    if(metabo_to_display == 0)
      NULL 
    else
      plot_NA_percent(dat[["metabocrates_dat_group"]], 
                      type = input[["NA_percent_plt_type"]])
  })
  
  NA_ratios_plt_full <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["NA_percent_plt_type"]])
    req(input[["run"]] == "Filtering")
    
    plot_NA_percent(dat[["metabocrates_dat_group"]], 
                    type = input[["NA_percent_plt_type"]],
                    interactive = FALSE)
  })
  
  output[["NA_ratios_plt_ui"]] <- renderUI({
    if(is.null(NA_ratios_plt()))
      create_message_box("No missing values found", type = "warning")
    else
      plot_with_button_UI("NA_ratios_plt")
  })
  
  outputOptions(output, "NA_ratios_plt_ui", suspendWhenHidden = FALSE)
  
  plot_with_button_SERVER("NA_ratios_plt", NA_ratios_plt,
                          full_plt = NA_ratios_plt_full)
  
  venn_plt <- reactive({
    req(input[["filtering_threshold"]])
    req(dat[["metabocrates_dat_group"]])
    if(is.null(attr(dat[["metabocrates_dat_group"]], "group"))) return(NULL)
    
    group_len <- dat[["metabocrates_dat_group"]] %>%
      filter(`sample type` == "Sample") %>%
      select(all_of(attr(dat[["metabocrates_dat_group"]], "group"))) %>%
      unlist() %>%
      unique() %>%
      length()
    
    if(!(group_len %in% 2:4))
      return(NULL)
    
    create_venn_diagram(dat[["metabocrates_dat_group"]], input[["filtering_threshold"]]/100)
  })
  
  output[["venn_diagram_ui"]] <- renderUI({
    tagList(
      br(),
      if(is.null(venn_plt())){
        column(12,
               create_message_box("Provide a group with more than 1 and up to 4 levels to see Venn diagram",
                                  type = "warning")
        )
      }
      else{
        column(12,
               br(),
               create_message_box(
                 "This Venn diagram illustrates how many metabolites have missing
                  value ratios above the threshold across the different group levels.",
                 type = "description"
               ),
               br()
        )
      },
      if(!is.null(venn_plt())){
        column(10, offset = 1,
          plot_with_button_UI("venn_diagram")
        )
      }
    )
  })
  
  plot_with_button_SERVER("venn_diagram", venn_plt)
  
  
  
  ######### imputation
  
  LOD_tbl_reactive <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    attr(dat[["metabocrates_dat_group"]], "LOD_table") %>%
      select(-c(type,
                attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]],
                attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]])) %>%
      mutate(across(everything(), display_short)) %>%
      custom_datatable(scrollY = 300,
                       paging = TRUE)
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
                 plate_bar_code = input[["pb_codes_heatmap"]],
                 show_colors = input[["missing_heatmap_colors"]])
  })
  
  missing_heatmap_height <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    length(attr(dat[["metabocrates_dat_group"]], "metabolites"))
  })
  
  plot_with_button_SERVER("missing_heatmap", missing_heatmap,
                          missing_heatmap_height)
  
  
  dist_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    if(is.null(attr(dat[["metabocrates_dat_group"]], "completed")))
      NULL
    else
      switch(input[["dist_plt_type"]],
             "Histogram" = create_distribution_plot(dat[["metabocrates_dat_group"]],
                                                    input[["sing_metabo_dist"]],
                                                    histogram_type = ifelse(input[["hist_type"]],
                                                                            "imputed",
                                                                            "all"),
                                                    bins = input[["hist_bins"]]),
             "Density" = create_distribution_plot(dat[["metabocrates_dat_group"]],
                                                  input[["sing_metabo_dist"]],
                                                  type = "density"),
             "Beeswarm" = create_distribution_plot(dat[["metabocrates_dat_group"]],
                                                   input[["sing_metabo_dist"]],
                                                   type = "beeswarm_interactive"),
             "Boxplot" = create_boxplot(dat[["metabocrates_dat_group"]],
                                        input[["sing_metabo_dist"]]),
             "Theoretical Q-Q plot" = create_qqplot(dat[["metabocrates_dat_group"]],
                                                    input[["sing_metabo_dist"]]),
             "Empirical Q-Q plot" = create_empirical_qqplot(dat[["metabocrates_dat_group"]],
                                                             input[["sing_metabo_dist"]])
      )
  })
  
  full_dist_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(attr(dat[["metabocrates_dat_group"]], "completed"))
    req(input[["sing_metabo_dist"]])
    
    if(input[["dist_plt_type"]] == "Beeswarm")
      create_distribution_plot(dat[["metabocrates_dat_group"]],
                               input[["sing_metabo_dist"]],
                               type = "beeswarm")
    else
      NULL
  })
  
  output[["dist_plt_ui"]] <- renderUI({
    req(input[["run"]] == "Completing")
    req(input[["dist_plt_type"]])
    
    if(is.null(dist_plt()))
      column(9,
             create_message_box("Complete data to see plots", type = "warning")
      )
    else
      column(9,
             br(),
             plot_with_button_UI(substr(input[["dist_plt_type"]], 1, 7))
      )
  })
  
  observe({
    if(input[["dist_plt_type"]] == "Beeswarm")
      plot_with_button_SERVER(substr(input[["dist_plt_type"]], 1, 7), dist_plt,
                              full_plt = full_dist_plt)
    else
      plot_with_button_SERVER(substr(input[["dist_plt_type"]], 1, 7), dist_plt)
  })
  
  ######## Quality control
  
  observeEvent(input[["run"]], {
    if(input[["run"]] %in% c("Quality control", "Outlier detection")){
      if(is.null(dat[["metabocrates_dat_group"]])){
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
                       - ULOQ method: <b>none</b>.<br>
                       You can go back anytime to modify the imputation."),
                       type = "warning",
                       html = TRUE)
        
        updateSelectInput(session, "LOD_method",
                          selected = "halfmin")
        
        updateSelectInput(session, "LLOQ_method",
                          selected = "limit")
        
        updateSelectInput(session, "ULOQ_method",
                          selected = "none")
        
        updateSelectInput(session, "LOD_type",
                          selected = "calc")
        
        dat[["metabocrates_dat_group"]] <-
          complete_data(dat[["metabocrates_dat_group"]],
                        LOD_method = "halfmin",
                        LLOQ_method = "limit",
                        ULOQ_method = "third quartile",
                        LOD_type = "calc")
        
        outputOptions(output, "dist_plt_ui", suspendWhenHidden = FALSE)
        
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
      HTML("none")
    else {
      HTML(paste0(to_remove_CV_display, collapse = ", "))
    }
  })
  
  output[["CV_removed_txt"]] <- renderUI({
    req(dat[["metabocrates_dat_group"]])
    
    removed <- attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]]
    
    if(length(removed) == 0)
      HTML("none")
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
  
  ######### Outlier detection
  
  sample_type_PCA_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["sample_type_PCA_type"]])
    req(length(setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                       unlist(attr(dat[["metabocrates_dat_group"]], "removed")))) > 1)
    
    if(input[["sample_type_PCA_type"]] == "variance") req(NULL)
    if(input[["sample_type_PCA_type"]] == "biplot") req(input[["sample_type_PCA_threshold"]])
    
    if(input[["sample_type_PCA_type"]] == "scatterplot" && is.null(input[["sample_type_PCA_types"]]))
      NULL
    else{
      if(input[["sample_type_PCA_type"]] == "scatterplot")
        types_to_display <- input[["sample_type_PCA_types"]]
      else types_to_display <- "all"
      
      create_PCA_plot(dat[["metabocrates_dat_group"]],
                      group_by = "sample_type",
                      type = input[["sample_type_PCA_type"]],
                      types_to_display = input[["sample_type_PCA_types"]],
                      threshold = input[["sample_type_PCA_threshold"]]/100)
    }
  })
  
  sample_type_PCA_plt_full <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["sample_type_PCA_type"]])
    req(length(setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                       unlist(attr(dat[["metabocrates_dat_group"]], "removed")))) > 1)
    if(input[["sample_type_PCA_type"]] == "biplot") req(input[["sample_type_PCA_threshold"]])
    if(input[["sample_type_PCA_type"]] == "scatterplot") req(input[["sample_type_PCA_types"]])
    
    create_PCA_plot(dat[["metabocrates_dat_group"]],
                    group_by = "sample_type",
                    type = input[["sample_type_PCA_type"]],
                    types_to_display = input[["sample_type_PCA_types"]],
                    threshold = input[["sample_type_PCA_threshold"]]/100,
                    interactive = FALSE
    )
  })
  
  plot_with_button_SERVER("sample_type_PCA_plt", sample_type_PCA_plt, full_plt = sample_type_PCA_plt_full)
  
  sample_type_PCA_variance_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["sample_type_PCA_type"]])
    if(input[["sample_type_PCA_type"]] != "variance") req(NULL)
    req(length(setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                       unlist(attr(dat[["metabocrates_dat_group"]], "removed")))) > 1)
    
    pca_variance(dat[["metabocrates_dat_group"]],
                 group_by = "sample_type",
                 threshold = input[["sample_type_PCA_variance_threshold"]]/100,
                 max_num = input[["sample_type_PCA_variance_max_num"]],
                 cumulative = input[["sample_type_PCA_variance_cum"]])
  })
  
  plot_with_button_SERVER("sample_type_PCA_variance_plt", sample_type_PCA_variance_plt)
  
  output[["sample_type_cond_pca_plt"]] <- renderUI({
    if(input[["sample_type_PCA_type"]] == "variance"){
      plot_with_button_UI("sample_type_PCA_variance_plt")
    }else{
      plot_with_button_UI("sample_type_PCA_plt")
    }
  })
  
  group_PCA_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["group_PCA_type"]])
    req(length(setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                       unlist(attr(dat[["metabocrates_dat_group"]], "removed")))) > 1)
    
    if(input[["group_PCA_type"]] == "variance") req(NULL)
    if(input[["group_PCA_type"]] == "biplot") req(input[["group_PCA_threshold"]])
      
      create_PCA_plot(dat[["metabocrates_dat_group"]],
                      group_by = "group",
                      type = input[["group_PCA_type"]],
                      threshold = input[["group_PCA_threshold"]]/100)
  })
  
  group_PCA_plt_full <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["group_PCA_type"]])
    req(length(setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                       unlist(attr(dat[["metabocrates_dat_group"]], "removed")))) > 1)
    
    if(input[["group_PCA_type"]] == "biplot") req(input[["group_PCA_threshold"]])
    if(input[["group_PCA_type"]] == "scatterplot") req(input[["group_PCA_types"]])
    
    create_PCA_plot(dat[["metabocrates_dat_group"]],
                    group_by = "group",
                    type = input[["group_PCA_type"]],
                    threshold = input[["group_PCA_threshold"]]/100,
                    interactive = FALSE
    )
  })
  
  plot_with_button_SERVER("group_PCA_plt", group_PCA_plt, full_plt = group_PCA_plt_full)
  
  group_PCA_variance_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["group_PCA_type"]])
    if(input[["group_PCA_type"]] != "variance") req(NULL)
    req(length(setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                       unlist(attr(dat[["metabocrates_dat_group"]], "removed")))) > 1)
    
    pca_variance(dat[["metabocrates_dat_group"]],
                 group_by = "group",
                 threshold = input[["group_PCA_variance_threshold"]]/100,
                 max_num = input[["group_PCA_variance_max_num"]],
                 cumulative = input[["group_PCA_variance_cum"]])
  })
  
  plot_with_button_SERVER("group_PCA_variance_plt", group_PCA_variance_plt)
  
  output[["group_cond_pca_plt"]] <- renderUI({
    if(is.null(attr(dat[["metabocrates_dat_group"]], "group")))
      column(9,
             create_message_box("Group data to see plots", type = "warning") 
      )
    else{
      if(input[["group_PCA_type"]] == "variance"){
        column(7, offset = 1,
               plot_with_button_UI("group_PCA_variance_plt")
        )
      }else if(input[["group_PCA_type"]] == "biplot"){
        tagList(
          column(9,
                 create_message_box("Biplot shows metabolite contributions to principal components,
                                    highlighting groups with similar correlation patterns.",
                                    type = "description")
          ),
          br(),
          column(7, offset = 1,
                 plot_with_button_UI("group_PCA_plt")
          )
        )
      }else{
        column(7, offset = 1,
               plot_with_button_UI("group_PCA_plt")
        )
      }
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
      "<br><br><b>Grouping columns: </b>",
      ifelse(is.null(attr(dat[["metabocrates_dat_group"]], "group")),
             "none",
             paste0(paste0(attr(dat[["metabocrates_dat_group"]], "group"),
                           collapse = ', '),
                    "<br><b>Levels:</b><br>",
                    paste0("<span style='margin-left: 2em;'>",
                           sort(unique(attr(dat[["metabocrates_dat_group"]], "NA_info")[["NA_ratios_group"]][["grouping_column"]])),
                           "</span>",
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

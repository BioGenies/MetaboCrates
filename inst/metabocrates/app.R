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
source("app_supplementary/plot_with_button_module.R")
source("app_supplementary/table_with_button_module.R")

panels_vec <- c("About", "Uploading data", "Group selection",
                "Filtering", "Completing",  "Quality control", "Summary", 
                "Download")


ui <- navbarPage(
  id = "main",
  
  includeCSS("www/style.css"),
  
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
                      style = "background-color:#f8f5f0; border-right: 1px solid",
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
               br(),
               
               h3("Selected:"),
               htmlOutput("selected_group"),
               
               column(6,
                      br(),
                      table_with_button_UI("group_columns")
               ),
               column(4, offset = 1,
                      br(),
                      plot_with_button_UI("groups_plt")
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
               column(8,
                      column(6, 
                             h2("Clean your data here."),
                      ),
                      column(6, align = "right", 
                             h2("Compounds filtering (step 3/7)"),
                             h3("next: Completing")
                      )
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
                                 br(),
                                 h4("Provide a group with up to 5 levels to see Venn diagram."),
                                 br(),
                                 plot_with_button_UI("venn_diagram")
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
                        label = "< LOD imputation method.",
                        choices = c("halfmin", "random", "halflimit", "limit", "limit-0.2min", "none"),
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
                                                  br(),
                                                  plot_with_button_UI("missing_heatmap")
                                         ),
                                         tabPanel("Single metabolite distribution",
                                                  br(),
                                                  column(5,
                                                         selectInput("sing_metabo_dist",
                                                                     "Metabolite",
                                                                     choices = character(0))
                                                  ),
                                                  column(5, offset = 1,
                                                         radioButtons("dist_plt_type",
                                                                      "Plot type",
                                                                      choices = c("Histogram", "Density", "Boxplot", "Q-Q plot"),
                                                                      inline = TRUE)),
                                                  br(),
                                                  column(10, offset = 1,
                                                         plot_with_button_UI("dist_plt")
                                                  )
                                         ),
                                         tabPanel("Correlations heatmap",
                                                  br(),
                                                  br(),
                                                  plot_with_button_UI("corr_heatmap"))
                             ),
                      )
               )
               
      ),
      #################
      tabPanel("Quality control",
               nav_btns_UI("Quality control"),
               column(4,
                      style = "background-color:#f8f5f0; border-right: 1px solid",
                      br(),
                      h4("Provide threshold."),
                      h5("All metabolites for which the coefficient of variation
                         value is greater than provided threshold will be
                         removed."),
                      br(),
                      numericInput(
                        inputId = "cv_threshold",
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
                      br(),
                      column(12, htmlOutput("CV_to_remove_txt")),
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
                      column(2, align = "center", offset = 2,
                             actionButton("CV_undo_btn", label = "Undo")),
                      br()
                      
               ),
               column(8,
                      column(12, align = "right", 
                             h2("Quality control (step 5/7)"),
                             h3("next: Summary")
                      )
               ),
               column(8,
                      tabsetPanel(
                        tabPanel("CV table",
                                 column(10, offset = 1,
                                        br(),
                                        br(),
                                        table_with_button_UI("CV_tbl"))
                        ),
                        tabPanel("PCA",
                                 column(4, offset = 1,
                                        br(),
                                        radioButtons("PCA_type",
                                                     label = "Select PCA plot type:",
                                                     choices = c("sample type",
                                                                 "group", "biplot"))
                                 ),
                                 column(4, offset = 1,
                                        br(),
                                        numericInput(
                                          inputId = "PCA_threshold",
                                          label = "Threshold for biplot [%]:",
                                          value = 30,
                                          min = 0,
                                          max = 100)
                                 ),
                                 column(10, offset = 1,
                                        br(),
                                        plot_with_button_UI("PCA_plt")  
                                 )
                        ),
                        tabPanel("Variance explained",
                                 column(10, offset = 1,
                                        br(),
                                        br(),
                                        plot_with_button_UI("PCA_variance")
                                 )
                        ),
                        tabPanel("Two metabolites plot",
                                 column(10, offset = 1,
                                        br(),
                                        column(5,
                                               selectInput("2_metabo_plt_1",
                                                           "First metabolite",
                                                           choices = character(0))
                                        ),
                                        column(5,
                                               selectInput("2_metabo_plt_2",
                                                           "Second metabolite",
                                                           choices = character(0))
                                        ),
                                        br(),
                                        plot_with_button_UI("2_metabo_plt")
                                 )
                        )
                      )
               )
      ),
      #######
      tabPanel("Summary",
               nav_btns_UI("Summary"),
               column(8, 
                      fluidRow(
                        column(5, offset = 1,
                               h3("Metabolites removed based on the")
                        ),
                        column(4, offset = 2,
                               h3("Analysis summary")
                        )
                      ),
                      fluidRow(
                        column(3, offset = 1,
                               h4("Limit of detection"),
                               tags$div(
                                 style = "height: 350px; overflow-y: scroll; overflow-x: scroll;
                                 border: 1px solid #ccc; padding: 11px;",
                                 htmlOutput("summary_LOD_removed_txt")
                               ),
                               htmlOutput("LOD_threshold_txt"),
                               htmlOutput("LOD_count_txt")
                        ),
                        column(3,
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
               ),
               column(4, align = "right", 
                      h2("Summary (step 6/7)"),
                      h3("next: Download")
               )
      )
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
    
    updateMultiInput(session, "LOD_to_remove", 
                     choices = attr(dat[["metabocrates_dat"]], "metabolites"))
    
    dat_LOD_type <- attr(dat[["metabocrates_dat"]], "LOD_table")[["type"]]
    
    aval_LOD_types <- c("calc", "OP")[c(any(grepl("calc.", dat_LOD_type)),
                                        any(grepl("OP", dat_LOD_type)))]
    
    updateSelectInput(session, "LOD_type", choices = aval_LOD_types)
    
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
  
  
  biocrates_matrix_reactive <- reactive({
    req(dat[["metabocrates_dat"]])
    
    metabolites <- attr(dat[["metabocrates_dat"]], "metabolites")
    
    dat[["metabocrates_dat"]] %>% 
      select(`sample type`, all_of(metabolites)) %>% 
      mutate_all(as.character) %>% 
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
    
    dat[["group_candidates"]] <- dat[["metabocrates_dat"]] %>% 
      select(!all_of(attr(dat[["metabocrates_dat"]], "metabolites"))) %>% 
      select(-any_of(c("plate bar code", "sample bar code", "collection date",
                       "sample identification", "op", "org. info", "plate note",
                       "plate production no.", "well position", "sample volume", 
                       "run number", "injection number", "measurement time"))) %>% 
      filter(`sample type` == "Sample") %>% 
      select(-`sample type`, -`species`)
    
    dat[["group_candidates"]] %>% 
      custom_datatable(scrollY = 300,
                       paging = TRUE,
                       selection = list(mode = "single", target = "column"))
  })
  
  
  table_with_button_SERVER("group_columns", group_columns_DT)
  
  
  output[["selected_group"]] <- renderUI({
    req(dat[["metabocrates_dat"]])
    
    dat[["metabocrates_dat_group"]] <- dat[["metabocrates_dat"]]
    
    if(!is.null(input[["group_columns-table_columns_selected"]])) {
      
      group_candidates <- dat[["group_candidates"]]
      
      group_name <- colnames(group_candidates)[input[["group_columns-table_columns_selected"]] + 1]
      
      group_col_samples <- dat[["metabocrates_dat"]] %>%  
        filter(`sample type` == "Sample") %>% 
        pull(group_name)
      
      if(group_col_samples %>%  is.na() %>%  any()) {
        sendSweetAlert(session = session,
                       title = "Invalid group: missing group labels",
                       text = "Make sure that all samples have non-missing group name!",
                       type = "error")
        req(NULL)
      }
      if(any(table(group_col_samples) < 2)) {
        sendSweetAlert(session = session,
                       title = "Invalid group: too many groups",
                       text = "We require at least 2 obsevations per group.",
                       type = "error")
        req(NULL)
      }
      
      if(length(unique(group_col_samples)) == 1) {
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
      
      dat[["metabocrates_dat_group"]] <- add_group(dat[["metabocrates_dat"]], 
                                                   group_name)
      
      updateRadioButtons(session, inputId = "NA_percent_plt_type",
                         choiceValues = c("joint", "NA_type", "group"),
                         choiceNames = c("Joint ratios", "Show NA type", "Show groups"),
                         inline = TRUE)
      
      group_name <- HTML(
        paste0(group_name, ", <br/> 
               Levels: ", paste0(sort(unique(group_col_samples)), collapse = ", "))
      )
    } else {
      req(NULL)
    }
    
    HTML(group_name)
  })
  
  
  groups_plt_reactive <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    if(!is.null(attr(dat[["metabocrates_dat_group"]], "group")))
      plot_groups(dat[["metabocrates_dat_group"]])
  })
  
  
  plot_with_button_SERVER("groups_plt", groups_plt_reactive)
  
  
  ######### filtering
  
  to_remove <- reactive({
    
    req(dat[["metabocrates_dat_group"]])
    req(input[["filtering_threshold"]])
    
    to_remove_tmp <- setdiff(
      get_LOD_to_remove(dat[["metabocrates_dat_group"]], 
                        input[["filtering_threshold"]]/100), 
      attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]]
    )
    
    updateMultiInput(session, "LOD_to_remove", selected = to_remove_tmp)
    
    to_remove_tmp
  })
  
  
  output[["LOD_to_remove_txt"]] <- renderUI({
    
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
    metabolites_vec <- setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"), 
                               attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]])
    
    updateMultiInput(session, "LOD_to_remove", 
                     choices = metabolites_vec)
  })
  
  
  observeEvent(input[["LOD_undo_btn"]], {
    req(dat[["metabocrates_dat_group"]])
    
    dat[["metabocrates_dat_group"]] <- unremove_all(dat[["metabocrates_dat_group"]],
                                                    type = "LOD")
    
    updateMultiInput(session, "LOD_to_remove", 
                     choices = attr(dat[["metabocrates_dat_group"]], "metabolites"))
  })
  
  
  NA_ratios_tbl <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    NA_table_type <- 
      ifelse(is.null(attr(dat[["metabocrates_dat_group"]], "group")), 
             "NA_ratios_type",
             "NA_ratios_group")
    
    attr(dat[["metabocrates_dat_group"]], "NA_info")[[NA_table_type]] %>% 
      filter(!(metabolite %in% attr(dat[["metabocrates_dat_group"]], 
                                    "removed")[["LOD"]])) %>% 
      arrange(-NA_frac) %>% 
      mutate(`NA fraction [%]` = round(NA_frac*100, 3)) %>%
      select(!NA_frac) %>%
      custom_datatable(scrollY = 300, paging = TRUE)
    
  })
  
  
  table_with_button_SERVER("NA_ratios_tbl", NA_ratios_tbl)
  
  NA_ratios_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    req(input[["NA_percent_plt_type"]])
    
    metabo_num <- length(attr(dat[["metabocrates_dat_group"]], "metabolites"))
    
    plot_NA_percent(dat[["metabocrates_dat_group"]], 
                    type = input[["NA_percent_plt_type"]],
                    height_svg = max(metabo_num * 22, 400)/96,
                    width_svg = 11)
    
  })
  
  plot_with_button_SERVER("NA_ratios_plt", NA_ratios_plt,
                          NA_ratios_plt_height)
  
  venn_plt <- reactive({
    req(input[["filtering_threshold"]])
    req(dat[["metabocrates_dat_group"]])
    req(attr(dat[["metabocrates_dat_group"]], "group"))
    
    if(length(na.omit(unique(unlist(dat[["metabocrates_dat_group"]][attr(dat[["metabocrates_dat_group"]], "group")])))) > 5)
      req(NULL)
    
    create_venn_diagram(dat[["metabocrates_dat_group"]], input[["filtering_threshold"]]/100)
  })
  
  plot_with_button_SERVER("venn_diagram", venn_plt)
  
  corr_heatmap_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    if(length(attr(dat[["metabocrates_dat_group"]], "metabolites")) > 10)
      create_correlations_heatmap(dat[["metabocrates_dat_group"]], num = 10,
                                  width_svg = 10, height_svg = 7)
    else
      create_correlations_heatmap(corr_dat, width_svg = 10, height_svg = 7)
  })
  
  full_corr_heatmap_plt <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    if(length(attr(dat[["metabocrates_dat_group"]], "metabolites")) > 10)
      create_correlations_heatmap(dat[["metabocrates_dat_group"]])
    else corr_heatmap_plt()
  })
  
  plot_with_button_SERVER("corr_heatmap", corr_heatmap_plt, full_plt = full_corr_heatmap_plt)
  
  ######### imputation
  
  LOD_tbl_reactive <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    attr(dat[["metabocrates_dat_group"]], "LOD_table") %>% 
      select(-type) %>% 
      custom_datatable(scrollY = 300,
                       paging = TRUE,
                       selection = list(mode = "single", target = "column"))
  })
  
  table_with_button_SERVER("LOD_tbl", LOD_tbl_reactive)
  
  completed_tbl_reactive <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    metabolites <- setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                           attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]])
    
    if(is.null(attr(dat[["metabocrates_dat_group"]], "completed"))) {
      dat_to_display <- dat[["metabocrates_dat_group"]] %>% 
        select(all_of(metabolites)) %>% 
        mutate_all(as.character) %>% 
        mutate_all(display_short)
    } else {
      dat_to_display <- attr(dat[["metabocrates_dat_group"]], "completed") %>% 
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
    
    updateSelectInput(session, inputId = "sing_metabo_dist",
                       choices =
                         attr(dat[["metabocrates_dat_group"]], "metabolites"))
    
    
    updateSelectInput(session, inputId = "2_metabo_plt_1",
                      choices = setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                                        attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]]))
    
    updateSelectInput(session, inputId = "2_metabo_plt_2",
                      choices = setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                                        attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]]))
  })
  
  observeEvent(input[["complete_undo_btn"]], {
    req(dat[["metabocrates_dat_group"]])
    
    attr(dat[["metabocrates_dat_group"]], "completed") <- NULL
    
    updateSelectInput(session, inputId = "sing_metabo_dist",
                       choices = c("None"))
    
    
    updateSelectInput(session, inputId = "2_metabo_plt_1",
                      choices = c("None"))
    
    updateSelectInput(session, inputId = "2_metabo_plt_2",
                      choices = c("None"))
  })
  
  missing_heatmap <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    plot_heatmap(dat[["metabocrates_dat_group"]])
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
                                                  input[["sing_metabo_dist"]]),
           "Density" = create_distribution_plot(dat[["metabocrates_dat_group"]],
                                                input[["sing_metabo_dist"]],
                                                type = "density"),
           "Boxplot" = create_boxplot(dat[["metabocrates_dat_group"]],
                                      input[["sing_metabo_dist"]]),
           "Q-Q plot" = create_qqplot(dat[["metabocrates_dat_group"]],
                                      input[["sing_metabo_dist"]]))
  })
  
  plot_with_button_SERVER("dist_plt", dist_plt)
  
  ######## Quality control
  
  observeEvent(input[["run"]], {
    if(input[["run"]] == "Quality control"){
      if(!is.null(attr(dat[["metabocrates_dat_group"]], "completed"))){
        dat[["metabocrates_dat_group"]] <-
          calculate_CV(dat[["metabocrates_dat_group"]])
        
        updateMultiInput(session, "CV_to_remove", 
                         choices = setdiff(
                           attr(dat[["metabocrates_dat_group"]], "metabolites"),
                           attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]]
                         ))
      }else{
        attr(dat[["metabocrates_dat_group"]], "cv") <- NULL
        attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]] <- NULL
      }
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
    
    updateMultiInput(session, "CV_to_remove", 
                     choices = setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"), 
                                       c(attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]],
                                         attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]])))
    
    
    updateSelectInput(session, inputId = "2_metabo_plt_1",
                      choices = setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"), 
                                        c(attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]],
                                          attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]])))
    
    updateSelectInput(session, inputId = "2_metabo_plt_2",
                      choices = setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"), 
                                        c(attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]],
                                          attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]])))
  })
  
  
  observeEvent(input[["CV_undo_btn"]], {
    req(dat[["metabocrates_dat_group"]])
    
    dat[["metabocrates_dat_group"]] <- unremove_all(dat[["metabocrates_dat_group"]],
                                                   type = "QC")
    
    updateMultiInput(session, "CV_to_remove", 
                     choices = attr(dat[["metabocrates_dat_group"]], "metabolites"))
    
    
    updateSelectInput(session, inputId = "2_metabo_plt_1",
                      choices = setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"), 
                                        c(attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]],
                                          attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]])))
    
    updateSelectInput(session, inputId = "2_metabo_plt_2",
                      choices = setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"), 
                                        c(attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]],
                                          attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]])))
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
    if(input[["PCA_type"]] == "biplot") req(input[["PCA_threshold"]])
    
    create_PCA_plot(dat[["metabocrates_dat_group"]],
                    type = ifelse(input[["PCA_type"]] == "sample type",
                                  "sample_type", input[["PCA_type"]]),
                    threshold = input[["PCA_threshold"]]/100)
  })
  
  plot_with_button_SERVER("PCA_plt", PCA_plt)
  
  PCA_variance <- reactive({
    req(dat[["metabocrates_dat_group"]])
    
    pca_variance(dat[["metabocrates_dat_group"]], 0.3, 5)
  })
  
  plot_with_button_SERVER("PCA_variance", PCA_variance)
  
  two_metabo_plt <- reactive({
    req(attr(dat[["metabocrates_dat_group"]], "completed"))
    
    create_plot_of_2_metabolites(dat[["metabocrates_dat_group"]],
                                 input[["2_metabo_plt_1"]],
                                 input[["2_metabo_plt_2"]])
  })
  
  plot_with_button_SERVER("2_metabo_plt", two_metabo_plt)
  
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
             attr(dat[["metabocrates_dat_group"]], "group")),
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
               "<span style='margin-left: 1em;'><b>ULOQ method: </b></span>",
               input[["ULOQ_method"]],
               "<br><br>"
             )
      ),
      "</div>"
    ))
  })
  
}

shinyApp(ui, server, options = list(launch.browser = TRUE))



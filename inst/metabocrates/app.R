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
               column(9,
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
               column(3,
                      column(12, align = "right", 
                             h2("Group selection (step 2/7)"),
                             h3("next: Filtering"),
                             br()),
               ),
               br(),
               
               column(3, 
                      h3("Selected:"),
                      htmlOutput("selected_group")
               ),
               
               column(8,
                      tabsetPanel(
                        tabPanel(
                          "Groups",
                          br(),
                          table_with_button_UI("group_columns")
                        ),
                        tabPanel(
                          "Summary",
                          br(),
                          plot_with_button_UI("groups_plt")
                        )
                      )
               )
      ),
      ####################
      tabPanel("Filtering",
               nav_btns_UI("Filtering"),
               column(3,
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
                      column(2, align = "center", 
                             actionButton("LOD_remove_btn", label = "Remove")),
                      column(2, align = "center", offset = 1,
                             actionButton("LOD_undo_btn", label = "Undo")),
                      br(),
                      br(),
                      br()
               ),
               column(9,
                      column(6, 
                             h2("Clean your data here."),
                      ),
                      column(6, align = "right", 
                             h2("Compounds filtering (step 3/7)"),
                             h3("next: Completing")
                      ),
                      tabsetPanel(
                        tabPanel("Ratios of missing values",
                                 column(10, offset = 1,
                                        br(),
                                        br(),
                                        table_with_button_UI("NA_ratios_tbl"))
                        ),
                        tabPanel("Visualization")
                      )
               )
               
      ),
      #################
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
                                        br(),
                                        table_with_button_UI("completed_tbl")),
                               tabPanel("Table of limits",
                                        br(),
                                        table_with_button_UI("LOD_tbl"))
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
    
    updateMultiInput(session, "LOD_to_remove", 
                     choices = attr(dat[["metabocrates_dat"]], "metabolites"))
    
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
    
    dat[["metabocrates_dat"]] %>% 
      select(!all_of(attr(dat[["metabocrates_dat"]], "metabolites"))) %>% 
      select(-`plate bar code`, - `sample bar code`, -`collection date`,
             -`sample identification`, -`op`, -`org. info`, -`plate note`,
             -`plate production no.`, -`well position`, -`sample volume`, 
             -`run number`, -`injection number`, -`measurement time`) %>% 
      custom_datatable(scrollY = 450,
                       paging = TRUE,
                       selection = list(mode = "single", target = "column"))
  })
  
  
  table_with_button_SERVER("group_columns", group_columns_DT)
  
  
  output[["selected_group"]] <- renderUI({
    req(dat[["metabocrates_dat"]])
    
    dat[["metabocrates_dat_group"]] <- dat[["metabocrates_dat"]]
    
    if(!is.null(input[["group_columns-table_columns_selected"]])) {
      
      group_candidates <- dat[["metabocrates_dat"]] %>% 
        select(!all_of(attr(dat[["metabocrates_dat"]], "metabolites"))) %>% 
        select(-`plate bar code`, - `sample bar code`, -`collection date`,
               -`sample identification`, -`op`, -`org. info`, -`plate note`,
               -`plate production no.`, -`well position`, -`sample volume`, 
               -`run number`, -`injection number`, -`measurement time`)
      
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
  
  output[["LOD_to_remove_txt"]] <- renderUI({
    req(dat[["metabocrates_dat"]])
    req(input[["filtering_threshold"]])
    
    to_remove <- setdiff(
      get_LOD_to_remove(attr(dat[["metabocrates_dat"]], "NA_info"), 
                        input[["filtering_threshold"]]/100), 
      attr(dat[["metabocrates_dat"]], "removed")[["LOD"]]
    )
    
    updateMultiInput(session, "LOD_to_remove", 
                     selected = to_remove)
    
    
    if(length(to_remove) == 0)
      HTML("None.")
    else {
      HTML(paste0(to_remove, collapse = ", "))
    }
  })
  
  
  observeEvent(input[["LOD_remove_btn"]], {
    req(dat[["metabocrates_dat"]])
    req(input[["LOD_to_remove"]])
    
    
    dat[["metabocrates_dat"]] <- remove_metabolites(
      dat[["metabocrates_dat"]],
      metabolites_to_remove = input[["LOD_to_remove"]],
      type = "LOD"
    )
    
    metabolites_vec <- setdiff(attr(dat[["metabocrates_dat"]], "metabolites"), 
                               attr(dat[["metabocrates_dat"]], "removed")[["LOD"]])
    
    updateMultiInput(session, "LOD_to_remove", 
                     choices = metabolites_vec)
  })
  
  
  NA_ratios_tbl <- reactive({
    req(dat[["metabocrates_dat"]])
    
    attr(dat[["metabocrates_dat"]], "NA_info")[["NA_ratios"]] %>% 
      filter(!(metabolite %in% attr(dat[["metabocrates_dat"]], "removed")[["LOD"]])) %>% 
      arrange(NA_frac) %>% 
      mutate(NA_frac = round(NA_frac, 3)) %>% 
      custom_datatable(scrollY = 400, paging = TRUE)
    
  })
  
  
  table_with_button_SERVER("NA_ratios_tbl", NA_ratios_tbl)
  
  
  
  
  ######### imputation
  
  LOD_tbl_reactive <- reactive({
    req(dat[["metabocrates_dat"]])
    
    attr(dat[["metabocrates_dat"]], "LOD_table") %>% 
      select(-type) %>% 
      custom_datatable(scrollY = 300,
                       paging = TRUE,
                       selection = list(mode = "single", target = "column"))
  })
  
  table_with_button_SERVER("LOD_tbl", LOD_tbl_reactive)
  
  
  completed_tbl_reactive <- reactive({
    req(dat[["metabocrates_dat"]])
    
    metabolites <- attr(dat[["metabocrates_dat"]], "metabolites")
    
    if(is.null(attr(dat[["metabocrates_dat"]], "completed"))) {
      dat[["metabocrates_dat"]] %>% 
        select(all_of(metabolites)) %>% 
        mutate_all(as.character) %>% 
        mutate_all(display_short) %>% 
        custom_datatable(scrollY = 400,
                         paging = TRUE)
    } else {
      attr(dat[["metabocrates_dat"]], "completed") %>% 
        select(all_of(metabolites)) %>% 
        mutate_all(as.numeric) %>% 
        mutate_all(round) %>% 
        custom_datatable(scrollY = 400,
                         paging = TRUE)
    }
  })
  table_with_button_SERVER("completed_tbl", completed_tbl_reactive)
  
  
  
  
  
}

shinyApp(ui, server)







nav_btns_UI <- function(id) {
  ns <- NS(id)
  
  if(id == "Uploading data") {
    tagList(
      tags$footer(
        align = "right",
        style = "position:absolute; bottom:0; width:95%; height:20px; padding: 0px 0px 80px 80px;",
        column(1),
        column(10),
        column(1, align = "right", 
               actionButton(ns("next"), 
                            label = "next", 
                            icon = icon("arrow-right"),
                            style='padding:15px; font-size:150%'))
      ),
    )
  }else {
    if(id == "Download") {
      tagList(
        tags$footer(
          align = "right",
          style = "position:absolute; bottom:0; width:95%; height:20px; padding: 0px 0px 80px 80px;",
          column(1, align = "left", 
                 actionButton(ns("prev"), 
                              label = "back", 
                              icon = icon("arrow-left"),
                              style='padding:15px; font-size:150%')),
          column(10),
          column(1)
        ),
      )
    }else {
      tagList(
        tags$footer(
          align = "right",
          style = "position:absolute; bottom:0; width:95%; height:20px; padding: 0px 0px 80px 80px;",
          column(1, align = "left", 
                 actionButton(ns("prev"), 
                              label = "back", 
                              icon = icon("arrow-left"),
                              style='padding:15px; font-size:150%')),
          column(10),
          column(1, align = "right", 
                 actionButton(ns("next"), 
                              label = "next", 
                              icon = icon("arrow-right"),
                              style='padding:15px; font-size:150%'))
        ),
      )
    }
  }
}


nav_btns_SERVER <- function(input, output, 
                            session, 
                            parent_session, 
                            panels_vec,
                            panel_id,
                            dat = NULL){
  
  observeEvent(input[["prev"]], {
    prev_panel <- panels_vec[which(panels_vec == panel_id) - 1]
    
    if(prev_panel == "Summary"){
      updateNavbarPage(parent_session, inputId = "main", selected = "Analysis")
    }
    updateTabsetPanel(parent_session, inputId = "run", selected = prev_panel)
  })
  
  observeEvent(input[["next"]], {
    next_panel <- panels_vec[which(panels_vec == panel_id) + 1]
    
    if(next_panel == "Download"){
      updateNavbarPage(parent_session, inputId = "main", selected = "Download")
    }else if(next_panel == "Quality control" &&
             is.null(attr(dat[["metabocrates_dat_group"]], "completed"))){
      sendSweetAlert(parent_session,
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
      
      updateSelectInput(parent_session, "LOD_method",
                        selected = "halfmin")
      
      updateSelectInput(parent_session, "LLOQ_method",
                        selected = "limit")
      
      updateSelectInput(parent_session, "ULOQ_method",
                        selected = "third quartile")
      
      updateSelectInput(parent_session, "LOD_type",
                        selected = "calc")
      
      dat[["metabocrates_dat_group"]] <-
        complete_data(dat[["metabocrates_dat_group"]],
                      LOD_method = "halfmin",
                      LLOQ_method = "limit",
                      ULOQ_method = "third quartile",
                      LOD_type = "calc")
      
      update_inputs_SERVER("complete_update", parent_session, input, dat)
    }else{
      updateTabsetPanel(parent_session, inputId = "run", selected = next_panel)
    }
  })
}


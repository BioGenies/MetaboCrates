

nav_btns_UI <- function(id) {
  ns <- NS(id)
  
  footer_style <- "position:fixed; bottom:0; width:95%; height:20px;
                   padding: 0px 0px 80px 80px; z-index: 9999"
  
  if(id == "Uploading data") {
    tagList(
      tags$footer(
        align = "right",
        style = footer_style,
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
          style = footer_style,
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
          style = footer_style,
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
    }else{
      updateTabsetPanel(parent_session, inputId = "run", selected = next_panel)
    }
  })
}


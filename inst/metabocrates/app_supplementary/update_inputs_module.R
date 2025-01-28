update_inputs_UI <- function(id) {
  ns <- NS(id)
  tagList()
}

update_inputs_SERVER <- function(id, main_session, dat) {
  moduleServer(id, function(input, output, session){
    observe({
      req(dat[["metabocrates_dat_group"]])
      
      metabolites <- setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                             c(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]],
                               attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]]))
      
      if(id == "complete_undo_update"){
        updateSelectInput(main_session, inputId = "sing_metabo_dist",
                          choices = c("None"))
        
        updateSelectInput(main_session, inputId = "2_metabo_plt_1",
                          choices = c("None"))
        
        updateSelectInput(main_session, inputId = "2_metabo_plt_2",
                          choices = c("None"))
        
        updatePickerInput(main_session, inputId = "corr_heatmap_metabolites",
                          choices = c("None"))
      }else if(id == "cv_update"){
        updateMultiInput(main_session, "CV_to_remove", 
                         choices = metabolites)
      }else{
        updateMultiInput(main_session, "LOD_to_remove", choices = metabolites)
        
        if(!is.null(attr(dat[["metabocrates_dat_group"]], "cv"))){
          updateMultiInput(main_session, "CV_to_remove", 
                           choices = metabolites)
        }
        
        if(!is.null(attr(dat[["metabocrates_dat_group"]], "completed"))){
          updateNumericInput(main_session, inputId = "PCA_variance_max_num",
                             max = length(metabolites),
                             value = min(c(5, length(metabolites))))
          
          updateSelectInput(main_session, inputId = "sing_metabo_dist",
                            choices = metabolites)
          
          updateSelectInput(main_session, inputId = "2_metabo_plt_1",
                            choices = metabolites)
          
          updateSelectInput(main_session, inputId = "2_metabo_plt_2",
                            choices = setdiff(metabolites,
                                              input[["2_metabo_plt_1"]]))
          
          uncomplete_metabolites <- attr(dat[["metabocrates_dat_group"]], "completed") %>%
            filter(`sample type` == "Sample") %>%
            select(all_of(metabolites)) %>%
            select(where(~ is.na(sd(., na.rm = TRUE)) | sd(., na.rm = TRUE) == 0))
          
          updatePickerInput(main_session, inputId = "corr_heatmap_metabolites",
                            choices = setdiff(metabolites,
                                              names(uncomplete_metabolites)),
                            selected = setdiff(metabolites,
                                               names(uncomplete_metabolites)),
                            choicesOpt = list(
                              style = rep("color: black;", length(metabolites))
                            ))
        }
      }
    })
  })
}

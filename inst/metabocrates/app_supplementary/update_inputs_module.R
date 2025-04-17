update_inputs_SERVER <- function(id, main_session, main_input, dat){
    metabolites <- reactive({
      req(dat[["metabocrates_dat_group"]])
      
      metabolites <- setdiff(attr(dat[["metabocrates_dat_group"]], "metabolites"),
                             c(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]],
                               attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]]))
    })
    
    LOD_to_remove <- reactive({
      req(dat[["metabocrates_dat_group"]])
      req(main_input[["filtering_threshold"]])
      
      setdiff(
        get_LOD_to_remove(dat[["metabocrates_dat_group"]],
                          main_input[["filtering_threshold"]]/100),
        c(attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]],
          attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]])
      )
    })
    
    CV_to_remove <- reactive({
      req(attr(dat[["metabocrates_dat_group"]], "cv"))
      
      setdiff(
        get_CV_to_remove(dat[["metabocrates_dat_group"]],
                         main_input[["cv_threshold"]]/100),
        c(attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]],
          attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]])
      )
    })
    
    if(id == "initial"){
      dat_LOD_type <- attr(dat[["metabocrates_dat"]], "LOD_table")[["type"]]
      
      aval_LOD_types <- c("calc", "OP")[c(any(grepl("calc.", dat_LOD_type)),
                                          any(grepl("OP", dat_LOD_type)))]
      
      aval_pb_codes <- dat[["metabocrates_dat"]] %>%
        filter(`sample type` == "Sample") %>%
        select(`plate bar code`) %>%
        unlist() %>%
        unique()
      
      updateSelectInput(main_session, "LOD_type", choices = aval_LOD_types)
      updateSelectInput(main_session, "pb_codes_heatmap", choices = aval_pb_codes)
    }
    else if(id == "complete_undo_update"){
      updateSelectInput(main_session, inputId = "sing_metabo_dist",
                        choices = c("None"))
        
      updateSelectInput(main_session, inputId = "2_metabo_plt_1",
                        choices = c("None"))
        
      updateSelectInput(main_session, inputId = "2_metabo_plt_2",
                        choices = c("None"))
        
      updatePickerInput(main_session, inputId = "corr_heatmap_metabolites",
                        choices = c("None"))
      
      attr(dat[["metabocrates_dat_group"]], "cv") <- NULL
    }else if(id == "cv_update"){
      updateMultiInput(main_session, "CV_to_remove", 
                       choices = metabolites(), 
                       selected = CV_to_remove())
    }else{
      if(id == "group_update"){
        if(!is.null(attr(dat[["metabocrates_dat_group"]], "group"))){
          pca_choices <- c("sample type", "group", "biplot", "variance")
          na_choice_vals <- c("joint", "NA_type", "group")
          na_choice_names <- c("Joint ratios", "Show NA type", "Show groups")
        }else{
          pca_choices <- c("sample type", "biplot", "variance")
          na_choice_vals <- c("joint", "NA_type")
          na_choice_names <- c("Joint ratios", "Show NA type")
        }
          
        updateRadioButtons(main_session, inputId = "PCA_type",
                           choices = pca_choices,
                           inline = TRUE)
        
        updateRadioButtons(main_session, inputId = "NA_percent_plt_type",
                           choiceValues = na_choice_vals,
                           choiceNames = na_choice_names,
                           inline = TRUE)
      }
        
      updateMultiInput(main_session, "LOD_to_remove", choices = metabolites())
        
      if(!is.null(attr(dat[["metabocrates_dat_group"]], "cv"))){
        updateMultiInput(main_session, "CV_to_remove",
                         choices = metabolites(),
                         selected = CV_to_remove())
      }
        
      if(!is.null(attr(dat[["metabocrates_dat_group"]], "completed"))){
        updateNumericInput(main_session, inputId = "PCA_variance_max_num",
                           max = length(metabolites()),
                           value = min(c(5, length(metabolites()))))
          
        updateSelectInput(main_session, inputId = "sing_metabo_dist",
                          choices = metabolites())
          
        updateSelectInput(main_session, inputId = "2_metabo_plt_1",
                          choices = metabolites()[-2])
          
        updateSelectInput(main_session, inputId = "2_metabo_plt_2",
                          choices = metabolites()[-1])
          
        uncomplete_metabolites <- reactive({
          req(dat[["metabocrates_dat_group"]])
            
          attr(dat[["metabocrates_dat_group"]], "completed") %>%
          filter(`sample type` == "Sample") %>%
          select(all_of(metabolites())) %>%
          select(where(~ is.na(sd(., na.rm = TRUE)) | sd(., na.rm = TRUE) == 0))
        })
          
        updatePickerInput(main_session, inputId = "corr_heatmap_metabolites",
                          choices = setdiff(metabolites(),
                                            names(uncomplete_metabolites())),
                          selected = setdiff(metabolites(),
                                             names(uncomplete_metabolites())),
                          choicesOpt = list(
                            style = rep("color: black;", length(metabolites()))
                          ))
          
        if(is.null(main_input[["PCA_types"]])){
          types <- attr(dat[["metabocrates_dat_group"]], "completed") %>%
            select(all_of(c(attr(dat[["metabocrates_dat_group"]], "metabolites"), "tmp_id"))) %>%
            select(where(~ n_distinct(na.omit(.)) > 1)) %>%
            na.omit() %>%
            select(where(~ n_distinct(.) > 1)) %>%
            left_join(select(attr(dat[["metabocrates_dat_group"]], "completed"),
                             all_of(c("tmp_id", "sample type")))) %>%
            select("sample type") %>%
            unlist()
          
          updateCheckboxGroupInput(main_session, inputId = "PCA_types",
                            choices = unique(types),
                            selected = unique(types))
        }
      }
    }
}

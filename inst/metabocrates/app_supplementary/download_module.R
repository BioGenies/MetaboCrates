download_UI <- function(id){
  ns <- NS(id)
  
  txt <- switch(id,
                "download_rds" = "Download project in rds file",
                "download_matrix" = "Download metabolomics matrix in xlsx file",
                "download_tables" = "Download project in xlsx file",
                "download_zip" = "Download plots in zip file",
                "download_pdf" = "Download report in pdf file")
  
  column(10, offset = 1,
         br(),
         div(
           style = "background-color: white; color: black; padding: 10px; border-radius: 5px; 
                  border: 2px solid black; display: flex; align-items: center; justify-content: space-between;",
           h4(
             span(txt, style = "margin-right: 20px;") %>%
               helper(id = "download_helper"),
             style = "margin: 0;"
           ),
           downloadButton(ns("download"), "Download")
         )
  )
  
  
  
}

download_SERVER <- function(id, dat){
  moduleServer(id, function(input, output, session){
    if(id == "download_rds"){
      output[["download"]] <- downloadHandler(
        filename = "project.rds",
        content = function(file) saveRDS(dat[["metabocrates_dat_group"]], file)
      )
    }else if(id == "download_matrix"){
      output[["download"]] <- downloadHandler(
        filename = "metabolomics_matrix.xlsx",
        content = function(file){
          wb_file <- createWorkbook()
          
          if(is.null(attr(dat[["metabocrates_dat_group"]], "completed")))
            metabo_tab <- dat[["metabocrates_dat_group"]] %>%
              select(!c(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]],
                        attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]]))
          else
            metabo_tab <- attr(dat[["metabocrates_dat_group"]], "completed") %>%
              select(!c(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]],
                        attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]]))
          
          addWorksheet(wb_file, "metabolites")
          writeData(wb_file, "metabolites", metabo_tab)
          saveWorkbook(wb_file, file, overwrite = TRUE)
        }
      )
    }else if(id == "download_tables"){
      output[["download"]] <- downloadHandler(
        filename = "project.xlsx",
        content = function(file){
          wb_file <- createWorkbook()
          
          metabo_tab <- dat[["metabocrates_dat_group"]] %>%
            select(!c(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]],
                      attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]]))
          
          addWorksheet(wb_file, "metabolites_martix")
          writeData(wb_file, "metabolites", metabo_tab)
          
          LOD_tab <- attr(dat[["metabocrates_dat_group"]], "LOD_table") %>%
            select(!c(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]],
                      attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]]))
          
          addWorksheet(wb_file, "LOD_table")
          writeData(wb_file, "LOD_table", LOD_tab)
          
          for(i in names(attr(dat[["metabocrates_dat_group"]], "NA_info"))){
            addWorksheet(wb_file, i)
            writeData(wb_file, i, attr(dat[["metabocrates_dat_group"]], "NA_info")[[i]])
          }
          
          metabo_names <-
            tibble(metabolites = attr(dat[["metabocrates_dat_group"]], "metabolites"))
          
          addWorksheet(wb_file, "metabolites_names")
          writeData(wb_file, "metabolites_names", metabo_names)
          
          addWorksheet(wb_file, "samples")
          writeData(wb_file, "samples",
                    attr(dat[["metabocrates_dat_group"]], "samples"))
          
          if(!is.null(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]])){
            addWorksheet(wb_file, "removed_LOD")
            writeData(wb_file, "samples",
                      attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]])
          }
          
          if(!is.null(attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]])){
            addWorksheet(wb_file, "removed_QC")
            writeData(wb_file, "samples",
                      attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]])
          }
          
          if(!is.null(attr(dat[["metabocrates_dat_group"]], "group"))){
            addWorksheet(wb_file, "group_name")
            writeData(wb_file, "group_name",
                      attr(dat[["metabocrates_dat_group"]], "group"))
          }
          
          if(!is.null(attr(dat[["metabocrates_dat_group"]], "completed"))){
            imp_tab <- attr(dat[["metabocrates_dat_group"]], "completed") %>%
              select(!c(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]],
                        attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]]))
            
            addWorksheet(wb_file, "imputed")
            writeData(wb_file, "imputed", imp_tab)
          }
          
          if(!is.null(attr(dat[["metabocrates_dat_group"]], "cv"))){
            cv_tab <- attr(dat[["metabocrates_dat_group"]], "cv") %>%
              select(!c(attr(dat[["metabocrates_dat_group"]], "removed")[["LOD"]],
                        attr(dat[["metabocrates_dat_group"]], "removed")[["QC"]]))
            
            addWorksheet(wb_file, "cv")
            writeData(wb_file, "cv", cv_tab)
          }
          
          saveWorkbook(wb_file, file, overwrite = TRUE)
        }
      )
    }else if(id == "download_zip"){
      output[["download"]] <- downloadHandler(
        filename = "all_plots.zip",
        content = function(file){
          get_plt <- function(id){
            if(exists(id)) get(id)()
            else NA
          }
            
          plots_lst <- list(
            "groups_sizes_barplot.pdf" = get_plt("groups_plt_reactive"),
            "missing_values_barplot.pdf" = get_plt("mv_types_plt_reactive"),
            "missing_values_counts.pdf" = get_plt("NA_ratios_plt_full"),
            "correlations_heatmap.pdf" = get_plt("full_corr_heatmap_plt"),
            "venn_diagram.pdf" = get_plt("venn_diagram"),
            "missing_values_heatmap.pdf" = get_plt("missing_heatmap"),
            "distribution_plot.pdf" = get_plt("dist_plt"),
            "PCA_plot.pdf" = get_plt("PCA_plt"),
            "variance_explained_plot.pdf" = get_plt("PCA_variance"),
            "2_metabolites_plot.pdf" = get_plt("2_metabo_plt")
          )
          
          for(i in 1:length(plots_lst)){
            if(!is.na(plots_lst[[i]]))
              ggsave(names(plots_lst)[i], plot = plots_lst[[i]], device = "pdf")
          }
          
          zip(file, names(plots_lst)[which(!is.na(plots_lst))])
        }
      )
    }else if(id == "download_pdf"){
      NULL
    }
  })
}
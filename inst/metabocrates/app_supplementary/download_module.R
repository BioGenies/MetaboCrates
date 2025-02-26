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
    observe({
    if(is.null(dat[["metabocrates_dat_group"]]))
      download_dat <- dat[["metabocrates_dat"]]
    else
      download_dat <- dat[["metabocrates_dat_group"]]
    
    if(id == "download_rds"){
      output[["download"]] <- downloadHandler(
        filename = "project.rds",
        content = function(file) saveRDS(download_dat, file)
      )
    }else if(id == "download_matrix"){
      output[["download"]] <- downloadHandler(
        filename = "metabolomics_matrix.xlsx",
        content = function(file){
          wb_file <- createWorkbook()
          
          if(is.null(attr(download_dat, "completed")))
            metabo_tab <- download_dat
          else
            metabo_tab <- attr(download_dat, "completed")
          
          metabo_tab <- metabo_tab %>%
            select(any_of(c(
              "sample identification", "sample id",
              attr(download_dat, "group"),
              setdiff(attr(download_dat, "metabolites"),
                      c(attr(download_dat, "removed")[["LOD"]],
                        attr(download_dat, "removed")[["QC"]]))
            )))
          
          addWorksheet(wb_file, "metabolites_matrix")
          writeData(wb_file, "metabolites_matrix", metabo_tab)
          saveWorkbook(wb_file, file, overwrite = TRUE)
        }
      )
    }else if(id == "download_tables"){
      output[["download"]] <- downloadHandler(
        filename = "project.xlsx",
        content = function(file){
          wb_file <- createWorkbook()
          
          metabo_tab <- download_dat %>%
            select(!c(attr(download_dat, "removed")[["LOD"]],
                      attr(download_dat, "removed")[["QC"]]))
          
          addWorksheet(wb_file, "metabolites_matrix")
          writeData(wb_file, "metabolites_matrix", metabo_tab)
          
          LOD_tab <- attr(download_dat, "LOD_table") %>%
            select(!c(attr(download_dat, "removed")[["LOD"]],
                      attr(download_dat, "removed")[["QC"]]))
          
          addWorksheet(wb_file, "LOD_table")
          writeData(wb_file, "LOD_table", LOD_tab)
          
          for(i in names(attr(download_dat, "NA_info"))){
            if(!is.null(attr(download_dat, "NA_info")[[i]])){
              if(i == "counts") sheet <- "NA_counts"
              else sheet <- i
              
              addWorksheet(wb_file, sheet)
              writeData(wb_file, sheet, attr(download_dat, "NA_info")[[i]])
            }
          }
          
          metabo_names <-
            tibble(metabolites = attr(download_dat, "metabolites"))
          
          addWorksheet(wb_file, "metabolites_names")
          writeData(wb_file, "metabolites_names", metabo_names)
          
          addWorksheet(wb_file, "samples")
          writeData(wb_file, "samples",
                    attr(download_dat, "samples"))
          
          if(!is.null(attr(download_dat, "removed")[["LOD"]])){
            addWorksheet(wb_file, "removed_LOD")
            writeData(wb_file, "samples",
                      attr(download_dat, "removed")[["LOD"]])
          }
          
          if(!is.null(attr(download_dat, "removed")[["QC"]])){
            addWorksheet(wb_file, "removed_QC")
            writeData(wb_file, "samples",
                      attr(download_dat, "removed")[["QC"]])
          }
          
          if(!is.null(attr(download_dat, "group"))){
            addWorksheet(wb_file, "group_name")
            writeData(wb_file, "group_name",
                      attr(download_dat, "group"))
          }
          
          if(!is.null(attr(download_dat, "completed"))){
            imp_tab <- attr(download_dat, "completed") %>%
              select(!c(attr(download_dat, "removed")[["LOD"]],
                        attr(download_dat, "removed")[["QC"]]))
            
            addWorksheet(wb_file, "imputed")
            writeData(wb_file, "imputed", imp_tab)
          }
          
          if(!is.null(attr(download_dat, "cv"))){
            cv_tab <- attr(download_dat, "cv") %>%
              select(!c(attr(download_dat, "removed")[["LOD"]],
                        attr(download_dat, "removed")[["QC"]]))
            
            addWorksheet(wb_file, "cv")
            writeData(wb_file, "cv", cv_tab)
          }
          
          saveWorkbook(wb_file, file, overwrite = TRUE)
        }
      )
    }else if(id == "download_zip"){
      if(is.null(dat[["metabocrates_dat_group"]]))
        dat[["metabocrates_dat_group"]] <- dat[["metabocrates_dat"]]
      
      output[["download"]] <- downloadHandler(
        filename = "all_plots.zip",
        content = function(file){
          get_plt <- function(plot_id){
            if(exists(plot_id)) get(plot_id)()
            else NA
          }
            
          plots_lst <- list(
            "groups_sizes_barplot.pdf" = get_plt("groups_plt_reactive"),
            "missing_values_barplot.pdf" = get_plt("mv_types_plt_reactive"),
            "missing_values_counts.pdf" = get_plt("NA_ratios_plt_full"),
            "correlations_heatmap.pdf" = get_plt("full_corr_heatmap_plt"),
            "venn_diagram.pdf" = get_plt("venn_diagram"),
            "missing_values_heatmap.pdf" = get_plt("missing_heatmap"),
            "PCA_plot.pdf" = get_plt("PCA_plt"),
            "variance_explained_plot.pdf" = get_plt("PCA_variance")
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
  })
}
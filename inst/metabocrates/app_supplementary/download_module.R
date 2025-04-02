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
    ns <- session$ns
    
    if(!file.exists(paste0("./texts/", id, ".md"))){
      file.create(paste0("./texts/", id, ".md"))
    }
    
    if(id == "download_rds"){
      output[["download"]] <- downloadHandler(
        filename = "project.rds",
        content = function(file) {
          if(is.null(dat[["metabocrates_dat_group"]]))
            download_dat <- dat[["metabocrates_dat"]]
          else
            download_dat <- dat[["metabocrates_dat_group"]]
          
          saveRDS(download_dat, file)
        }
      )
    }else if(id == "download_matrix"){
      output[["download"]] <- downloadHandler(
        filename = "metabolomics_matrix.xlsx",
        content = function(file){
          if(is.null(dat[["metabocrates_dat_group"]]))
            download_dat <- dat[["metabocrates_dat"]]
          else
            download_dat <- dat[["metabocrates_dat_group"]]
          
          wb_file <- createWorkbook()
          
          if(is.null(attr(download_dat, "completed")))
            metabo_tab <- download_dat
          else
            metabo_tab <- attr(download_dat, "completed")
          
          metabo_tab <- metabo_tab %>%
            select(any_of(c(
              "plate bar code", "sample identification", "sample id",
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
          if(is.null(dat[["metabocrates_dat_group"]]))
            download_dat <- dat[["metabocrates_dat"]]
          else
            download_dat <- dat[["metabocrates_dat_group"]]
          
          wb_file <- createWorkbook()
          
          metabo_tab <- download_dat %>%
            select(!any_of("tmp_id")) %>%
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
            tibble(metabolites = setdiff(attr(download_dat, "metabolites"),
                                         c(attr(download_dat, "removed")[["LOD"]],
                                           attr(download_dat, "removed")[["QC"]])))
          
          addWorksheet(wb_file, "metabolites_names")
          writeData(wb_file, "metabolites_names", metabo_names)
          
          addWorksheet(wb_file, "samples")
          writeData(wb_file, "samples",
                    attr(download_dat, "samples"))
          
          if(!is.null(attr(download_dat, "removed")[["LOD"]])){
            addWorksheet(wb_file, "removed_LOD")
            writeData(wb_file, "removed_LOD",
                      attr(download_dat, "removed")[["LOD"]])
          }
          
          if(!is.null(attr(download_dat, "removed")[["QC"]])){
            addWorksheet(wb_file, "removed_QC")
            writeData(wb_file, "removed_QC",
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
              filter(!(metabolite %in%
                         c(attr(download_dat, "removed")[["LOD"]],
                           attr(download_dat, "removed")[["QC"]])))
            
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
          if(is.null(dat[["metabocrates_dat_group"]]))
            download_dat <- dat[["metabocrates_dat"]]
          else
            download_dat <- dat[["metabocrates_dat_group"]]
          
          tmp_dir <- tempdir()
            
          plots_lst <- list(
            "groups_sizes_barplot.pdf" = {
              if(is.null(attr(download_dat, "group")))
                NULL
              else
                plot_groups(download_dat)
            },
            "missing_values_barplot.pdf" =  plot_mv_types(download_dat),
            "missing_values_counts.pdf" = plot_NA_percent(download_dat, 
                                                          type = "joint",
                                                          interactive = FALSE),
            "missing_values_counts_type.pdf" = plot_NA_percent(download_dat, 
                                                          type = "NA_type",
                                                          interactive = FALSE),
            "missing_values_counts_group.pdf" = {
              if(is.null(attr(download_dat, "group")))
                NULL
              else
                plot_NA_percent(download_dat, type = "group",
                                interactive = FALSE)
            },
            "correlations_heatmap.pdf" = {
              if(is.null(attr(download_dat, "completed")))
                NULL
              else
                create_correlations_heatmap(
                  download_dat,
                  threshold = ifelse(
                    is.null(input[["corr_threshold"]]),
                    0.3,
                    input[["corr_threshold"]]
                  ),
                  metabolites_to_display =
                    ifelse(is.null(input[["corr_heatmap_metabolites"]]),
                           "all",
                           input[["corr_heatmap_metabolites"]]),
                  interactive = FALSE
                )
            },
            "venn_diagram.pdf" = {
              if(is.null(attr(download_dat, "group")))
                NULL
              else
               create_venn_diagram(
                download_dat,
                ifelse(is.null(input[["filtering_threshold"]]),
                       0.8,
                       input[["filtering_threshold"]]/100)
               )
            },
            "missing_values_heatmap.pdf" = plot_heatmap(download_dat),
            "PCA_plot_sample_type.pdf" = {
              if(is.null(attr(download_dat, "completed")))
                NULL
              else
                if(is.null(input[["PCA_types"]]))
                  create_PCA_plot(download_dat, type = "sample_type",
                                  interactive = FALSE)
                else
                  create_PCA_plot(download_dat, type = "sample_type",
                                  types_to_display = input[["PCA_types"]],
                                  interactive = FALSE)
            },
            "PCA_plot_group.pdf" = {
              if(is.null(attr(download_dat, "completed")))
                NULL
              else{
                if(is.null(attr(download_dat, "group")))
                  NULL
                else
                  create_PCA_plot(download_dat, type = "group",
                                  interactive = FALSE)
              }
            },
            "biplot.pdf" = {
              if(is.null(attr(download_dat, "completed")))
                NULL
              else
                create_PCA_plot(
                  download_dat, type = "biplot",
                  threshold = ifelse(is.null(input[["PCA_threshold"]]),
                                     0.8,
                                     input[["PCA_threshold"]]/100),
                  interactive = FALSE
                )
            },
            "variance_explained_plot.pdf" = {
              if(is.null(attr(download_dat, "completed")))
                NULL
              else
                pca_variance(
                  download_dat,
                  threshold = ifelse(is.null(input[["PCA_variance_threshold"]]),
                                     0.8,
                                     input[["PCA_variance_threshold"]]/100),
                  max_num = ifelse(is.null(input[["PCA_variance_max_num"]]),
                                   5,
                                   input[["PCA_variance_max_num"]])
                )
            }
          )
          
          plot_files <- c()
          for(name in names(plots_lst)){
            plot_obj <- plots_lst[[name]]
            if(!is.null(plot_obj)){
              file_path <- file.path(tmp_dir, name)
              ggplot2::ggsave(file_path, plot = plot_obj,
                              width = 14, height = 7, device = "pdf")
              plot_files <- c(plot_files, file_path)
            }
          }
          
          zip(zipfile = file, files = plot_files, flags = "-j")
        }
      )
    }else if(id == "download_pdf"){
        output[["download"]] <- downloadHandler(
          filename = "report.pdf",
          content = function(file){
            rmarkdown::render("./app_supplementary/report_template.Rmd",
                              output_file = file,
                              envir = new.env(parent = globalenv()),
                              params = list(
                                dat = dat
                              ))
          }
        )
    }
  })
}
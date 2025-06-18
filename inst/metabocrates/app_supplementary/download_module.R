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
               helper(content = id),
             style = "margin: 0;"
           ),
           downloadButton(ns("download"), "Download")
         )
  )
  
  
  
}

download_SERVER <- function(id, dat, main_input, filtering_threshold_ex = NULL){
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
              "plate bar code", "sample identification",
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
              
              if(i == "NA_ratios_group"){
                NA_table <- attr(download_dat, "NA_info")[[i]] %>%
                  tidyr::separate(grouping_column,
                                  into = attr(download_dat, "group"),
                                  sep = ",")
                
                writeData(wb_file, sheet, NA_table)
              }else
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
          
          if(!is.null(attr(download_dat, "group"))){
            addWorksheet(wb_file, "group")
            
            writeData(wb_file, "group", "Grouping columns")
            writeData(wb_file, "group", startRow = 2,
                      attr(download_dat, "group"))
            
            group_tab <- download_dat %>%
              filter(`sample type` == "Sample") %>%
              group_by(across(all_of(attr(download_dat, "group")))) %>%
              summarise(Counts = n())
            
            writeData(wb_file, "group", startCol = 3,
                      group_tab)
          }
          
          if(!is.null(attr(download_dat, "removed")[["LOD"]])){
            addWorksheet(wb_file, "removed_LOD")
            
            writeData(
              wb_file, "removed_LOD",
              data.frame(
                "Metabolites removed" =  attr(download_dat, "removed")[["LOD"]],
                check.names = FALSE
              )
            )
            
            writeData(wb_file, "removed_LOD", startCol = 3, colNames = FALSE,
                      data.frame(
                        c("Threshold:", "Count:"),
                        c(paste0(main_input[["filtering_threshold"]], "%"),
                          length(attr(download_dat, "removed")[["LOD"]])),
                        check.names = FALSE
                      )
            )
          }
          
          if(!is.null(attr(download_dat, "removed")[["QC"]])){
            addWorksheet(wb_file, "removed_QC")
            
            writeData(
              wb_file, "removed_QC",
              data.frame(
                "Metabolites removed" =  attr(download_dat, "removed")[["QC"]],
                check.names = FALSE
              )
            )
            
            writeData(wb_file, "removed_QC", startCol = 3, colNames = FALSE,
                      data.frame(
                        c("Threshold:", "Count:"),
                        c(paste0(main_input[["cv_threshold"]], "%"),
                          length(attr(download_dat, "removed")[["QC"]])),
                        check.names = FALSE
                      )
            )
          }
          
          if(!is.null(attr(download_dat, "completed"))){
            imputation_info <- data.frame(
              Name = c("LOD method:", "LOD type:", "LLOQ method:",
                       "ULOQ method:"),
              Value = c(main_input[["LOD_method"]], main_input[["LOD_type"]],
                        main_input[["LLOQ_method"]], main_input[["ULOQ_method"]])
            )
            
            addWorksheet(wb_file, "imputation_info")
            writeData(wb_file, "imputation_info",
                      imputation_info, colNames = FALSE)
            
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
          showModal(modalDialog("Creating file, please wait...", footer = NULL))
          on.exit(removeModal())
          
          if(is.null(dat[["metabocrates_dat_group"]]))
            download_dat <- dat[["metabocrates_dat"]]
          else
            download_dat <- dat[["metabocrates_dat_group"]]
          
          tmp_dir <- tempdir()
            
          plots_lst <- list(
            "group_barplot.pdf" = {
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
                  threshold = main_input[["corr_threshold"]],
                  metabolites_to_display =main_input[["corr_heatmap_metabolites"]],
                  type = "both",
                  interactive = FALSE
                )
            },
            "venn_diagram.pdf" = {
              if(is.null(attr(download_dat, "group")))
                NULL
              else{
                lvls_count <- dat[["metabocrates_dat_group"]] %>%
                  filter(`sample type` == "Sample") %>%
                  select(all_of(attr(dat[["metabocrates_dat_group"]], "group"))) %>%
                  n_distinct()
                
                if(lvls_count > 5) NULL
                else
                  create_venn_diagram(download_dat,
                                      main_input[["filtering_threshold"]]/100)
              }
            },
            "missing_values_heatmap.pdf" = plot_heatmap(download_dat),
            "sample_type_PCA_plot.pdf" = {
              if(is.null(attr(download_dat, "completed")))
                NULL
              else
                create_PCA_plot(download_dat, group_by = "sample_type",
                                types_to_display = main_input[["sample_type_PCA_types"]],
                                interactive = FALSE)
            },
            "group_PCA_plot.pdf" = {
              if(is.null(attr(download_dat, "completed")) ||
                 is.null(attr(download_dat, "group")))
                NULL
              else
                create_PCA_plot(download_dat, group_by = "group",
                                interactive = FALSE)
            },
            "sample_type_biplot.pdf" = {
              if(is.null(attr(download_dat, "completed")))
                NULL
              else
                create_PCA_plot(download_dat, type = "biplot",
                                group_by = "sample_type",
                                threshold = main_input[["sample_type_PCA_threshold"]]/100,
                                interactive = FALSE)
            },
            "group_biplot.pdf" = {
              if(is.null(attr(download_dat, "completed")) ||
                 is.null(attr(download_dat, "group")))
                NULL
              else
                create_PCA_plot(download_dat, type = "biplot",
                                group_by = "group",
                                threshold = main_input[["group_PCA_threshold"]]/100,
                                interactive = FALSE)
            },
            "sample_type_variance_explained_plot.pdf" = {
              if(is.null(attr(download_dat, "completed")))
                NULL
              else
                pca_variance(
                  download_dat,
                  threshold = main_input[["sample_type_PCA_variance_threshold"]]/100,
                  max_num = main_input[["sample_type_PCA_variance_max_num"]],
                  cumulative = main_input[["sample_type_PCA_variance_cum"]]
                )
            },
            "group_variance_explained_plot.pdf" = {
              if(is.null(attr(download_dat, "completed")) ||
                 is.null(attr(download_dat, "group")))
                NULL
              else
                pca_variance(
                  download_dat,
                  type = "group",
                  threshold = main_input[["group_PCA_variance_threshold"]]/100,
                  max_num = main_input[["group_PCA_variance_max_num"]],
                  cumulative = main_input[["group_PCA_variance_cum"]]
                )
            },
            "correlations_heatmap_after_imputation.pdf" = {
              if(is.null(attr(download_dat, "completed")))
                NULL
              else
                create_correlations_heatmap(
                  download_dat,
                  threshold = main_input[["corr_threshold"]],
                  metabolites_to_display = main_input[["corr_heatmap_metabolites"]],
                  interactive = FALSE
                )
            }
          )
          
          plot_files <- c()
          
          metabo_num <- length(setdiff(attr(download_dat, "metabolites"),
                                       unlist(attr(download_dat, "removed"))))
          na_metabo_num <- attr(download_dat, "NA_info")[["NA_ratios_type"]] %>%
            filter(NA_frac > 0 &
                   !(metabolite %in% unlist(attr(download_dat, "removed")))) %>%
            select(metabolite) %>%
            n_distinct()
          
          for(name in names(plots_lst)){
            plot_obj <- plots_lst[[name]]
            if(!is.null(plot_obj)){
              file_path <- file.path(tmp_dir, name)
              
              if(name %in% c("missing_values_counts.pdf",
                             "missing_values_counts_type.pdf",
                             "missing_values_counts_group.pdf")){
                plt_height <- max(na_metabo_num * 0.2, 7)
                plt_width <- 14
              }else if(name == "missing_values_heatmap.pdf"){
                plt_height <- download_dat %>%
                  filter(`sample type` == "Sample") %>%
                  select(`plate bar code`) %>%
                  n_distinct()
                plt_height <- max(plt_height * metabo_num * 0.2, 7)
                
                plt_width <- download_dat %>%
                  filter(`sample type` == "Sample") %>%
                  group_by(`plate bar code`) %>%
                  summarise(n = n()) %>%
                  select(n) %>%
                  max()
                plt_width <- max(plt_width * 0.175, 14)
              }else{
                plt_height <- 7
                plt_width <- 14
              }
              
              ggplot2::ggsave(file_path, plot = plot_obj,
                              width = plt_width, height = plt_height,
                              device = "pdf", limitsize = FALSE)
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
            showModal(modalDialog("Creating report, please wait...",
                                  footer = NULL))
            on.exit(removeModal())
            
            params <- list("filtering_threshold",
                           "LOD_method",
                           "LLOQ_method",
                           "ULOQ_method",
                           "LOD_type",
                           "corr_threshold",
                           "corr_heatmap_metabolites",
                           "cv_threshold",
                           "sample_type_PCA_types",
                           "sample_type_PCA_threshold",
                           "sample_type_PCA_variance_threshold",
                           "sample_type_PCA_variance_max_num",
                           "sample_type_PCA_variance_cum",
                           "group_PCA_threshold",
                           "group_PCA_variance_threshold",
                           "group_PCA_variance_max_num",
                           "group_PCA_variance_cum",
                           "corr_threshold_both",
                           "corr_heatmap_metabolites_both")
            
            params_lst <- setNames(
              lapply(params, function(param) main_input[[param]]),
              params
            )
            
            if(is.null(dat[["metabocrates_dat_group"]]))
              params_lst[["dat"]] <- dat[["metabocrates_dat"]]
            else
              params_lst[["dat"]] <- dat[["metabocrates_dat_group"]]
            
            params_lst[["filtering_threshold_ex"]] <- filtering_threshold_ex()
              
            params_lst <- params_lst[lengths(params_lst) != 0]
              
            rmarkdown::render("./app_supplementary/report_template.Rmd",
                              output_file = file,
                              envir = new.env(parent = globalenv()),
                              params = params_lst)
            
          }
        )
    }
  })
}
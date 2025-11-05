
custom_datatable <- function(dat,
                             paging = TRUE,
                             scrollY = 380,
                             selection = list(selectable = FALSE),
                             pageLength = 15,
                             container = NULL,
                             styles = NULL) {
  
  custom_dt <- if(is.null((container)))
    DT::datatable(dat,
                  editable = FALSE,
                  selection = selection,
                  extensions = "Buttons",
                  options = list(paging = paging,
                                 scrollX = TRUE,
                                 scrollY = scrollY,
                                 pageLength = pageLength,
                                 searching = FALSE,
                                 dom = 'Bfrtip',
                                 buttons = c("csv", "excel", "pdf")),
                  class = "display nowrap",
                  rownames = FALSE)
  else
    DT::datatable(dat,
                  editable = FALSE,
                  selection = selection,
                  extensions = "Buttons",
                  options = list(paging = paging,
                                 scrollX = TRUE,
                                 scrollY = scrollY,
                                 pageLength = pageLength,
                                 searching = FALSE,
                                 dom = 'Bfrtip',
                                 buttons = c("csv", "excel", "pdf")),
                  class = "display nowrap",
                  rownames = FALSE,
                  container = container)
  
  if(!is.null(styles)){
    for(i in 1:length(styles))
      custom_dt <- do.call(formatStyle, c(table = list(custom_dt), styles[[i]]))
  }
  
  custom_dt
}



display_short <- function(column, digits = 2) {
  short_form <- ifelse(is.na(as.numeric(column)),
                  column,
                  ifelse(nchar(column) > 7,
                         round(as.numeric(column), digits),
                         column))
  ifelse(is.na(short_form), "NA", short_form)
}


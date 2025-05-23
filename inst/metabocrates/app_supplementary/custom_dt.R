
custom_datatable <- function(dat,
                             paging = TRUE,
                             scrollY = 380,
                             selection = list(selectable = FALSE)) {
  DT::datatable(dat,
                editable = FALSE,
                selection = selection,
                extensions = "Buttons",
                options = list(paging = paging,
                               scrollX = TRUE,
                               scrollY = scrollY,
                               pageLength = 15,
                               searching = FALSE,
                               dom = 'Bfrtip',
                               buttons = c("csv", "excel", "pdf")),
                class = "display nowrap",
                rownames = FALSE)
}



display_short <- function(column) {
  short_form <- ifelse(is.na(as.numeric(column)),
                  column,
                  ifelse(nchar(column) > 7,
                         round(as.numeric(column), 2),
                         column))
  ifelse(is.na(short_form), "NA", short_form)
}


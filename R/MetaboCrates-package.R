#' MetaboCrates
#'
#' @description The \code{MetaboCrates} package is a toolbox for processing of 
#' Biocrates® metabolomics data
#' 
#' @author Weronika Puchala, Michal Burdukiewicz.
#' @docType package
#' @name MetaboCrates-package
#' @aliases MetaboCrates

if(getRversion() >= "2.15.1")  
  utils::globalVariables(c('sample type', 'metabolite', 'value', 'CV', 'sample',
                           'NA_frac', 'plate bar code', 'measurement time', 
                           'sample identification'))
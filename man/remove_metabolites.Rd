% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raw_data_funs.R
\name{remove_metabolites}
\alias{remove_metabolites}
\title{Add metabolites to the attribute \code{removed}}
\usage{
remove_metabolites(dat, metabolites_to_remove, type)
}
\arguments{
\item{dat}{a \code{\link{raw_data}} object. Output of \code{\link[=read_data]{read_data()}}
function.}

\item{metabolites_to_remove}{a character string or vector specifying the
names of metabolites to remove.}

\item{type}{a character string specifying the criterion used to evaluate
whether a metabolite should be removed. Can be \code{LOD} or \code{QC}.}
}
\description{
\code{remove_metabolites()} remove the specified metabolites from data by adding
them to the attribute \code{removed}.
}
\examples{
path <- get_example_data("small_biocrates_example.xls")
test_dat <- read_data(path)
attr(remove_metabolites(test_dat, "C0", "LOD"), "removed")

}

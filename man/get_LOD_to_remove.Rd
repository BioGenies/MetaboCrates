% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raw_data_funs.R
\name{get_LOD_to_remove}
\alias{get_LOD_to_remove}
\title{Get metabolites to remove based on missing values proportion}
\usage{
get_LOD_to_remove(dat, threshold = 0.8, use_group = TRUE)
}
\arguments{
\item{dat}{a \code{\link{raw_data}} object. Output of \code{\link[=read_data]{read_data()}}
function.}

\item{threshold}{a decimal specifying the minimum proportion of missing
values a metabolite must have to be removed.}

\item{use_group}{logical. If \code{TRUE}, a metabolite will be returned only if
the proportion of missing values exceeds the threshold in every group level.}
}
\description{
\code{get_LOD_to_remove()} returns the names of metabolites having the higher
proportion of missing values than the given threshold.
}
\examples{
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
get_LOD_to_remove(dat, 0.1)

}

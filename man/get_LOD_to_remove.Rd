% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raw_data_funs.R
\name{get_LOD_to_remove}
\alias{get_LOD_to_remove}
\title{Get metabolites to remove}
\usage{
get_LOD_to_remove(dat, threshold = 0.8, use_group = TRUE)
}
\arguments{
\item{dat}{words}

\item{threshold}{Percentage value.}

\item{use_group}{logical...}
}
\description{
Returns metabolite names having more NA values in each group
level than the given threshold.
}
\examples{
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
get_LOD_to_remove(dat, 0.1)

}

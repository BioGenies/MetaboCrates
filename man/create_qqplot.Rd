% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_funs.R
\name{create_qqplot}
\alias{create_qqplot}
\title{Qqplots of individual metabolite values before and after imputation}
\usage{
create_qqplot(dat, metabolite, interactive = TRUE)
}
\arguments{
\item{dat}{A \code{raw_data} object, the output of the \code{\link[=read_data]{read_data()}} function.
The data should be completed and filtered to include only samples of type
"Sample".}

\item{metabolite}{A name of metabolite of interest.}

\item{interactive}{logical indicating whether the plot should be interactive.
If TRUE, the plot includes interactive tooltips.}
}
\description{
Qqplots of individual metabolite values before and after imputation
}
\examples{
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
dat <- complete_data(dat, "limit", "limit", "limit")
create_qqplot(dat, "C0")

}

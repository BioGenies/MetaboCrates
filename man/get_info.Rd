% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raw_data_funs.R
\name{get_info}
\alias{get_info}
\title{Get informations from Biocrates data}
\usage{
get_info(raw_data)
}
\arguments{
\item{raw_data}{a \code{\link{raw_data}} object. Output of \code{\link[=read_data]{read_data()}}
function.}
}
\description{
Get informations from Biocrates data
}
\examples{
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
get_info(dat)
cat(get_info(dat))

dat <- add_group(dat, "group")
get_info(dat)
cat(get_info(dat))

}
\seealso{
\code{\link[=read_data]{read_data()}}
}

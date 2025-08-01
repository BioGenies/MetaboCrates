% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/complete_LODs.R
\name{complete_data}
\alias{complete_data}
\title{Complete not quantified values}
\usage{
complete_data(
  dat,
  LOD_method = NULL,
  LLOQ_method = NULL,
  ULOQ_method = NULL,
  LOD_type = "calc"
)
}
\arguments{
\item{dat}{a \code{\link{raw_data}} object. Output of \code{\link[=read_data]{read_data()}}
function.}

\item{LOD_method}{a character string specifying the imputation method to be
applied to \verb{< LOD} values, or \code{NULL} to change these values to \code{NA}.
Available methods are: \code{halfmin}, \code{random}, \code{halflimit}, \code{limit},
\verb{limit-0.2min} and \code{logspline}.}

\item{LLOQ_method}{a character string specifying the imputation method to be
applied to \verb{< LLOQ} values, or \code{NULL} if these values should not be imputed.
Currently, the only available method is: \code{limit}.}

\item{ULOQ_method}{a character string specifying the imputation method to be
applied to \verb{> ULOQ} values, or \code{NULL} if these values should not be imputed.
Available methods are: \code{limit}, \verb{third quartile} and \verb{scaled random}.}

\item{LOD_type}{a character string specifying which LOD type to use for
imputing values. Possible values are \code{"OP"} and \code{"calc"}.}
}
\description{
\code{complete_data()} completes missing values related to the limits of
quantification or detection.
}
\examples{
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
dat <- complete_data(dat)

}

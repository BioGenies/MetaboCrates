% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/complete_LODs.R
\name{complete_ULOQ}
\alias{complete_ULOQ}
\title{Complete values above limit of quantification}
\usage{
complete_ULOQ(gathered_data, method, LOD_vals)
}
\arguments{
\item{gathered_data}{description}

\item{method}{a character. Imputation method, one of "limit" or NULL meaning
changing missing values into NAs.}

\item{LOD_vals}{description}
}
\description{
This function imputes values below limit of quantification
("> ULOQ")
}
\keyword{internal}

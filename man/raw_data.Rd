% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raw_data_class.R
\name{raw_data}
\alias{raw_data}
\title{raw_data class}
\usage{
raw_data(metabolomics_matrix, LOD_table, metabolites, group = NULL)
}
\arguments{
\item{metabolomics_matrix}{a \code{\link{data.frame}}, matrix containing
biocrates data}

\item{LOD_table}{a \code{\link{list}} containing two elements:
\itemize{
\item \code{table}: LOD table of limits of detection / quantification,
\item \code{types}: character vector of types of values, for example calc.
meaning calculated from samples and op, received from operator.
}}

\item{metabolites}{a \code{\link{character}}, vector of metabolites names
contained in the data.}

\item{group}{a \code{NULL} indicating no grouping or
a vector of \code{\link{character}} names of group columns. Default to NULL.
The code will throw an error in the case when:
\itemize{
\item column of such a name won't be contained in the dataset,
\item there will be NA's in the grouping column,
\item any group will have less than 2 observations.
}}
}
\value{
\code{\link{raw_data}} object metabolomics matrix with the following
attributes:
\itemize{
\item \code{LOD_table}
\item \code{NA_info}: a \code{\link{list}} related to missing values in the data.
It contains \code{NA_ratios}: fractions of missing values per every
metabolite. When \code{group} parameter (see below) is provided and
\code{counts}: table of types of missing values with their counts (related
to "sample" type samples).
\item \code{metabolites}
\item \code{samples}: a \code{\link{data.frame}} containing names of samples
types and their counts
\item \code{group} : a character name of group from the table
\item \code{removed}: a list of removed metabolites
\item \code{plate bar code}: each measurement has two plate bar codes depending on the method used
}
}
\description{
raw_data class
}

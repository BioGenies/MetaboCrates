% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_funs.R
\name{plot_heatmap}
\alias{plot_heatmap}
\title{Heatmap of missing values}
\usage{
plot_heatmap(
  dat,
  plate_bar_code = NULL,
  include_title = FALSE,
  show_colors = TRUE
)
}
\arguments{
\item{dat}{a \code{\link{raw_data}} object. Output of \code{\link[=read_data]{read_data()}}
function.}

\item{plate_bar_code}{a single plate bar code used to select observations to
include in the plot. If \code{NULL} (default), a grid of plots is returned, where
each plot corresponds to a different plate bar code.}

\item{include_title}{logical. Indicates whether the title with the
plate bar code should be included (only if \code{plate_bar_code} is not \code{NULL}).
Defaults to \code{FALSE}.}

\item{show_colors}{logical. If \code{TRUE}, distinct colors are applied to
different types of missing values, following the conventions used in
Biocrates® files.}
}
\description{
\code{plot_heatmap()} creates a heatmap of missing values for the specified
plate bar code or for all of them.
}
\examples{
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
plot_heatmap(dat, "1036372116-1 | 1036372121-1")

}

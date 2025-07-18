% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_funs.R
\name{create_plot_of_2_metabolites}
\alias{create_plot_of_2_metabolites}
\title{Plot of two metabolites}
\usage{
create_plot_of_2_metabolites(dat, metabolite1, metabolite2, interactive = TRUE)
}
\arguments{
\item{dat}{a \code{raw_data} object, the output of the \code{\link[=read_data]{read_data()}} function.
The data have to be completed, for example using the \code{\link[=complete_data]{complete_data()}}
function.}

\item{metabolite1}{first metabolite name.}

\item{metabolite2}{second metabolite name.}

\item{interactive}{logical. If \code{TRUE} (default), a ggiraph interactive
plot is returned; otherwise, a standard ggplot object is produced.}
}
\description{
Plot of two metabolites
}
\examples{
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
dat <- complete_data(dat, "limit", "limit", "limit")
print(create_plot_of_2_metabolites(dat, "C0", "C2"))

}

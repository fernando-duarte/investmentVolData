% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bea.R
\name{make_vars_bea}
\alias{make_vars_bea}
\title{\code{make_vars_bea()} constructs variables from cleaned BEA data.}
\usage{
make_vars_bea(clean_data, nu = 1/2)
}
\arguments{
\item{clean_data}{Data returned by \link{clean_bea}.}

\item{nu}{a number between 0 and 1 that determines placed-in-service timing for investment
nu = 0 corresponds to the end of the year,
nu = 1 corresponds to the beginning of the year,
nu = 1/2 is what the BEA uses.}
}
\description{
\code{make_vars_bea()} constructs variables from cleaned BEA data.
}
\examples{
bea_raw <- get_bea()
bea_clean <- clean_bea(bea_raw)
make_vars_bea(bea_clean)
}

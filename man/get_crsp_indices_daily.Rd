% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crsp.R
\name{get_crsp_indices_daily}
\alias{get_crsp_indices_daily}
\title{\code{get_crsp_indices_daily()} downloads daily frequency stock market indices from CRSP}
\usage{
get_crsp_indices_daily(
  start_date = "1940-01-01",
  end_date = "2023-12-31",
  rf = NULL
)
}
\arguments{
\item{start_date}{Starting date in any format recognized by \link[lubridate:ymd]{lubridate::ymd}}

\item{end_date}{Ending date in any format recognized by \link[lubridate:ymd]{lubridate::ymd}}

\item{rf}{Risk-free rate to compute excess returns. Defaults to \code{NULL}, in which
case excess returns are not computed.}
}
\description{
\code{get_crsp_indices_daily()} downloads daily frequency stock market indices from CRSP
}

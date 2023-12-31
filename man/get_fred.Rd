% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fred.R
\name{get_fred}
\alias{get_fred}
\title{Download data from the Federal Reserve Bank of St. Louis FRED}
\usage{
get_fred(
  series_id = c("GDPC1", "GDP", "PNFI", "B009RC1Q027SBEA", "B009RC1A027NBEA",
    "NA000339Q", "Y033RC1Q027SBEA", "Y033RC1A027NBEA", "NA000340Q", "K1NTOTL1ST000",
    "K1NTOTL1EQ000", "M1NTOTL1EQ000", "M1NTOTL1ST000"),
  start_date = "1940-01-01",
  end_date = "2023-12-31",
  ...
)
}
\arguments{
\item{series_id}{A single character string, a character vector or tibble
representing a single (or multiple) FRED series}

\item{start_date}{Starting date in any format recognized by \link[lubridate:ymd]{lubridate::ymd}}

\item{end_date}{Ending date in any format recognized by \link[lubridate:ymd]{lubridate::ymd}}

\item{...}{Additional arguments passed to \link[fredr:fredr]{fredr::fredr}.}
}
\description{
\code{get_fred()} downloads FRED data.
}

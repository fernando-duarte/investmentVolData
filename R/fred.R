#' Download data from the Federal Reserve Bank of St. Louis FRED
#'
#' `get_fred()` downloads FRED data.
#'
#' @param fred_series A single character string, a character vector or tibble
#'   representing a single (or multiple) FRED series
#' @param ... Additional arguments passed to [tidyquant::tq_get].
#' @export get_fred
get_fred <- function(
    fred_series = c(
      "GDPC1",
      "GDPPOT"
    ),
    ...) {
  tidyquant::tq_get(
    fred_series,
    get = "economic.data",
    ...
  )
}

#' `clean_fred()` cleans FRED data.
#'
#' @param data Data downloaded from [investmentVolData::get_fred]
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @export clean_fred
#' @importFrom rlang .data
clean_fred <- function(
    data,
    start_date = "1950-01-01",
    end_date = "2023-01-01") {
  data |>
    dplyr::mutate(
      month = lubridate::floor_date(date, "month")
    ) |>
    dplyr::rename_with(stringr::str_to_lower) |>
    dplyr::filter(.data$month >= start_date & .data$month <= end_date)
}

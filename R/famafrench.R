#' Download data from Kenneth French's data library
#'
#' `get_famafrench()` downloads Fama-French 3 Factors.
#'
#' @param dataset_names Input vector with names of datasets to download. Either a character vector, or something
#'  coercible to one.
#' @param ... Additional arguments passed to [frenchdata::download_french_data].
#' @export get_famafrench
get_famafrench <- function(
    dataset_names = c("Fama/French 3 Factors", "Fama/French 3 Factors [Daily]"),
    ...) {
  dataset_names |> purrr::map(\(x) frenchdata::download_french_data(x, ...))
}

#' Cleans data downloaded from Kenneth French's data library
#' `clean_famafrench()` cleans data downloaded with [investmentVolData::get_famafrench].
#'
#' @param data Data in a format returned by `get_famafrench()`.
#'
#' @param start_date Date to start the data at.
#' @param end_date Date to end the data at.
#'
#' @export clean_famafrench
#' @importFrom rlang .data
clean_famafrench <- function(
    data,
    start_date = "1950-01-01",
    end_date = "2023-01-01") {
  start_date <- lubridate::ymd(start_date)
  end_date <- lubridate::ymd(end_date)
  data[[1]]$subsets$data[[1]] |>
    dplyr::mutate(
      month = lubridate::floor_date(lubridate::ymd(stringr::str_c(date, "01")), "month")
    ) |>
    dplyr::rename_with(stringr::str_to_lower) |>
    dplyr::filter(.data$month >= start_date & .data$month <= end_date)
}
#
#   raw <- frenchdata::download_french_data(dataset_name)
#   raw$subsets$data[[1]] |>
#     dplyr::mutate(
#       month = lubridate::floor_date(lubridate::ymd(stringr::str_c(date, "01")), "month"),
#       .keep = "none"
#     ) |>
#     dplyr::rename_with(stringr::str_to_lower) |>
#     dplyr::filter(month >= start_date & month <= end_date)
# }

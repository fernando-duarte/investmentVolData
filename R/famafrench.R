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
    start_date = "1940-01-01",
    end_date = "2023-12-31") {
  start_date <- lubridate::ymd(start_date)
  end_date <- lubridate::ymd(end_date)
  list(
    monthly =
  data[[1]]$subsets$data[[1]] |>
    dplyr::mutate(
      date = lubridate::floor_date(lubridate::ymd(stringr::str_c(date, "01")), "month")
    ) |>
    dplyr::rename_with(stringr::str_to_lower) |>
    dplyr::filter(.data$date >= start_date & .data$date <= end_date)
  ,
  daily =
  data[[2]]$subsets$data[[1]] |>
    dplyr::mutate(
      date = lubridate::ymd(date),
      dplyr::across(c("RF", "Mkt-RF", "SMB", "HML"), ~as.numeric(.) / 100),
      .keep = "none"
    ) |>
    dplyr::rename_with(stringr::str_to_lower) |>
    dplyr::filter(.data$date >= start_date & .data$date <= end_date)
  )
}


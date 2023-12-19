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

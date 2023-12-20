#' Download data the Bureau of Economic Analysis (BEA)
#'
#' `get_bea()` downloads bea data.
#'
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @export get_bea
#' @importFrom rlang .data
get_bea <- function(
    start_date = "1950-01-01",
    end_date = "2023-01-01") {
  start_date <- lubridate::ymd(start_date)
  end_date <- lubridate::ymd(end_date)
  beaSpecs <- list(
    "UserID" = Sys.getenv("BEA_API_KEY"),
    "Method" = "GetData",
    "datasetname" = "FixedAssets",
    "TableName" = "FAAt401",
    "Frequency" = "Q",
    "Year" = "X",
    "ResultFormat" = "json"
  )
  bea_raw <- bea.R::beaGet(beaSpecs)
}

#' `clean_bea()` downloads WRDS data.
#'
#' @param data Data downloaded from [investmentVolData::get_bea]
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @export clean_bea
#' @importFrom rlang .data
#' @importFrom lubridate %m+%
clean_bea <- function(
    data,
    start_date = "1950-01-01",
    end_date = "2023-01-01") {
  data %>%
    dplyr::filter(.data$LineNumber %in% c(1, 2)) %>%
    tidyr::pivot_longer(
      dplyr::starts_with("DataValue_"),
      names_to = c(".value", "year"),
      names_sep = "_",
      names_transform = list(year = \(x) lubridate::ymd(x, truncated = 2))
    ) %>%
    dplyr::select(.data$LineDescription, .data$DataValue, .data$year, .data$UNIT_MULT) %>%
    tidyr::pivot_wider(
      names_from = .data$LineDescription,
      values_from = .data$DataValue
    ) %>%
    tsibble::as_tsibble()
}

#' Download data the Bureau of Economic Analysis (BEA)
#'
#' `get_bea()` downloads BEA data.
#'
#' @param table_names Name of tables to download from BEA.
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @export get_bea
#' @importFrom rlang .data
get_bea <- function(
    table_names = c(
      #K='FAAt401',    #Table 4.1. Current-Cost Net Stock of Private Nonresidential Fixed Assets
      #D='FAAt404',    #Table 4.4. Current-Cost Depreciation of Private Nonresidential Fixed Assets
      I='FAAt407'     #Table 4.7. Investment in Private Nonresidential Fixed Assets
    ),
    start_date = "1940-01-01",
    end_date = "2023-12-31") {
  start_date <- lubridate::ymd(start_date)
  end_date <- lubridate::ymd(end_date)

  beaSpec <- \(table_name) list(
    'UserID' = Sys.getenv("BEA_API_KEY"),
    'Method' = 'GetData',
    'datasetname' = 'FixedAssets',
    'TableName' = table_name,
    'Frequency' = 'A',
    'Year' = 'X',
    'ResultFormat' = 'json'
  )

  purrr::map(table_names,\(table_name) bea.R::beaGet(beaSpec(table_name)))
}

#' `clean_bea()` Cleans data downloaded from BEA.
#'
#' @param raw_data Data downloaded from [investmentVolData::get_bea]
#' @param lines Vector of character or integers specifying which lines to keep
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @export clean_bea
#' @importFrom rlang .data
#' @examples
#' bea_raw <- get_bea()
#' clean_bea(bea_raw)
#' # Specify lines by name:
#' clean_bea(bea_raw, lines = c(
#' "Private nonresidential fixed assets",
#' "Equipment",
#' "Structures"
#' )
#' )
#' # Specify lines by number:
#' clean_bea(bea_raw, lines = c(1,2,3))
#' # Specify dates:
#' clean_bea(bea_raw,
#'   start_date = "1950-01-01",
#'   end_date = "2023-01-01"
#' )
clean_bea <- function(
    raw_data,
    lines = c(
      "Private nonresidential fixed assets",
      "Equipment",
      "Structures"
    ),
    start_date = "1940-01-01",
    end_date = "2023-12-31"
    ) {
  raw_data %>% purrr::imap(\(value,name)
                          value %>%
                            dplyr::filter(.data$LineNumber %in%
                                            # if lines given by name, convert to line number
                                            if (rlang::is_bare_character(lines)) which(lines %in% .data$LineDescription) else lines
                                          ) %>%
                            tidyr::pivot_longer(
                              dplyr::starts_with("DataValue_"),
                              names_to = c(".value", "date"),
                              names_sep = "_",
                              names_transform = list(date = \(x) lubridate::ymd(x, truncated = 2)),
                              values_transform = \(x) x*10^6/10^9,
                            ) %>%
                            dplyr::select(
                              dplyr::all_of(c("LineDescription", "DataValue", "date", "UNIT_MULT"))
                            ) %>%
                            tidyr::pivot_wider(
                              names_from = "LineDescription",
                              values_from = "DataValue"
                            ) %>%
                            dplyr::rename_with(
                              ~ stringr::str_glue("{(.)}.{name}"),
                              .cols = -c("date")
                            )
  ) %>%
    purrr::reduce(dplyr::inner_join, by = "date")
}

#' `make_vars_bea()` constructs variables from cleaned BEA data.
#'
#' @param clean_data Data returned by [investmentVolData::clean_bea].
#' @param nu a number between 0 and 1 that determines placed-in-service timing for investment
#' nu = 0 corresponds to the end of the year,
#' nu = 1 corresponds to the beginning of the year,
#' nu = 1/2 is what the BEA uses.
#' @export make_vars_bea
#' @importFrom rlang .data
#' @examples
#' bea_raw <- get_bea()
#' bea_clean <- clean_bea(bea_raw)
#' make_vars_bea(bea_clean)
make_vars_bea <- function(
    clean_data,
    nu = 1/2
) {
  list(
    data = clean_data,
  metadata = dplyr::tibble(
      id = c(
        "i3ntotl1es00",
        "i3ntotl1eq00",
        "i3ntotl1st00"
      ),
      label = c(
        # FA = Fixed Asset Tables
        "FA.PNFA.I",
        "FA.Equipment.I",
        "FA.Structures.I"
      ),
      realtime_start = as.character(lubridate::today()),
      realtime_end = as.character(lubridate::today()),
      title = c(
        "Current-Cost Gross Investment of Fixed Assets: Private: Nonresidential",
        "Current-Cost Gross Investment of Fixed Assets: Private: Nonresidential: Equipment",
        "Current-Cost Gross Investment of Fixed Assets: Private: Nonresidential: Structures"
      ),
      observation_start = (clean_data %>%
        dplyr::select("date","Private nonresidential fixed assets.I") %>%
        tidyr::drop_na() %>%
        dplyr::summarize(min_date=min(date)) %>%
        tibble::deframe() %>%
        as.character()),
      observation_end = (clean_data %>%
        dplyr::select("date","Private nonresidential fixed assets.I") %>%
        tidyr::drop_na() %>%
        dplyr::summarize(max_date=max(date)) %>%
        tibble::deframe() %>%
        as.character()),
      frequency = "Annual",
      frequency_short = "A",
      units = "Billions of Dollars",
      units_short = "Bil. of $",
      seasonal_adjustment = "Not Seasonally Adjusted",
      seasonal_adjustment_short = "NSA",
      last_updated = paste0( as.character(lubridate::today())," 00:00:00-06"),
      notes = paste("Fixed Assets Table 4.7", utils::URLencode("https://www.bea.gov/itable/fixed-assets")),
      source = "BEA"
    )
  )

}



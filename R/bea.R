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
      K='FAAt401',    #Table 4.1. Current-Cost Net Stock of Private Nonresidential Fixed Assets
      D='FAAt404',    #Table 4.4. Current-Cost Depreciation of Private Nonresidential Fixed Assets
      I='FAAt407'     #Table 4.7. Investment in Private Nonresidential Fixed Assets
    ),
    start_date = "1950-01-01",
    end_date = "2023-01-01") {
  start_date <- lubridate::ymd(start_date)
  end_date <- lubridate::ymd(end_date)

  beaSpec <- \(table_name) list(
    'UserID' = Sys.getenv("BEA_API_KEY") ,
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
    start_date = "1950-01-01",
    end_date = "2023-01-01"
    ) {
  raw_data %>% purrr::imap(\(value,name)
                          value %>%
                            dplyr::filter(.data$LineNumber %in%
                                            # if lines given by name, convert to line number
                                            if (rlang::is_bare_character(lines)) which(lines %in% .data$LineDescription) else lines
                                          ) %>%
                            tidyr::pivot_longer(
                              dplyr::starts_with("DataValue_"),
                              names_to = c(".value", "year"),
                              names_sep = "_",
                              names_transform = list(year = \(x) lubridate::ymd(x, truncated = 2))
                            ) %>%
                            dplyr::select(
                              dplyr::all_of(c("LineDescription", "DataValue", "year", "UNIT_MULT"))
                            ) %>%
                            tidyr::pivot_wider(
                              names_from = "LineDescription",
                              values_from = "DataValue"
                            ) %>%
                            dplyr::rename_with(
                              ~ stringr::str_glue("{(.)}.{name}"),
                              .cols = -c("year")
                            ) %>%
                            tsibble::as_tsibble()
  ) %>%
    purrr::reduce(dplyr::inner_join, by = "year")
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
  clean_data %>%
    dplyr::mutate(
  K = .data$Equipment.K + .data$Structures.K,
  lag.K = dplyr::lag(.data$K, order_by = .data$year),
  lag.Equipment.K = dplyr::lag(.data$Equipment.K, order_by = .data$year),
  lag.Structures.K = dplyr::lag(.data$Structures.K, order_by = .data$year),
  I = .data$Equipment.I+.data$Structures.I,
  IK = .data$I/.data$lag.K,
  IK.Equipment = .data$Equipment.I/ .data$lag.Equipment.K,
  IK.Structures = .data$Structures.I/ .data$lag.Structures.K,
  IK = .data$I/.data$lag.K,
  depreciation.Equipment = .data$Equipment.D/ (.data$lag.Equipment.K + nu * .data$Equipment.I),
  depreciation.Structures = .data$Structures.D/ (.data$lag.Structures.K + nu * .data$Structures.I),
  depreciation.K = (.data$lag.Equipment.K/.data$lag.K) * .data$depreciation.Equipment + (.data$lag.Structures.K/.data$lag.K) * .data$depreciation.Structures
)
}


## Notes
# Kstruct,t is the current-cost net stock of non-residential structures (from Fixed assets Table 4.1)
# We use the data provided in Fixed Assets tables 4.1 (for the net stocks at current cost) and 4.7 (for the gross investment flows at current cost). For physical capital, we use the sum of equipment and structures, and for intangible capital
#Dtc is the estimate of current-cost depreciation reported in Fixed Assets table 4.4,
#current-cost average depreciation estimate

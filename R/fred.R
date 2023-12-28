#' Download data from the Federal Reserve Bank of St. Louis FRED
#'
#' `get_fred()` downloads FRED data.
#'
#' @param series_id A single character string, a character vector or tibble
#'   representing a single (or multiple) FRED series
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @param ... Additional arguments passed to [fredr::fredr].
#' @export get_fred
get_fred <- function(
    series_id= c(
      "GDPC1",
      "PNFI",

      ##National Income and Product Accounts (NIPA)
      #Private fixed investment: Nonresidential: Structures
      "B009RC1Q027SBEA", #Seasonally Adjusted, Quarterly
      "B009RC1A027NBEA", #Annual"
      "NA000339Q", #Not Seasonally Adjusted, Quarterly

      #Gross Private Domestic Investment: Fixed Investment: Nonresidential: Equipment
      "Y033RC1Q027SBEA",#Seasonally Adjusted, Quarterly
      "Y033RC1A027NBEA", #Annual
      "NA000340Q",#Not Seasonally Adjusted, Quarterly

      ##Fixed Assets Accounts Tables (FA)
      #Table 4.1.
      #Current-Cost Net Stock of Fixed Assets: Private: Nonresidential: Structures
      "K1NTOTL1ST000",#Annual
      #Current-Cost Net Stock of Fixed Assets: Private: Nonresidential: Equipment
      "K1NTOTL1EQ000",#Annual
      #Table 4.4.
      #Current-Cost Depreciation of Fixed Assets: Private: Nonresidential: Equipment
      "M1NTOTL1EQ000",
      #Current-Cost Depreciation of Fixed Assets: Private: Nonresidential: Structures
      "M1NTOTL1ST000"
    ),
    start_date = "1940-01-01",
    end_date = "2023-12-31",
    ...) {
  # user-friendly names
  lookup <- c(
    gdp = "GDPC1",
    pnfi = "PNFI",
    NIPA.Structures.I.SA = "B009RC1Q027SBEA",
    NIPA.Structures.I.NSA = "NA000339Q",
    NIPA.Structures.I = "B009RC1A027NBEA",
    NIPA.Equipment.I.SA = "Y033RC1Q027SBEA",
    NIPA.Equipment.I.NSA = "NA000340Q",
    NIPA.Equipment.I = "Y033RC1A027NBEA",
    FA.Structures.K = "K1NTOTL1ST000",
    FA.Equipment.K = "K1NTOTL1EQ000",
    FA.Equipment.D = "M1NTOTL1EQ000",
    FA.Structures.D = "M1NTOTL1ST000"
  )
  list(
    data = purrr::pmap_dfr(
      .l = list(
        series_id = series_id
      ),
      .f = ~ fredr::fredr(
        series_id = .x,
        observation_start = as.Date(start_date),
        observation_end = as.Date(end_date)
        )
    ) %>%
      dplyr::left_join(
        tibble::as_tibble_col(lookup, column_name = "series_id") %>% tibble::add_column(label = names(lookup)),
        by = c("series_id"),
        relationship = "many-to-one"
      )
    ,
    metadata = purrr::map_dfr(
      .x = series_id,
      .f = \(x) fredr::fredr_series(x)
    ) %>% dplyr::rows_patch(
      tibble::tibble(
        id=c("K1NTOTL1ST000","K1NTOTL1EQ000","M1NTOTL1EQ000","M1NTOTL1ST000"),
        notes = c(
          paste("Fixed Assets Table 4.1", utils::URLencode("https://www.bea.gov/itable/fixed-assets")),
          paste("Fixed Assets Table 4.1", utils::URLencode("https://www.bea.gov/itable/fixed-assets")),
          paste("Fixed Assets Table 4.4", utils::URLencode("https://www.bea.gov/itable/fixed-assets")),
          paste("Fixed Assets Table 4.4", utils::URLencode("https://www.bea.gov/itable/fixed-assets"))
        )
      )
    ) %>%
      dplyr::mutate(
        source = "FRED"
      ) %>%
      dplyr::left_join(
        tibble::as_tibble_col(lookup, column_name = "id") %>% tibble::add_column(label = names(lookup)),
        by = c("id")
      ) %>%
      dplyr::relocate("id", "label")
  )
}

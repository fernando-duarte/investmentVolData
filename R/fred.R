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
      "GDPPOT",
      "CPIAUCNS",
      "PNFI",

      ##National Income and Product Accounts
      #Private fixed investment: Nonresidential: Structures
      "B009RC1Q027SBEA", #Seasonally Adjusted, Quarterly
      "B009RC1A027NBEA", #Annual"
      "NA000339Q", #Not Seasonally Adjusted, Quarterly

      #Gross Private Domestic Investment: Fixed Investment: Nonresidential: Equipment
      "Y033RC1Q027SBEA",#Seasonally Adjusted, Quarterly
      "Y033RC1A027NBEA", #Annual
      "NA000340Q",#Not Seasonally Adjusted, Quarterly

      ##Fixed Assets Accounts Tables
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
    start_date = "1950-01-01",
    end_date = "2023-01-01",
    ...) {
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
    ),
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
      )

  )
}

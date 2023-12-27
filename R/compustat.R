#' Download data from Compustat using Wharton's WRDS
#'
#' `get_compustat()` downloads data from Compustat
#'
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @export get_compustat
#' @importFrom rlang .data
get_compustat <- function(
    start_date = "1974-01-01",
    end_date = "2023-12-01") {

  wrds <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    host = "wrds-pgdata.wharton.upenn.edu",
    dbname = "wrds",
    port = 9737,
    sslmode = "require",
    user = Sys.getenv("WRDS_USER"),
    password = Sys.getenv("WRDS_PASSWORD")
  )

  funda_db <- dplyr::tbl(wrds, dbplyr::in_schema("comp", "funda"))
  # fundq_db <- dplyr::tbl(wrds, dbplyr::in_schema("comp", "fundq"))

  compustat <- funda_db |>
    dplyr::filter(
        .data$consol == "C" &
        .data$indfmt == "INDL" &
        .data$datafmt == "STD" &
        .data$popsrc == "D" &
        .data$datadate >= start_date & .data$datadate <= end_date
      ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          "datadate", # Date
          "fyr", # Fiscal Year-end Month
          "gvkey", # Firm identifier
          "conm", # Company Name
          "exchg",# Stock Exchange Code
          "fic", # ISO Country Code - Incorporation
          "sich", # Standard Industry Classification Code - Historical
          "adrr", # American Depositary Receipt (ADR) Ratio
          "bkvlps", # Book Value Per Share
          "emp", # Employees
          "act", # Float	Current Assets - Total

          "capx", # Capital Expenditures
          "capxv", # Capital Expenditures Property, Plant and Equipment (Schedule V)
          "ppegt", # Property, Plant and Equipment - Total (Gross)
          "ppent", # Property, Plant and Equipment - Total (Net)
          "ppevbb", # Property, Plant and Equipment - Beginning Balance (Schedule V)
          "ppeveb", # Property, Plant, and Equipment - Ending Balance (Schedule V)
          "udpfa", # Depreciation of Fixed Assets
          "dfxa", # Depreciation of Tangible Fixed Assets
          "lct", # Float	Current Liabilities - Total
          "che", # Cash and Short-Term Investments
          "gdwl", # Goodwill
          "sppe", #	Sale of Property

          # needed to compute Fama-French's book equity
          "seq", # Stockholders' equity - Parent
          "ceq", # Common/Ordinary Equity - Total
          "pstk", # Preferred/Preference Stock (Capital) - Total
          "at", # Assets - Total
          "lt", # Liabilities - Total
          "txditc", # Deferred Taxes and Investment Tax Credit
          "txdb", # Deferred Taxes (Balance Sheet)
          "itcb", # Investment Tax Credit (Balance Sheet)
          "pstkrv", # Preferred Stock - Redemption Value
          "pstkl", # Preferred Stock - Liquidating Value

          # needed to compute Fama-French's operating profitability
          "sale",  # Sales/Turnover (Net)
          "cogs", # Cost of Goods Sold
          "xsga", # Selling, General and Administrative Expense
          "xint" # Interest and Related Expense - Total

          # "oancf", # Operating Activities - Net Cash Flow
          # "depc", # Depreciation and Depletion (Cash Flow)
        )
      )
    ) |>
    dplyr::collect()
}

#' `clean_compustat()` cleans Compustat data.
#'
#' @param data Data downloaded from [investmentVolData::get_compustat]
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @export clean_compustat
#' @importFrom rlang .data
#' @importFrom lubridate %m+%
clean_compustat <- function(
    data,
    start_date = "1974-01-01",
    end_date = "2023-01-01") {

  compustat <- data |>
    # keep only the last available information for each firm-year group
    dplyr::mutate(year = lubridate::year(.data$datadate)) |>
    dplyr::group_by(.data$gvkey, .data$year) |>
    dplyr::filter(.data$datadate == max(.data$datadate)) |>
    dplyr::ungroup()
  compustat <- compustat |>
    dplyr::mutate(
      # create book equity as in https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/variable_definitions.html
      be = dplyr::coalesce(.data$seq, .data$ceq + .data$pstk, .data$at - .data$lt) +
        dplyr::coalesce(.data$txditc, .data$txdb + .data$itcb, 0) -
        dplyr::coalesce(.data$pstkrv, .data$pstkl, .data$pstk, 0),
      be = dplyr::if_else(.data$be <= 0, as.numeric(NA), .data$be),
      # create operating profitability as in https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/variable_definitions.html
      op = (.data$sale - dplyr::coalesce(.data$cogs, 0) -
              dplyr::coalesce(.data$xsga, 0) - dplyr::coalesce(.data$xint, 0)) / .data$be,
    ) |>
    # create investment ratio as in https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/variable_definitions.html
    dplyr::left_join(
      compustat |>
          dplyr::mutate(at_lag = .data$at, year = .data$year + 1) |>
          dplyr::select(dplyr::all_of(c("gvkey", "year", "at_lag"))),
      by = c("gvkey", "year")
    ) |>
      dplyr::mutate(
        inv = .data$at / .data$at_lag - 1,
        inv = dplyr::if_else(.data$at_lag <= 0, as.numeric(NA), .data$inv)
      ) |>
    dplyr::filter(
      .data$fic == "USA" &
       dplyr::if_all(c("emp", "sale", "at", "act", "lct", "ppent", "ppegt", "che", "gdwl"), ~ . >= 0) &
        # drop any observation which we can identify as an American Depository Institution (ADR)
        # drop observations with non-standard accounting
        purrr::reduce(
          purrr::map(
            c(
              # adr suffixes
              "-ADR",
              "-ADS",
              "ADR NEW",
              "AM SHARES",
              "AMER SH",
              "NY REG",
              "NY SH",
              "NY SHARES",
              "SPON ADR",
              "-REDH",
              "-PRE FASB",
              "-PRO FORMA"
            ),
            \(suffix) stringr::str_detect(.data$conm, suffix, negate = TRUE) #stringr::str_ends(.data$conm, suffix, negate = TRUE)
          ),
          `&`
        )
    ) |>
    dplyr::filter(is.na(.data$adrr)) |>
    dplyr::select(!c("conm","adrr")) |>
    dplyr::rename(dplyr::all_of(c(date = "datadate"))) |>
    dplyr::mutate(
      year = lubridate::year(.data$date),
      quarter = lubridate::quarter(.data$date),
      month = lubridate::month(.data$date),
    )
}


#' `get_sic()` downloads link table between gvkey and sic
#'
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @export get_sic
#' @importFrom rlang .data
get_sic <- function(
    start_date = "1974-01-01",
    end_date = "2023-01-01") {

  wrds <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    host = "wrds-pgdata.wharton.upenn.edu",
    dbname = "wrds",
    port = 9737,
    sslmode = "require",
    user = Sys.getenv("WRDS_USER"),
    password = Sys.getenv("WRDS_PASSWORD")
  )

  funda_db <- dplyr::tbl(wrds, dbplyr::in_schema("comp", "namesd"))
  sic <- funda_db |>
    dplyr::select(
      dplyr::all_of(
        c(
          "sic", # Standard Industry Classification Code
          "gvkey" # Global Company Key
        )
      )
    ) |>
    dplyr::distinct() |>
    dplyr::collect()
}


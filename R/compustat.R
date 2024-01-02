#' Download data from Compustat using Wharton's WRDS
#'
#' `get_compustat()` downloads data from Compustat
#'
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @param compustat_freq Either "annual" (the default) or "quarterly"
#' @export get_compustat
#' @importFrom rlang .data
get_compustat <- function(
    start_date = "1940-01-01",
    end_date = "2023-12-31",
    compustat_freq = c("annual", "quarterly")
    ) {
  wrds <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    host = "wrds-pgdata.wharton.upenn.edu",
    dbname = "wrds",
    port = 9737,
    sslmode = "require",
    user = Sys.getenv("WRDS_USER"),
    password = Sys.getenv("WRDS_PASSWORD")
  )

  if (compustat_freq=="annual"){
  funda_db <- dplyr::tbl(wrds, dbplyr::in_schema("comp", "funda"))

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
          "conm", # Company Name
          "exchg",# Stock Exchange Code
          "fic", # ISO Country Code - Incorporation
          "gvkey", # Firm identifier

          #Variables in Fundamentals Annual only
          "adrr", # American Depositary Receipt (ADR) Ratio
          "bkvlps", # Book Value Per Share (Fundamentals Annual only)
          "emp", # Employees (Fundamentals Annual only)
          "act", # Float	Current Assets - Total
          "sich", # Standard Industry Classification Code - Historical
          "capx", # Capital Expenditures
          "sppe", #	Sale of Property
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
          "at", # Assets - Total

          # needed to compute Fama-French's book equity
          "seq", # Stockholders' equity - Parent
          "ceq", # Common/Ordinary Equity - Total
          "pstk", # Preferred/Preference Stock (Capital) - Total
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

  } else if (compustat_freq=="quarterly"){

    fundq_db <- dplyr::tbl(wrds, dbplyr::in_schema("comp", "fundq"))

    compustat <- fundq_db |>
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
            "conm", # Company Name
            "exchg",# Stock Exchange Code
            "fic", # ISO Country Code - Incorporation
            "gvkey", # Firm identifier

            # Variables in Fundamentals Quarterly only
            "actq", # Float	Current Assets - Total
            "adrrq", # American Depositary Receipt (ADR) Ratio
            "capxy", # Capital Expenditures Year-to-Date
            "sppey", #	Sale of Property Year-to-Date
            "ppegtq", # Property, Plant and Equipment - Total (Gross)
            "ppentq", # Property, Plant and Equipment - Total (Net)
            "lctq", # Float	Current Liabilities - Total
            "cheq", # Cash and Short-Term Investments
            "gdwlq", # Goodwill
            "atq" # Assets - Total
          )
        )
      ) |>
      dplyr::rename(adrr = .data$adrrq) |>
      dplyr::collect()
  }
}

#' `clean_compustat()` cleans Compustat data.
#'
#' @param data Data downloaded from [investmentVolData::get_compustat]
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @param compustat_freq Either "annual" (the default) or "quarterly"
#' @export clean_compustat
#' @importFrom rlang .data
#' @importFrom lubridate %m+%
clean_compustat <- function(
    data,
    start_date = "1940-01-01",
    end_date = "2023-12-31",
    compustat_freq = c("annual", "quarterly")
    ) {

  compustat <- data |>
    dplyr::filter( (.data$fic == "USA")  ) |>
    dplyr::filter( .data$exchg>10 & .data$exchg <= 20  ) |> #US exchanges
    dplyr::rename(dplyr::all_of(c(date = "datadate"))) |>
    dplyr::mutate(
      year = lubridate::year(.data$date),
      quarter = lubridate::quarter(.data$date),
      month = lubridate::month(.data$date),
    ) |>
    # drop any observation which we can identify as an American Depository Institution (ADR) or
    # that has non-standard accounting
    dplyr::filter(
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
              # non-standard accounting
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
    dplyr::select(!c("conm","adrr"))


  if (compustat_freq=="annual"){

    # keep only the last available information for each firm-year group
    # to do: check small number of observations that are dropped, is there more
    # than one firm-year observation?
    compustat <- compustat |>
      dplyr::group_by(.data$gvkey, .data$year) |>
      dplyr::filter(.data$date == max(.data$date)) |>
      dplyr::ungroup()

    # create firm-level investment, capital
    compustat <- compustat |>
    dplyr::mutate(
      firmI = .data$capx - .data$sppe, #Gross investment in physical capital is computed as capital expenditures minus sales of property, plant and equipment
      firmK_gross = dplyr::na_if(.data$ppegt,0), # Property, Plant and Equipment - Total (Gross)
      firmK_net = dplyr::na_if(.data$ppent,0), # Property, Plant and Equipment - Total (Net)
    )

    # and their lags
    compustat <- compustat |>
      dplyr::left_join(
        compustat |>
          dplyr::mutate(
            firmI_lag = .data$firmI,
            firmK_gross_lag = .data$firmK_gross,
            firmK_net_lag = .data$firmK_net,
            year = .data$year + 1
            ) |>
          dplyr::select(dplyr::all_of(c("gvkey", "year", "firmI_lag","firmK_gross_lag","firmK_net_lag"))),
        by = c("gvkey", "year")
      )
    #  tidyr::drop_na("firmK_gross","firmK_net","firmI","firmK_gross_lag","firmK_net_lag")

    # fama french variables
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
        ) %>%
      dplyr::mutate(
          date = lubridate::floor_date(date, unit="quarter")
        )

  } else if (compustat_freq=="quarterly"){

    # keep only the last available information for each firm-quarter group
    # compustat <- compustat |>
    #   dplyr::group_by(.data$gvkey, .data$quarter) |>
    #   dplyr::filter(.data$date == max(.data$date)) |>
    #   dplyr::ungroup()

    # create quarterly flow variables from year-to-date flow variables
    compustat <- compustat |>
      # create fiscal year and fiscal quarter
      # to do: can we download this from compustat directly?
      dplyr::mutate(
        fiscal_start = (.data$fyr + 1) %% 12,
        fiscal_start = dplyr::if_else(.data$fiscal_start == 0, 12, .data$fiscal_start),
      ) |>
      dplyr::mutate(
        fiscal_quarter = as.character(
          unlist(
            purrr::pmap(
              list(x=.data$date, fiscal_start = .data$fiscal_start),
              function(x,fiscal_start) lubridate::quarter(x,type = "year.quarter", fiscal_start = fiscal_start)
            )
          )
        )
      ) |>
      tidyr::separate(.data$fiscal_quarter, into = c("fiscal_y","fiscal_q"),remove=FALSE) |>
      dplyr::mutate(dplyr::across(c("fiscal_y","fiscal_q"),as.integer)) |>
      # create all fiscal quarters for each firm-year
      dplyr::group_by(.data$gvkey,.data$fiscal_y) |>
      dplyr::arrange(.data$date) |>
      tidyr::complete(fiscal_q = 1:4)

      # create quarterly capxy, sppey from year-to-date capxy, sppey
      compustat <- compustat |>
      ytd_to_q("capxy", "capx") |>
      ytd_to_q("sppey", "sppe") |>
      dplyr::ungroup()

      # add calendar dates for newly created fiscal quarters
      compustat <- compustat |>
      dplyr::mutate(shift_q = (12-.data$fyr) %/% 3 ) |>
      dplyr::group_by(.data$gvkey) |>
      tidyr::fill("shift_q", .direction = "downup") |>
      tidyr::unite("fiscal_quarter",c("fiscal_y","fiscal_q"),remove=FALSE,sep=".") |>
      dplyr::mutate(
        fiscal_date = lubridate::yq(.data$fiscal_quarter),
        date = .data$fiscal_date - months(3*.data$shift_q),
        date = lubridate::ceiling_date(.data$date,unit="quarter") - lubridate::days(1),
        date = lubridate::floor_date(.data$date, unit="quarter"),
        year = lubridate::year(.data$date),
        quarter = lubridate::quarter(.data$date),
        month = lubridate::month(.data$date),
      ) |>
      dplyr::select(!c("fiscal_start","fiscal_quarter","fiscal_y","fiscal_q","shift_q","fiscal_date"))

    # create firm-level investment, capital
    compustat <- compustat |>
      dplyr::mutate(
        firmI = .data$capx - .data$sppe, #Gross investment in physical capital is computed as capital expenditures minus sales of property, plant and equipment
        firmK_gross = dplyr::na_if(.data$ppegtq,0), # Property, Plant and Equipment - Total (Gross)
        firmK_net = dplyr::na_if(.data$ppentq,0), # Property, Plant and Equipment - Total (Net)
      )

    # and their lags
    compustat <- compustat |>
      dplyr::group_by(.data$gvkey,.data$date) |>
      dplyr::filter(dplyr::n()==1) |> # to do: check that duplicates occur when fiscal year changes, and that they are treated correctly when creating quarterly capx
      dplyr::ungroup() |>
      dplyr::left_join(
        compustat |>
          dplyr::mutate(
            firmI_lag = .data$firmI,
            firmK_gross_lag = .data$firmK_gross,
            firmK_net_lag = .data$firmK_net,
            date = .data$date %m+% months(3)
          ) |>
          dplyr::select(dplyr::all_of(c("gvkey", "date", "firmI_lag","firmK_gross_lag","firmK_net_lag"))),
        by = c("gvkey", "date")
      )
    #  tidyr::drop_na("firmK_gross","firmK_net","firmI","firmK_gross_lag","firmK_net_lag")

  }
}

#' `get_sic()` downloads link table between gvkey and sic
#'
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @export get_sic
#' @importFrom rlang .data
get_sic <- function(
    start_date = "1940-01-01",
    end_date = "2023-12-31") {

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

#' `ytd_to_q()` constructs quarterly flow from year-to-date flow taking into
#' account different fiscal year ends and missing observations
#'
#' @param data A tibble of four observations for a fiscal year of one firm
#' @param ytd_var The year-to-date flow variable
#' @param q_var_name The name of the new quarterly flow variable created
#' @param fiscal_q The fiscal quarter of the firm (with values 1,2,3 or 4)
#' @importFrom rlang .data
ytd_to_q <- function(data, ytd_var, q_var_name, fiscal_q="fiscal_q") {
  data |>
    dplyr::arrange(.data[[fiscal_q]]) |>
    dplyr::mutate(
      #linearly interpolate missing year-to-date (ytd) flow
      fill_ = zoo::na.approx(.data[[ytd_var]], na.rm = FALSE),
      # split flow equally across quarters with missing ytd flow
      first_nonmiss_index = which(!is.na(.data$fill_))[1],
      first_fill_ = dplyr::first(.data$fill_,na_rm =TRUE)/.data$first_nonmiss_index,
      filled_invest = c(dplyr::first(.data$fill_),diff(.data$fill_)),
      # if ytd flow has trailing quarters with NA, make quarterly flow NA for the trailing quarters
      num_trailing_NA = length( .data[[ytd_var]] ) - length(zoo::na.trim(.data[[ytd_var]], sides = "right")),
      "{q_var_name}" := dplyr::case_when(
        .data$num_trailing_NA <= 4 - .data[[fiscal_q]] ~ dplyr::coalesce(.data$filled_invest,.data$first_fill_),
        .default = NA
      )
    ) |> dplyr::select(!c("fill_","first_nonmiss_index","first_fill_","filled_invest","num_trailing_NA"))
}


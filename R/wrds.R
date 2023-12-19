#'@export get_wrds
#'@importFrom rlang .data
get_wrds <- function(
    start_date = "1950-01-01",
    end_date = "2023-01-01"
  ){
  start_date <- lubridate::ymd(start_date)
  end_date <- lubridate::ymd(end_date)
  wrds <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    host = "wrds-pgdata.wharton.upenn.edu",
    dbname = "wrds",
    port = 9737,
    sslmode = "require",
    user = Sys.getenv("WRDS_USER"),
    password = Sys.getenv("WRDS_PASSWORD")
  )

  msf_db <- dplyr::tbl(wrds, dbplyr::in_schema("crsp", "msf"))
  msenames_db <- dplyr::tbl(wrds, dbplyr::in_schema("crsp", "msenames"))
  msedelist_db <- dplyr::tbl(wrds, dbplyr::in_schema("crsp", "msedelist"))

  #crsp_monthly
  msf_db |>
    dplyr::filter(.data$date >= start_date & .data$date <= end_date) |>
    dplyr::inner_join(
      msenames_db |>
        dplyr::filter(.data$shrcd %in% c(10, 11)) |>
        dplyr::select(.data$permno, .data$exchcd, .data$siccd, .data$namedt, .data$nameendt),
      by = c("permno")
    ) |>
    dplyr::filter(.data$date >= .data$namedt & .data$date <= .data$nameendt) |>
    dplyr::mutate(month = lubridate::floor_date(.data$date, "month")) |>
    dplyr::left_join(
      msedelist_db |>
        dplyr::select(.data$permno, .data$dlstdt, .data$dlret, .data$dlstcd) |>
        dplyr::mutate(month = lubridate::floor_date(.data$dlstdt, "month")),
      by = c("permno", "month")
    ) |>
    dplyr::select(
      .data$permno, # Security identifier
      .data$date, # Date of the observation
      .data$month, # Month of the observation
      .data$ret, # Return
      .data$shrout, # Shares outstanding (in thousands)
      .data$altprc, # Last traded price in a month
      .data$exchcd, # Exchange code
      .data$siccd, # Industry code
      .data$dlret, # Delisting return
      .data$dlstcd # Delisting code
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      month = lubridate::ymd(.data$month),
      shrout = .data$shrout * 1000
    )
}
#'@export clean_wrds
#'@importFrom rlang .data
#' @importFrom lubridate %m+%
clean_wrds <- function(
    data,
    start_date = "1950-01-01",
    end_date = "2023-01-01"
){

crsp_monthly <- data |>
  dplyr::mutate(
    mktcap = abs(.data$shrout * .data$altprc) / 10^6, #in millions of USD
    mktcap = dplyr::na_if(.data$mktcap, 0)
  )

mktcap_lag <- crsp_monthly |>
  dplyr::mutate(month = .data$month %m+% months(1)) |>
  dplyr::select(.data$permno, .data$month, mktcap_lag = .data$mktcap)

crsp_monthly <- crsp_monthly |>
  dplyr::left_join(.data$mktcap_lag, by = c("permno", "month"))

crsp_monthly <- crsp_monthly |>
  dplyr::mutate(exchange = dplyr::case_when(
    .data$exchcd %in% c(1, 31) ~ "NYSE",
    .data$exchcd %in% c(2, 32) ~ "AMEX",
    .data$exchcd %in% c(3, 33) ~ "NASDAQ",
    .default = "Other"
  ))

crsp_monthly <- crsp_monthly |>
  dplyr::mutate(industry = dplyr::case_when(
    .data$siccd >= 1 & .data$siccd <= 999 ~ "Agriculture",
    .data$siccd >= 1000 & .data$siccd <= 1499 ~ "Mining",
    .data$siccd >= 1500 & .data$siccd <= 1799 ~ "Construction",
    .data$siccd >= 2000 & .data$siccd <= 3999 ~ "Manufacturing",
    .data$siccd >= 4000 & .data$siccd <= 4899 ~ "Transportation",
    .data$siccd >= 4900 & .data$siccd <= 4999 ~ "Utilities",
    .data$siccd >= 5000 & .data$siccd <= 5199 ~ "Wholesale",
    .data$siccd >= 5200 & .data$siccd <= 5999 ~ "Retail",
    .data$siccd >= 6000 & .data$siccd <= 6799 ~ "Finance",
    .data$siccd >= 7000 & .data$siccd <= 8999 ~ "Services",
    .data$siccd >= 9000 & .data$siccd <= 9999 ~ "Public",
    TRUE ~ "Missing"
  ))

  crsp_monthly <- crsp_monthly |>
    dplyr::mutate(ret_adj = dplyr::case_when(
      is.na(.data$dlstcd) ~ ret,
      !is.na(.data$dlstcd) & !is.na(.data$dlret) ~ .data$dlret,
      .data$dlstcd %in% c(500, 520, 580, 584) |
        (.data$dlstcd >= 551 & .data$dlstcd <= 574) ~ -0.30,
      .data$dlstcd == 100 ~ .data$ret,
      TRUE ~ -1
    )) |>
    dplyr::select(-c(.data$dlret, .data$dlstcd))
}

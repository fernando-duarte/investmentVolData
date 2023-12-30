#' Download data from CRSP using Wharton's WRDS
#'
#' `get_crsp()` downloads data from CRSP
#'
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @export get_crsp
#' @importFrom rlang .data
get_crsp <- function(
    start_date = "1940-01-01",
    end_date = "2023-12-31") {
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

  # crsp_monthly
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
        dplyr::select(.data$permno, .data$dlstdt, .data$dlret, .data$dlstcd, .data$dlprc) |>
        dplyr::mutate(month = lubridate::floor_date(.data$dlstdt, "month")),
      by = c("permno", "month")
    ) |>
    dplyr::select(
      .data$permno, # Security identifier
      .data$date, # Date of the observation
      .data$month, # Month of the observation
      .data$ret, # Return
      .data$shrout, # Shares outstanding (in thousands)
      .data$prc, # Closing price or the negative bid/ask average
      .data$altprc, # Last traded price in a month
      .data$exchcd, # Exchange code
      .data$siccd, # Industry code
      .data$dlret, # Delisting return
      .data$dlstcd, # Delisting code
      .data$dlprc # Delisting price
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      month = lubridate::ymd(.data$month),
      shrout = .data$shrout * 1000
    )
}

#' `clean_crsp()` downloads WRDS data.
#'
#' @param data Data downloaded from [investmentVolData::get_crsp]
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @param rf Risk-free rate to compute excess returns. Defaults to `NULL`, in which
#' case excess returns are not computed.
#' @export clean_crsp
#' @importFrom rlang .data
#' @importFrom lubridate %m+%
clean_crsp <- function(
    data,
    start_date = "1940-01-01",
    end_date = "2023-12-31",
    rf = NULL) {
  crsp_monthly <- data |>
    dplyr::mutate(
      mktcap = abs(.data$shrout * dplyr::coalesce(.data$prc, .data$altprc, .data$dlprc)) / 10^6, # in millions of USD
      mktcap = dplyr::na_if(.data$mktcap, 0)
    )

  mktcap_lag <- crsp_monthly |>
    dplyr::mutate(month = .data$month %m+% months(1)) |>
    dplyr::select(.data$permno, .data$month, mktcap_lag = .data$mktcap)

  crsp_monthly <- crsp_monthly |>
    dplyr::left_join(mktcap_lag, by = c("permno", "month")) |>
    tidyr::drop_na("mktcap", "mktcap_lag")

  crsp_monthly <- crsp_monthly |>
    dplyr::mutate(exchange = dplyr::case_when(
      .data$exchcd %in% c(1, 31) ~ "NYSE",
      .data$exchcd %in% c(2, 32) ~ "AMEX",
      .data$exchcd %in% c(3, 33) ~ "NASDAQ",
      .default = "Other"
    )) |>
    dplyr::filter(.data$exchcd != "Other")

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

  if(!is.null(rf)){
    crsp_monthly <- crsp_monthly |>
      dplyr::left_join(rf,
            by = "month"
      ) |>
      dplyr::filter(.data$ret_adj > -1) |>
      dplyr::mutate(
        ret_excess = .data$ret_adj - rf,
        #ret_excess = pmax(.data$ret_excess, -1),
        log_ret = log(1+.data$adj_ret),
        log_ret_excess = log(1+.data$ret_excess)
      ) |>
      tidyr::drop_na("ret_excess")
  }
  crsp_monthly
}

#' `get_crsp_daily()` downloads daily frequency data from CRSP
#'
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @param rf Risk-free rate at daily frequency to compute excess returns.
#' Defaults to `NULL`, in which case excess returns are not computed.
#' @param permnos Vector of permnos to download. Defaults to `NULL`, in which case all
#' permnos are downloaded
#' @param batch_size How many observations to download at a time. Defaults to 500.
#' @export get_crsp_daily
#' @importFrom rlang .data
get_crsp_daily <- function(
    start_date = "1940-01-01",
    end_date = "2023-12-31",
    rf = NULL,
    permnos = NULL,
    batch_size = 500) {

  wrds <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    host = "wrds-pgdata.wharton.upenn.edu",
    dbname = "wrds",
    port = 9737,
    sslmode = "require",
    user = Sys.getenv("WRDS_USER"),
    password = Sys.getenv("WRDS_PASSWORD")
  )
  on.exit(RPostgres::dbDisconnect(wrds))

  if (is.null(permnos)) {
    rs <- RPostgres::dbSendQuery(wrds, "select
                   date, permno, ret, shrout, prc
                   from CRSP.DSF WHERE date > $1 AND date < $2")
    RPostgres::dbBind(rs, list(start_date, end_date))
    crsp_daily_sub <- RPostgres::dbFetch(rs, n = batch_size)
    while (!RPostgres::dbHasCompleted(rs)) {
      crsp_daily_sub <- dplyr::bind_rows(
        crsp_daily_sub,
        RPostgres::dbFetch(rs, n = batch_size) |> tidyr::drop_na()
      )
      if ((nrow(crsp_daily_sub)%%batch_size)==0) {
        cat(nrow(msedelist_sub), "rows downloaded for daily crsp data\n")
      }
    }
    rs_delist <- RPostgres::dbSendQuery(wrds, "select
                    permno, dlstdt, dlret,
                    from CRSP.DSEDELIST  WHERE date > $1 AND date  < $2") # CRSP.MSEDELIST CRSP.DSEDELIST
    RPostgres::dbBind(rs_delist, params = list(start = start_date, end = end_date))
    msedelist_sub <- RPostgres::dbFetch(rs_delist, n = batch_size)
    while (!RPostgres::dbHasCompleted(rs_delist)) {
      msedelist_sub <- dplyr::bind_rows(
        msedelist_sub,
        RPostgres::dbFetch(rs_delist, n = batch_size) |> tidyr::drop_na()
      )
      if ((nrow(msedelist_sub)%%batch_size)==0) {
        cat(nrow(msedelist_sub), "rows downloaded for delisting information\n")
      }
    }
  } else {
    dsf_db <- dplyr::tbl(wrds, dbplyr::in_schema("crsp", "dsf"))
    msedelist_db <- dplyr::tbl(wrds, dbplyr::in_schema("crsp", "msedelist"))
    batches <- ceiling(length(permnos) / batch_size)
    permno_batch <- permnos[1:min(batch_size, length(permnos))]
    crsp_daily_sub <-
      dsf_db |>
      dplyr::filter(.data$permno %in% permno_batch &
                      .data$date >= start_date & .data$date <= end_date) |>
      dplyr::select("date", "permno", "ret", "shrout", "prc") |>
      dplyr::collect() |>
      tidyr::drop_na()
    msedelist_sub <- msedelist_db |>
      dplyr::filter(.data$permno %in% permno_batch) |>
      dplyr::select("permno", "dlstdt", "dlret") |>
      dplyr::collect() |>
      tidyr::drop_na()
    if (batches > 1){
    for (j in 2:batches) {
      permno_batch <- permnos[
        ((j - 1) * batch_size + 1):min(j * batch_size, length(permnos))
      ]
      crsp_daily_sub <- dplyr::bind_rows(
        crsp_daily_sub,
        dsf_db |>
          dplyr::filter(.data$permno %in% permno_batch &
                          .data$date >= start_date & .data$date <= end_date) |>
          dplyr::select("date", "permno", "ret", "shrout", "prc") |>
          dplyr::collect() |>
          tidyr::drop_na()
      )
      if (nrow(crsp_daily_sub) > 0) {
        msedelist_sub <- dplyr::bind_rows(
          msedelist_sub,
          msedelist_db |>
            dplyr::filter(.data$permno %in% permno_batch) |>
            dplyr::select("permno", "dlstdt", "dlret") |>
            dplyr::collect() |>
            tidyr::drop_na()
        )
      }
      cat("Batch", j, "out of", batches, "done (", scales::percent(j / batches), ")\n")
    }
    }
  }

  # Adjustment for delistings:
  # 1. If dlret exists for dlstdt, replace ret with dlret at dlstdt
  # 2. If dlret does not exist for dlstdt, add new observation with a delisting return
  # 3. Drop observations after the delisting date
  crsp_daily_sub <- crsp_daily_sub |>
    dplyr::left_join(msedelist_sub, by = c("permno", "date" = "dlstdt")) |>
    dplyr::bind_rows(msedelist_sub |>
        dplyr::anti_join(crsp_daily_sub,
        by = c("permno", "dlstdt" = "date")
      )) |>
    dplyr::mutate(
      ret = dplyr::if_else(!is.na(.data$dlret), .data$dlret, .data$ret),
      date = dplyr::if_else(!is.na(.data$dlstdt), .data$dlstdt, .data$date)
    ) |>
    dplyr::select(-c("dlret", "dlstdt")) |>
    dplyr::left_join(
      msedelist_sub |> dplyr::select("permno", "dlstdt"),
      by = "permno"
    ) |>
    dplyr::mutate(dlstdt = tidyr::replace_na(.data$dlstdt, lubridate::ymd(end_date))) |>
    dplyr::filter(.data$date <= .data$dlstdt) |>
    dplyr::select(-c("dlstdt")) |>
    dplyr::mutate(month = lubridate::floor_date(.data$date, "month"))

  # add risk free rate if provided, compute excess returns and log returns
  if (!is.null(rf)){
  crsp_daily_sub <- crsp_daily_sub |>
    dplyr::left_join(rf, by = "date") |>
    dplyr::mutate(
      ret_excess = .data$ret - .data$rf,
      ret_excess = pmax(.data$ret_excess, -1),
      log_ret_excess = log(1+.data$ret_excess)
    ) |>
    tidyr::drop_na("ret_excess","ret")
  }
  # create market cap and lagged market cap
  crsp_daily_sub <- crsp_daily_sub  |>
    dplyr::mutate(
      log_ret = log(1+.data$ret),
      shrout = .data$shrout * 1000,
      mktcap = abs(.data$shrout * .data$prc) / 10^6, # in millions of USD
      mktcap = dplyr::na_if(.data$mktcap, 0)
    )

  mktcap_lag <- crsp_daily_sub |>
    dplyr::mutate(date = .data$date + lubridate::days(1)) |>
    dplyr::select(.data$permno, .data$date, mktcap_lag = .data$mktcap)

  crsp_daily_sub <- crsp_daily_sub |>
    dplyr::left_join(mktcap_lag, by = c("permno", "date")) |>
    tidyr::drop_na("mktcap", "mktcap_lag")

  crsp_daily_sub |>
    dplyr::select(dplyr::any_of(
      c(
        "permno",
        "date",
        "month",
        "ret",
        "ret_excess",
        "rf",
        "log_ret",
        "log_ret_excess",
        "mktcap",
        "mktcap_lag"
        )
      )
    )
}

#' `get_crsp_indices_daily()` downloads daily frequency stock market indices from CRSP
#'
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @export get_crsp_indices_daily
#' @importFrom rlang .data
get_crsp_indices_daily <- function(
    start_date = "1940-01-01",
    end_date = "2023-12-31"
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
  on.exit(RPostgres::dbDisconnect(wrds))

  indx_db <- dplyr::tbl(wrds, dbplyr::in_schema("crsp", "dsi"))
  indx_db |>
    dplyr::filter(.data$date >= start_date & .data$date <= end_date) |>
    dplyr::collect()
}


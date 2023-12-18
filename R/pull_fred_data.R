#' Pull data from FRED
#'
#' @param fred_api_key API key from FRED
#' @param replicate Boolean, use original paper vintage
#' @param vintage_date String date ("yyyy-mm-dd"), use given date as vintage
#'
#' @return A tibble/data.frame
#'
pull_fred_data <- function(
    fred_api_key,
    replicate = FALSE,
    vintage_date = NULL) {
  qdate <- value <- NULL
  fredr::fredr_set_key(fred_api_key)

  ## Note that rgdp has a slightly later date, set in my_alfredr
  if (replicate == TRUE) vintage_date <- c("2018-07-26")

  ## Fred series codes that are pulled
  series <- c(
    gdpcap = "A939RX0Q048SBEA",
    gdp = "GDP",
    rgdp = "GDPC1",
    gdpdef = "GDPDEF",
    sh_nondur = "DNDGRE1Q156NBEA",
    sh_ser = "DSERRE1Q156NBEA",
    sh_dur = "DDURRE1Q156NBEA",
    sh_inv = "A006RE1Q156NBEA",
    oph = "OPHNFB",
    labor_sh = "PRS85006173",
    hw = "PRS85006023",
    pop = "CNP16OV",
    emp_pop = "CE16OV",
    ur = "UNRATE",
    ff = "FEDFUNDS"
  )

  my_fredr <- function(series, frequency) {
    agg <- ifelse(series == "CE16OV", "eop", "avg")
    fredr::fredr(series, frequency = frequency, aggregation_method = agg)
  }

  my_alfredr <- function(series, vintage_date) {
    if (replicate == TRUE && series == "GDPC1") vintage_date <- "2018-07-28"

    df <- alfred::get_alfred_series(series, "value",
      realtime_start = vintage_date, realtime_end = vintage_date,
      observation_start = "1947-01-01"
    ) |>
      dplyr::as_tibble() |>
      dplyr::select(date, value)

    df <- df |>
      dplyr::mutate(qdate = lubridate::date(paste0(
        lubridate::year(date), "-", lubridate::quarter(date) * 3 - 2, "-01"
      )))

    if (series == "CE16OV") {
      df <- df |>
        dplyr::group_by(qdate) |>
        dplyr::filter(date == max(date)) |>
        dplyr::mutate(date = qdate) |>
        dplyr::ungroup()
    }
    if (series == "UNRATE") {
      df <- df |>
        dplyr::group_by(qdate) |>
        dplyr::summarize(value = mean(value)) |>
        dplyr::mutate(date = qdate) |>
        dplyr::ungroup()
    }
    if (series == "FEDFUNDS") {
      df <- df |>
        dplyr::group_by(qdate) |>
        dplyr::summarize(value = mean(value)) |>
        dplyr::mutate(date = qdate) |>
        dplyr::ungroup()
    }
    if (series == "CNP16OV") {
      df <- df |>
        dplyr::group_by(qdate) |>
        dplyr::summarize(value = mean(value)) |>
        dplyr::mutate(date = qdate) |>
        dplyr::ungroup()
    }

    df <- df |>
      dplyr::select(date, value) |>
      tidyr::pivot_longer(cols = value, names_to = "series_id") |>
      dplyr::mutate(series_id = series)

    return(df)
  }

  if (!is.null(vintage_date)) {
    data <- Reduce(rbind, lapply(
      series, my_alfredr,
      vintage_date = vintage_date
    ))
  } else {
    data <- Reduce(rbind, lapply(
      series, my_fredr,
      frequency = "q"
    ))
  }

  data <- data[, c("date", "series_id", "value")]

  data <- data[!is.na(data$value), ]

  data <- data |>
    tidyr::pivot_wider(names_from = "series_id", values_from = "value")

  names(data) <- c("date", names(series))

  data <- dplyr::as_tibble(data) |>
    dplyr::arrange(date)

  return(data)
}

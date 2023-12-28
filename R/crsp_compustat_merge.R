#' Download linking table from CRSP-Compustat
#'  Merged table and merge CRSP and Compustat
#'
#' `get_crsp_compustat()` dowloads CRSP-Compustat linking table
#'
#' @param crsp_monthly A tibble with CRSP monthly data
#' @param start_date Starting date in any format recognized by [lubridate::ymd]
#' @param end_date Ending date in any format recognized by [lubridate::ymd]
#' @export get_crsp_compustat
#' @importFrom rlang .data
get_crsp_compustat <- function(
    crsp_monthly,
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

  ccmxpf_linktable_db <- dplyr::tbl(
    wrds,
    dbplyr::in_schema("crsp", "ccmxpf_linktable")
  )

  ccmxpf_linktable <- ccmxpf_linktable_db |>
    dplyr::filter(.data$linktype %in% c("LU", "LC") &
             .data$linkprim %in% c("P", "C") &
             .data$usedflag == 1) |>
    dplyr::select(permno = .data$lpermno, .data$gvkey, .data$linkdt, .data$linkenddt) |>
    dplyr::collect() |>
    dplyr::mutate(linkenddt = tidyr::replace_na(.data$linkenddt, lubridate::today()))

  ccm_links <- crsp_monthly |>
    dplyr::inner_join(ccmxpf_linktable,
               by = "permno", relationship = "many-to-many") |>
    dplyr::filter(!is.na(.data$gvkey) &
             (.data$date >= .data$linkdt & .data$date <= .data$linkenddt)) |>
    dplyr::select(.data$permno, .data$gvkey, .data$date)
}



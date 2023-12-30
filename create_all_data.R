library(devtools)
load_all()

library(tidyverse)

fred <- get_fred()
bea_raw <- get_bea()
bea_clean <- clean_bea(bea_raw)
bea <- make_vars_bea(bea_clean)

metadata <- fred$metadata %>%
  rows_append(bea$metadata)

famafrench_raw <- get_famafrench()
famafrench <- clean_famafrench(famafrench_raw)

rf <- famafrench$monthly |>
  dplyr::select(date, rf) |>
  dplyr::collect()
rf_daily <- famafrench$daily |>
  dplyr::select(date, rf) |>
  dplyr::collect()

crsp_raw <- get_crsp()
crsp <- clean_crsp(crsp_raw,rf)
# permnos <- crsp |>
#   distinct(permno) |>
#   pull()

compustat_raw <- get_compustat(compustat_freq = "annual")
compustat <- clean_compustat(compustat_raw,compustat_freq = "annual")
# floor date compustat
compustat_raw_q <- get_compustat(compustat_freq = "quarterly")
compustat_q <- clean_compustat(compustat_raw_q, compustat_freq = "quarterly")
# floor date compustat_q
sic <- get_sic()

# Merge CRSP and Compustat ------------------------------------------------
crsp_compustat_link <- get_crsp_compustat(crsp)

crsp_monthly <- crsp |>
  left_join(crsp_compustat_link, by = c("permno", "date"))

crsp_compustat <- crsp_monthly |>
  right_join(compustat, by = c("gvkey", "date")) |>
  left_join(sic, by = c("gvkey")) |>
  dplyr::mutate(
    sic = as.numeric(stringr::str_sub(.data$sic, start = 1, end=2) )
  ) |>
  dplyr::filter(
    .data$sic != 49 & # utilities
      ! dplyr::between(.data$sic, 60, 69) & # finance and real estate
      ! dplyr::between(.data$sic, 90, 99) # public administration
      #drop oil companies too?
  )
#  drop_na("sic") to drop if 2-digit SIC code is missing

# Select Compustat's SICH as primary SIC code,
# If not available then use CRSP's historical SICCD
# coalesce(a.sich,d.siccd) as sic


# Daily indices -----------------------------------------------------------
crsp_indices_daily <- get_crsp_indices_daily() |>
  dplyr::select(
    date,
    dplyr::all_of(c("vwretd", "vwretx", "ewretd", "ewretx", "sprtrn"))  # columns that have returns
  )

aggregation_funs <- function(x,  dt = 1) {
  dplyr::tibble(
    avg = na_if(mean((1/{{dt}})*x, na.rm = TRUE), NaN),
    cumret = prod(1+x)-1,
    sd = sqrt(1/{{dt}})*sd(x, na.rm = TRUE)
  )
}

crsp_indices <- crsp_indices_daily %>%  timetk::summarize_by_time(
    .date_var = date,
    .by = "quarter",
    dplyr::across(dplyr::where(is.numeric),
                  \(x) aggregation_funs(x, 4/252),
                  .unpack = TRUE
    )
    )


# Daily individual stock returns ------------------------------------------
crsp_daily <- get_crsp_daily(
    rf = rf_daily,
    permnos = unique(crsp_compustat$permno)
  )

crsp_daily_portfolios <- crsp_daily %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    weights = .data$mktcap_lag / sum(.data$mktcap_lag),
    dplyr::across(
      .cols = dplyr::contains("ret"),
      ~ sum(.x * .data$weights),
      .names = "{.col}_vw"
    ),
    dplyr::across(
      .cols = dplyr::contains("ret") & !dplyr::ends_with("_vw"),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_ew"
    )
  ) %>%
  dplyr::summarize(
    dplyr::across(dplyr::ends_with("_ew") | dplyr::ends_with("_vw"), last)
  )


crsp_quarterly_portfolios <- crsp_daily_portfolios %>%
  mutate(quarter = lubridate::quarter(date)) %>%
  group_by(quarter) %>%
  timetk::summarize_by_time(
    .date_var = date,
    .by = "quarter",
    dplyr::across(dplyr::where(is.numeric),
                  \(x) aggregation_funs(x, 4/252),
                  .unpack = TRUE
    )
  )




# Merge everything --------------------------------------------------------

ts <- variables <- purrr::reduce(
  list(
    pivot_wider(fred$data,names_from = label,values_from=value,id_cols=date) ,
    bea$data %>% select(!starts_with("UNIT_MULT")),
    crsp_indices %>% select(!ends_with("end_of_period"))
    ),
  dplyr::full_join,
  by = "date"
  ) %>%
  arrange(date)


# Save to database --------------------------------------------------------
library(RSQLite)
library(dbplyr)

# create database
database <- RSQLite::dbConnect(
  RSQLite::SQLite(),
  "inst/extdata/all_data.sqlite",
  extended_types = TRUE
)
# save
RSQLite::dbWriteTable(database,
                      "ts",
                      value = ts,
                      overwrite = TRUE
)
RSQLite::dbWriteTable(database,
             "fred",
             value = fred$data,
             overwrite = TRUE
)
RSQLite::dbWriteTable(database,
             "crsp",
             value = crsp,
             overwrite = TRUE
)
RSQLite::dbWriteTable(database,
             "famafrench",
             value = famafrench$monthly,
             overwrite = TRUE
)
RSQLite::dbWriteTable(database,
             "bea",
             value = bea$data,
             overwrite = TRUE
)
RSQLite::dbWriteTable(database,
             "compustat",
             value = compustat,
             overwrite = TRUE
)
RSQLite::dbWriteTable(database,
                      "compustat_q",
                      value = compustat_q,
                      overwrite = TRUE
)
RSQLite::dbWriteTable(database,
             "crsp_compustat",
             value = crsp_compustat,
             overwrite = TRUE
)
dbWriteTable(database,
             "crsp_indices",
             value = crsp_indices,
             overwrite = TRUE
)
dbDisconnect(database)



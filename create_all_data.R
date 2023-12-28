library(investmentVolData)
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
  dplyr::select(month, rf) |>
  dplyr::collect()
rf_daily <- famafrench$daily |>
  dplyr::select(date, rf) |>
  dplyr::collect()

crsp_raw <- get_crsp()
crsp <- clean_crsp(crsp_raw,rf)
permnos <- crsp |>
  distinct(permno) |>
  pull()
# crsp_daily <- get_crsp_daily(
#     rf = rf_daily,
#     permnos = permnos
#   )

compustat_raw <- get_compustat()
compustat <- clean_compustat(compustat_raw)
sic <- get_sic()

# Merge CRSP and Compustat ------------------------------------------------
crsp_compustat_link <- get_crsp_compustat(crsp)

crsp_monthly <- crsp |>
  left_join(crsp_compustat_link, by = c("permno", "date")) |>
  select(-month)

crsp_compustat <- crsp_monthly |>
  left_join(compustat, by = c("gvkey", "date")) |>
  left_join(sic, by = c("gvkey")) |>
  dplyr::mutate(
    sic = as.numeric(stringr::str_sub(.data$sic, start = 1, end=2) )
  ) |>
  dplyr::filter(
    .data$sic != 49 & # utilities
      ! dplyr::between(.data$sic, 60, 69) & # finance and real estate
      ! dplyr::between(.data$sic, 90, 99) # public administration
  )
#  drop_na("sic") to drop if 2-digit SIC code is missing

# Select Compustat's SICH as primary SIC code,
# If not available then use CRSP's historical SICCD
# coalesce(a.sich,d.siccd) as sic

crsp_compustat_agregate <- crsp_compustat %>% dplyr::mutate(
  firmI = capx - sppe, #Gross investment in physical capital is computed as capital expenditures minus sales of property, plant and equipment
  firmK_gross = na_if(ppegt,0), # Property, Plant and Equipment - Total (Gross)
  firmK_net = na_if(ppent,0), # Property, Plant and Equipment - Total (Net)
  firmIK_gross = firmI/firmK_gross,
  firmIK_net = firmI/firmK_net
) %>%
  drop_na(firmIK_gross,firmIK_net) %>%
  dplyr::group_by(year,quarter) %>%
  dplyr::summarize(
    firmI = sum(firmI, na.rm = TRUE),
    firmK_gross = sum(firmK_gross, na.rm = TRUE),
    firmK_net = sum(firmK_net, na.rm = TRUE),
    firmIK_gross = mean(firmIK_gross, na.rm = TRUE),
    firmIK_net = mean(firmIK_net, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    date = lubridate::ymd(paste0(.data$year,"-",.data$quarter * 3 - 2 ,"-","01"))
  )

# Stock market indices return and vol --------------------------------

crsp_indices_daily <- get_crsp_indices_daily()

#functions to collapse from daily to quarterly
aggregation_funs <- function(x) {
  dplyr::tibble(
    avg = na_if(mean(x, na.rm = TRUE), NaN),
    end_of_period = last(x, na_rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
}

crsp_indices <- timetk::summarize_by_time(
  crsp_indices_daily |> dplyr::select("date","vwretd", "vwretx", "ewretd", "ewretx", "sprtrn"),
  .date_var = date,
  .by = "quarter",
  dplyr::across(dplyr::where(is.numeric),
                \(x) aggregation_funs(x),
                .unpack = TRUE
  ),
  .type = "ceiling"
) %>%
  #remove column if all NA
  dplyr::select_if(~ !all(is.na(.)))

# Merge everything --------------------------------------------------------

ts <- variables <- purrr::reduce(
  list(
    pivot_wider(fred$data,names_from = label,values_from=value,id_cols=date) ,
    bea$data %>% select(!starts_with("UNIT_MULT")),
    crsp_compustat_agregate%>% select(-c("year","quarter")),
    crsp_indices %>% select(!ends_with("end_of_period"))
    ),
  dplyr::full_join,
  by = "date"
  )


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
             "fred",
             value = fred,
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
             value = bea,
             overwrite = TRUE
)
RSQLite::dbWriteTable(database,
             "compustat",
             value = compustat,
             overwrite = TRUE
)
RSQLite::dbWriteTable(database,
             "crsp_compustat",
             value = crsp_compustat,
             overwrite = TRUE
)
dbWriteTable(database,
             "crsp_compustat_agregate",
             value = crsp_compustat_agregate,
             overwrite = TRUE
)
dbWriteTable(database,
             "crsp_indices",
             value = crsp_indices,
             overwrite = TRUE
)
dbDisconnect(database)




purrr::map(c(
  "dplyr","stringr","purrr","lubridate","frenchdata","rlang","RPostgres","dbplyr","bea.R","magrittr","tidyr","scales","fredr"
),
\(x) utils::install.packages(x) )

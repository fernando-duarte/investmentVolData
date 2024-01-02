library(devtools)
load_all()
library(tidyverse)


# Download data -----------------------------------------------------------

## FRED --------------------------------------------------------------------
fred <- get_fred()

## BEA ---------------------------------------------------------------------
bea_raw <- get_bea()
bea_clean <- clean_bea(bea_raw)
bea <- make_vars_bea(bea_clean)

## Fama-French -------------------------------------------------------------
famafrench_raw <- get_famafrench()
famafrench <- clean_famafrench(famafrench_raw)

rf_monthly <- famafrench$monthly |>
  dplyr::select(date, rf) |>
  dplyr::collect()
rf_daily <- famafrench$daily |>
  dplyr::select(date, rf) |>
  dplyr::collect()

## CRSP --------------------------------------------------------------------
### Individual stocks monthly -----------------------------------------------------------------
crsp_raw <- get_crsp()
crsp <- clean_crsp(crsp_raw,rf_monthly)
permnos <- crsp |>
  distinct(permno) |>
  pull()
### Individual stocks daily -------------------------------------------------------------------
crsp_daily <- get_crsp_daily(
  rf = rf_daily,
  permnos = permnos #unique(crsp_compustat$permno)
)
### Indices daily -----------------------------------------------------------
crsp_indices_daily <- get_crsp_indices_daily(
  rf = rf_daily
  ) |>
  dplyr::select(
    date,
    dplyr::all_of(c("vwretd", "vwretx", "ewretd", "ewretx", "sprtrn")) # columns that have returns
    |
    dplyr::ends_with("_excess") # columns that excess returns
  )

## Compustat ---------------------------------------------------------------
### Annual ------------------------------------------------------------------
compustat_raw <- get_compustat(compustat_freq = "annual")
compustat <- clean_compustat(compustat_raw,compustat_freq = "annual")
### Quarterly ---------------------------------------------------------------
compustat_raw_q <- get_compustat(compustat_freq = "quarterly")
compustat_q <- clean_compustat(compustat_raw_q, compustat_freq = "quarterly")

compustat_q %>% summarize(n=n(),.by="year") %>% arrange(year) %>% print(n="all")
compustat_raw_q %>% filter(fic=="USA") %>% summarize(n=n(),.by="datadate") %>% arrange(datadate) %>% print(n="all")

compustat_q %>% summarize(n=n(),.by="date") %>% arrange(date) %>% print(n="all")%>%
  mutate(year = year(date)) %>% summarize(nn=sum(n),.by="year")%>% print(n="all")
compustat_q %>% filter(fic=="USA") %>% summarize(n=n(),.by="year") %>% arrange(year) %>% print(n="all")

### Industry codes ----------------------------------------------------------
sic <- get_sic()

## CRSP-Compustat Link -----------------------------------------------------
crsp_compustat_link <- get_crsp_compustat(crsp)

# Time aggregate ----------------------------------------------------------
aggregation_funs <- function(x,  dt = 1) {
  dplyr::tibble(
    avg = na_if(mean((1/{{dt}})*x, na.rm = TRUE), NaN),
    cumret = prod(1+x)-1,
    sd = sqrt(1/{{dt}})*sd(x, na.rm = TRUE)
  )
}

## CRSP --------------------------------------------------------------------
### Individual stocks daily to quarterly-------------------------------------------------------------------
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

crsp_q <- crsp_daily_portfolios %>%
  timetk::summarize_by_time(
    .date_var = date,
    .by = "quarter",
    dplyr::across(dplyr::where(is.numeric),
                  \(x) aggregation_funs(x, 4/252),
                  .unpack = TRUE
    )
  )


### Collect permnos of each portfolio ---------------------------------------
crsp_daily_portfolios_permnos <- crsp_daily %>%
  reframe(permno = list(permno), .by = date)

crsp_q_permnos <- crsp_daily_portfolios_permnos %>%
  dplyr::mutate(
    date = lubridate::yq(
      paste0(lubridate::year(date),".",lubridate::quarter(date))
    )
  ) %>%
  reframe(
    permno = list(permno),
    .by = date
  ) %>%
  mutate(permno = purrr::map(permno,\(x) unique(unlist(x))))

### Indices daily to quarterly-----------------------------------------------------------
crsp_indices_q <- crsp_indices_daily %>%
  timetk::summarize_by_time(
      .date_var = date,
      .by = "quarter",
      dplyr::across(dplyr::where(is.numeric),
                    \(x) aggregation_funs(x, 4/252),
                    .unpack = TRUE
  )
)
## CRSP-Compustat Link -----------------------------------------------------
### Monthly to quarterly-----------------------------------------------------------
crsp_compustat_link_q <- crsp_compustat_link %>%
  dplyr::mutate(
    yq = lubridate::yq(
      paste0(lubridate::year(date),".",lubridate::quarter(date))
    )
  ) %>%
  group_by(permno, yq) %>%
  dplyr::arrange(date) %>%
  dplyr::summarize(
    gvkey = last(gvkey),
    .groups = "drop"
  ) %>%
  rename(date = yq)

# Merge -------------------------------------------------------------------
## CRSP and Compustat ------------------------------------------------
compustat_permnos_q <- compustat_q |>
  left_join(crsp_compustat_link_q, by = c("gvkey", "date")) %>%
  arrange(permno, date) %>%
  relocate(gvkey,permno,date,capxy,capx,sppey,sppe,contains("firm"))

compustat_permnos_q %>%
  filter(!is.na(firmI)) %>%
  summarize(n=n())

compustat_ports_q <- compustat_permnos_q %>%
  left_join(
    crsp_q_permnos %>% unnest(cols=permno),
    by = c("permno", "date")
  )
# %>%  drop_na(permno)

compustat_ports_q %>%
  filter(!is.na(permno) & !is.na(firmI)) %>%
  summarize(n=n())

compustat_ports_q %>%
  filter(!is.na(capx)) %>%
  summarize(n=n())

crsp_compustat <- compustat_ports_q |>
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

crsp_compustat_agregate <- compustat %>% #crsp_compustat %>%
  dplyr::mutate(
  firmCapex = sum(capex, na.rm = TRUE),
  firmIK_gross = firmI/firmK_gross_lag,
  firmIK_net = firmI/firmK_net_lag,
  firmCapexK_gross = firmCapex/firmK_gross_lag,
  firmCapexK_net = firmCapex/firmK_net_lag
) %>%
  # drop_na(firmIK_gross,firmIK_net) %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(
    firmI = sum(firmI, na.rm = TRUE),
    firmK_gross = sum(firmK_gross, na.rm = TRUE),
    firmK_net = sum(firmK_net, na.rm = TRUE),
    firmI_lag = sum(firmI_lag, na.rm = TRUE),
    firmK_gross_lag = sum(firmK_gross_lag, na.rm = TRUE),
    firmK_net_lag = sum(firmK_net_lag, na.rm = TRUE),
    xs_avg_firmCapexK_gross = mean(firmCapexK_gross, na.rm = TRUE),
    xs_avg_firmCapexK_net = mean(firmCapexK_net, na.rm = TRUE),
    xs_avg_firmIK_gross = mean(firmIK_gross, na.rm = TRUE),
    xs_avg_firmIK_net = mean(firmIK_net, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    firmIK_gross = firmI/firmK_gross_lag,
    firmIK_net = firmI/firmK_net_lag,
    firmCapexK_gross = firmCapex/firmK_gross_lag,
    firmCapexK_net = firmCapex/firmK_net_lag,
  ) %>%
  arrange(date) %>%
  relocate(date,n,firmIK_gross,xs_avg_firmIK_gross,firmIK_net,xs_avg_firmIK_net)

print(crsp_compustat_agregate,n="all")


# Merge everything --------------------------------------------------------

ts <- variables <- purrr::reduce(
  list(
    pivot_wider(fred$data,names_from = label,values_from=value,id_cols=date) ,
    bea$data %>% select(!starts_with("UNIT_MULT")),
    crsp_indices_q,
    crsp_q,
    crsp_compustat_agregate
    ),
  dplyr::full_join,
  by = "date"
  ) %>%
  arrange(date)

# Make all units billions of dollars and annualized rates (for flows)
ts <- ts %>%
  mutate(
    NIPA.Structures.I.NSA = NIPA.Structures.I.NSA*4/1000,
    NIPA.Equipment.I.NSA = NIPA.Equipment.I.NSA*4/1000,
    FA.Structures.K = FA.Structures.K/1000,
    FA.Equipment.K = FA.Equipment.K/1000,
    FA.Equipment.D = FA.Equipment.D/1000,
    FA.Structures.D = FA.Structures.D/1000,

    `Private nonresidential fixed assets.K` = `Private nonresidential fixed assets.K`/1000,
    Equipment.K = Equipment.K/1000,
    Structures.K = Structures.K/1000,

    `Private nonresidential fixed assets.D` = `Private nonresidential fixed assets.D`/1000,
    Equipment.D = Equipment.D/1000,
    Structures.D = Structures.D/1000,

    `Private nonresidential fixed assets.I` = `Private nonresidential fixed assets.I`/1000,
    Equipment.I = Equipment.I/1000,
    Structures.I = Structures.I/1000,

    firmI = firmI/1000,
    firmK_gross = firmK_gross/1000,
    firmK_net = firmK_net/1000,
    firmI_lag = firmI_lag/1000,
    firmK_gross_lag = firmK_gross_lag/1000,
    firmK_net_lag = firmK_net_lag/1000
  )






# Construct aggregate K, I, D ---------------------------------------------

# parameter between 0 and 1 that determines assumption of when investment is placed in service
#nu  = 0 is end of year, nu  = 1 is beginning of year; BEA uses nu  = 1/2
nu <- 1/2

ts <- ts %>%
  #remove column if all NA
  dplyr::select_if(~ !all(is.na(.))) %>%
  # pick what series to use
  dplyr::mutate(
    tsEquipment.K = Equipment.K,
    tsStructures.K = Structures.K,
    tsEquipment.I = Equipment.I, # "NIPA.Equipment.I.SA" "NIPA.Equipment.I" "NIPA.Equipment.I.NSA"; from FA: "Equipment.I"
    tsStructures.I = Structures.I, # "NIPA.Structures.I.SA" "NIPA.Structures.I" "NIPA.Structures.I.NSA"; from FA: "Structures.I"
    tsEquipment.D = Equipment.D,
    tsStructures.D = Structures.D,
    tsY = gdp
  ) %>%
  # construct variables
  dplyr::mutate(
    K = .data$tsEquipment.K + .data$tsStructures.K,
    lag.K = dplyr::lag(.data$K, order_by = .data$date, n = 4L),
    lag.Equipment.K = dplyr::lag(.data$tsEquipment.K, order_by = .data$date, n = 4L),
    lag.Structures.K = dplyr::lag(.data$tsStructures.K, order_by = .data$date, n = 4L),
    I = .data$tsEquipment.I+.data$tsStructures.I,
    IK = .data$I/.data$lag.K,
    IK.Equipment = .data$tsEquipment.I/ .data$lag.Equipment.K,
    IK.Structures = .data$tsStructures.I/ .data$lag.Structures.K,
    IK = .data$I/.data$lag.K,
    depreciation.Equipment = .data$tsEquipment.D/ (.data$lag.Equipment.K + nu * .data$tsEquipment.I),
    depreciation.Structures = .data$tsStructures.D/ (.data$lag.Structures.K + nu * .data$tsStructures.I),
    depreciation.K = (.data$lag.Equipment.K/.data$lag.K) * .data$depreciation.Equipment + (.data$lag.Structures.K/.data$lag.K) * .data$depreciation.Structures,
    YK = .data$tsY/.data$lag.K,
    IY = .data$I/.data$tsY
  )
# Save to csv -------------------------------------------------------------
csv_path = "inst/extdata/csv/"
write.csv(ts, paste0(csv_path,"quarterly_time_series",".csv"), row.names = FALSE)



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



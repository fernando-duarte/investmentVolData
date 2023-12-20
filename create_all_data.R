library(investmentVolData)

fred_raw <- get_fred()
fred <- clean_fred(fred_raw)
wrds_raw <- get_wrds()
wrds <- clean_wrds(wrds_raw)
famafrench_raw <- get_famafrench()
famafrench <- clean_famafrench(famafrench_raw)
bea_raw <- get_bea()
bea <- clean_bea(bea_raw)

library(RSQLite)
library(dbplyr)
# create database
database <- dbConnect(
  SQLite(),
  "inst/extdata/all_data.sqlite",
  extended_types = TRUE
)
# save
dbWriteTable(database,
             "fred",
             value = fred,
             overwrite = TRUE
)
dbWriteTable(database,
             "wrds",
             value = wrds,
             overwrite = TRUE
)
dbWriteTable(database,
             "famafrench",
             value = famafrench,
             overwrite = TRUE
)
dbWriteTable(database,
             "bea",
             value = bea,
             overwrite = TRUE
)

# library(RSQLite)
#
# ff <- get_famafrench()
# fred <- get_fred()
# wrds <- get_wrds()
#
# tidy_finance <- dbConnect(
#   SQLite(),
#   "data/tidy_finance_r.sqlite",
#   extended_types = TRUE
# )
#
# factors_ff3_monthly <- tbl(tidy_finance, "factors_ff3_monthly") |>
#   select(month, rf) |>
#   collect()
#
# crsp_monthly <- crsp_monthly |>
#   left_join(factors_ff3_monthly,
#             by = "month"
#   ) |>
#   mutate(
#     ret_excess = ret_adj - rf,
#     ret_excess = pmax(ret_excess, -1)
#   ) |>
#   select(-ret_adj, -rf)
#
# crsp_monthly <- crsp_monthly |>
#   drop_na(ret_excess, mktcap, mktcap_lag)
#
# dbWriteTable(tidy_finance,
#              "crsp_monthly",
#              value = crsp_monthly,
#              overwrite = TRUE
# )
#
# crsp_monthly |>
#   count(exchange, date) |>
#   ggplot(aes(x = date, y = n, color = exchange, linetype = exchange)) +
#   geom_line() +
#   labs(
#     x = NULL, y = NULL, color = NULL, linetype = NULL,
#     title = "Monthly number of securities by listing exchange"
#   ) +
#   scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
#   scale_y_continuous(labels = comma)
#
# tbl(tidy_finance, "crsp_monthly") |>
#   left_join(tbl(tidy_finance, "cpi_monthly"), by = "month") |>
#   group_by(month, exchange) |>
#   summarize(
#     mktcap = sum(mktcap, na.rm = TRUE) / cpi,
#     .groups = "drop"
#   ) |>
#   collect() |>
#   mutate(month = ymd(month)) |>
#   ggplot(aes(
#     x = month, y = mktcap / 1000,
#     color = exchange, linetype = exchange
#   )) +
#   geom_line() +
#   labs(
#     x = NULL, y = NULL, color = NULL, linetype = NULL,
#     title = "Monthly market cap by listing exchange in billions of Dec 2022 USD"
#   ) +
#   scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
#   scale_y_continuous(labels = comma)
#
# cpi_monthly <- tbl(tidy_finance, "cpi_monthly") |>
#   collect()
#
# crsp_monthly_industry <- crsp_monthly |>
#   left_join(cpi_monthly, by = "month") |>
#   group_by(month, industry) |>
#   summarize(
#     securities = n_distinct(permno),
#     mktcap = sum(mktcap) / mean(cpi),
#     .groups = "drop"
#   )
#
# crsp_monthly_industry |>
#   ggplot(aes(
#     x = month,
#     y = securities,
#     color = industry,
#     linetype = industry
#   )) +
#   geom_line() +
#   labs(
#     x = NULL, y = NULL, color = NULL, linetype = NULL,
#     title = "Monthly number of securities by industry"
#   ) +
#   scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
#   scale_y_continuous(labels = comma)
#
# ## daily
# dsf_db <- tbl(wrds, in_schema("crsp", "dsf"))
#
# factors_ff3_daily <- tbl(tidy_finance, "factors_ff3_daily") |>
#   collect()
#
# permnos <- tbl(tidy_finance, "crsp_monthly") |>
#   distinct(permno) |>
#   pull()
#
# batch_size <- 500
# batches <- ceiling(length(permnos) / batch_size)
#
# for (j in 1:batches) {
#
#   permno_batch <- permnos[
#     ((j - 1) * batch_size + 1):min(j * batch_size, length(permnos))
#   ]
#
#   crsp_daily_sub <- dsf_db |>
#     filter(permno %in% permno_batch &
#              date >= start_date & date <= end_date) |>
#     select(permno, date, ret) |>
#     collect() |>
#     drop_na()
#
#   if (nrow(crsp_daily_sub) > 0) {
#
#     msedelist_sub <- msedelist_db |>
#       filter(permno %in% permno_batch) |>
#       select(permno, dlstdt, dlret) |>
#       collect() |>
#       drop_na()
#
#     crsp_daily_sub <- crsp_daily_sub |>
#       left_join(msedelist_sub, by = c("permno", "date"="dlstdt")) |>
#       bind_rows(msedelist_sub |>
#                   anti_join(crsp_daily_sub,
#                             by = c("permno", "dlstdt" = "date"))) |>
#       mutate(ret = if_else(!is.na(dlret), dlret, ret),
#              date = if_else(!is.na(dlstdt), dlstdt, date)) |>
#       select(-c(dlret, dlstdt)) |>
#       left_join(msedelist_sub |>
#                   select(permno, dlstdt), by = "permno") |>
#       mutate(dlstdt = replace_na(dlstdt, end_date)) |>
#       filter(date <= dlstdt) |>
#       select(-dlstdt)
#
#     crsp_daily_sub <- crsp_daily_sub |>
#       mutate(month = floor_date(date, "month")) |>
#       left_join(factors_ff3_daily |>
#                   select(date, rf), by = "date") |>
#       mutate(
#         ret_excess = ret - rf,
#         ret_excess = pmax(ret_excess, -1)
#       ) |>
#       select(permno, date, month, ret, ret_excess)
#
#     dbWriteTable(tidy_finance,
#                  "crsp_daily",
#                  value = crsp_daily_sub,
#                  overwrite = ifelse(j == 1, TRUE, FALSE),
#                  append = ifelse(j != 1, TRUE, FALSE)
#     )
#   }
#
#   cat("Batch", j, "out of", batches, "done (", percent(j / batches), ")\n")
# }
#
# # compustat
# funda_db <- tbl(wrds, in_schema("comp", "funda"))
# compustat <- funda_db |>
#   filter(
#     indfmt == "INDL" &
#       datafmt == "STD" &
#       consol == "C" &
#       datadate >= start_date & datadate <= end_date
#   ) |>
#   select(
#     gvkey, # Firm identifier
#     datadate, # Date of the accounting data
#     seq, # Stockholders' equity
#     ceq, # Total common/ordinary equity
#     at, # Total assets
#     lt, # Total liabilities
#     txditc, # Deferred taxes and investment tax credit
#     txdb, # Deferred taxes
#     itcb, # Investment tax credit
#     pstkrv, # Preferred stock redemption value
#     pstkl, # Preferred stock liquidating value
#     pstk, # Preferred stock par value
#     capx, # Capital investment
#     oancf, # Operating cash flow
#     sale,  # Revenue
#     cogs, # Costs of goods sold
#     xint, # Interest expense
#     xsga # Selling, general, and administrative expenses
#   ) |>
#   collect()
#
# compustat <- compustat |>
#   mutate(
#     be = coalesce(seq, ceq + pstk, at - lt) +
#       coalesce(txditc, txdb + itcb, 0) -
#       coalesce(pstkrv, pstkl, pstk, 0),
#     be = if_else(be <= 0, as.numeric(NA), be),
#     op = (sale - coalesce(cogs, 0) -
#             coalesce(xsga, 0) - coalesce(xint, 0)) / be,
#   )
#
# compustat <- compustat |>
#   mutate(year = year(datadate)) |>
#   group_by(gvkey, year) |>
#   filter(datadate == max(datadate)) |>
#   ungroup()
#
# compustat <- compustat |>
#   left_join(
#     compustat |>
#       select(gvkey, year, at_lag = at) |>
#       mutate(year = year + 1), by = c("gvkey", "year")
#   ) |>
#   mutate(
#     inv = at / at_lag - 1,
#     inv = if_else(at_lag <= 0, as.numeric(NA), inv)
#   )
#
# dbWriteTable(tidy_finance,
#              "compustat",
#              value = compustat,
#              overwrite = TRUE
# )
#
# #Merging CRSP with Compustat
# ccmxpf_linktable_db <- tbl(
#   wrds,
#   in_schema("crsp", "ccmxpf_linktable")
# )
# ccmxpf_linktable <- ccmxpf_linktable_db |>
#   filter(linktype %in% c("LU", "LC") &
#            linkprim %in% c("P", "C") &
#            usedflag == 1) |>
#   select(permno = lpermno, gvkey, linkdt, linkenddt) |>
#   collect() |>
#   mutate(linkenddt = replace_na(linkenddt, today()))
#
# ccm_links <- crsp_monthly |>
#   inner_join(ccmxpf_linktable,
#              by = "permno", relationship = "many-to-many") |>
#   filter(!is.na(gvkey) &
#            (date >= linkdt & date <= linkenddt)) |>
#   select(permno, gvkey, date)
#
# crsp_monthly <- crsp_monthly |>
#   left_join(ccm_links, by = c("permno", "date"))
# dbWriteTable(tidy_finance,
#              "crsp_monthly",
#              value = crsp_monthly,
#              overwrite = TRUE
# )
# crsp_monthly |>
#   group_by(permno, year = year(month)) |>
#   filter(date == max(date)) |>
#   ungroup() |>
#   left_join(compustat, by = c("gvkey", "year")) |>
#   group_by(exchange, year) |>
#   summarize(
#     share = n_distinct(permno[!is.na(be)]) / n_distinct(permno),
#     .groups = "drop"
#   ) |>
#   ggplot(aes(
#     x = year,
#     y = share,
#     color = exchange,
#     linetype = exchange
#   )) +
#   geom_line() +
#   labs(
#     x = NULL, y = NULL, color = NULL, linetype = NULL,
#     title = "Share of securities with book equity values by exchange"
#   ) +
#   scale_y_continuous(labels = percent) +
#   coord_cartesian(ylim = c(0, 1))
#

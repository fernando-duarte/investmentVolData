library(tidyverse)
library(RSQLite)
library(scales)
library(ggplot2)
library(ggeasy)
library(latex2exp)

# load
all_data <- dbConnect(
  SQLite(),
  "inst/extdata/all_data.sqlite",
  extended_types = TRUE
)
dbListTables(all_data)
ts <- tbl(all_data, "ts") %>%
  collect()





crsp_compustat_agregate <- crsp_compustat %>% dplyr::mutate(
  firmIK_gross = firmI/firmK_gross_lag,
  firmIK_net = firmI/firmK_net_lag
) %>%
  drop_na(firmIK_gross,firmIK_net) %>%
  dplyr::group_by(year,quarter) %>%
  dplyr::summarize(
    firmI = sum(firmI, na.rm = TRUE),
    firmK_gross = sum(firmK_gross, na.rm = TRUE),
    firmK_net = sum(firmK_net, na.rm = TRUE),
    firmI_lag = sum(firmI_lag, na.rm = TRUE),
    firmK_gross_lag = sum(firmK_gross_lag, na.rm = TRUE),
    firmK_net_lag = sum(firmK_net_lag, na.rm = TRUE),
    xs_avg_firmIK_gross = mean(firmIK_gross, na.rm = TRUE),
    xs_avg_firmIK_net = mean(firmIK_net, na.rm = TRUE),
    n = n(),
    vw_return_excess = sum(mktcap_lag / sum(mktcap_lag) * ret_excess)
  ) %>%
  ungroup() %>%
  mutate(
    date = lubridate::ymd(paste0(.data$year,"-",.data$quarter * 3 - 2 ,"-","01")),
    firmIK_gross = firmI/firmK_gross_lag,
    firmIK_net = firmI/firmK_net_lag,
  )

crsp_compustat_agregate%>% select(-c("year","quarter")),

# Stock market indices return and vol --------------------------------

%>%
  #remove column if all NA
  dplyr::select_if(~ !all(is.na(.)))
%>%
  dplyr::mutate(
    K = .data$Equipment.K + .data$Structures.K,
    lag.K = dplyr::lag(.data$K, order_by = .data$date),
    lag.Equipment.K = dplyr::lag(.data$Equipment.K, order_by = .data$date),
    lag.Structures.K = dplyr::lag(.data$Structures.K, order_by = .data$date),
    I = .data$Equipment.I+.data$Structures.I,
    IK = .data$I/.data$lag.K,
    IK.Equipment = .data$Equipment.I/ .data$lag.Equipment.K,
    IK.Structures = .data$Structures.I/ .data$lag.Structures.K,
    IK = .data$I/.data$lag.K,
    depreciation.Equipment = .data$Equipment.D/ (.data$lag.Equipment.K + nu * .data$Equipment.I),
    depreciation.Structures = .data$Structures.D/ (.data$lag.Structures.K + nu * .data$Structures.I),
    depreciation.K = (.data$lag.Equipment.K/.data$lag.K) * .data$depreciation.Equipment + (.data$lag.Structures.K/.data$lag.K) * .data$depreciation.Structures
  ))

# Plots -------------------------------------------------------------------
ts %>% select(IK, firmIK_gross, firmIK_net,date) %>%
  pivot_longer(c(IK, firmIK_gross, firmIK_net)) %>%
  drop_na() %>%
  ggplot(aes(x=date,y=value,color=name))+
  geom_path()+
  easy_labs() +
  ylab(NULL)+
  xlab(NULL)+
  theme(
    legend.title=element_blank()) +
  scale_color_discrete(
    labels=c(
      IK = TeX(r"( $I_t / K_{t-1}$  from Fixed Asset Tables)" ),
      firmIK_gross = TeX(r"(Average $I_t / K^{gross}_{t-1}$  over Compustat firms)" ),
      firmIK_net = TeX(r"(Average $I_t / K^{net}_{t-1}$  over Compustat firms)" )
    )
  )




# BEA data ----------------------------------------------------------------
bea_st <-  tbl(all_data, "bea") %>%
  collect() %>%
  as_tibble()

l1<- gsub('\"', "", "$I_t$", fixed = TRUE)
vtable::st(bea,
           vars = c("IK","depreciation.K"),
           labels = c(
            "I/K",
            "Depreciation rate"
           ),
           digits = 2,
           summ=c("mean(x)","sd(x)","min(x)","max(x)","notNA(x)"),
           summ.names = c("Mean","SD","Min","Max","N"),
           note = paste0(
             labelled::get_variable_labels(bea)$K," ",
             labelled::get_variable_labels(bea)$I
             ),
           out="kable",
           file = "IK.tex"
)

bea_ts <-  tbl(all_data, "bea") %>%
  collect

bea %>% select(I,K,date) %>%
  mutate(across(!date,~ .x)) %>%
  pivot_longer(c("I","K")) %>%
  ggplot(aes(x=date,y=value,color=name))+geom_line()

library(latex2exp)

bea %>% select(IK, depreciation.K,date) %>%
  pivot_longer(c("IK","depreciation.K")) %>%
  ggplot(aes(x=date,y=value,color=name))+
  geom_line()+
  easy_labs() +
  ylab(NULL)+
  xlab(NULL)+
  theme(
    legend.title=element_blank()) +
  scale_color_discrete(
    labels=c(
      "Depreciation rate",TeX(r"( $I_t / K_{t-1}$ )")
      )
    )

# Summary stats -----------------------------------------------------------
library(vtable)

bea <- tbl(all_data, "bea") %>%  collect()
compustat_crsp_aggregate <- tbl(all_data, "bea") %>%  collect()

vtable::st(bea_vars,
           digits = 2,
           summ=c("mean(x)","sd(x)","min(x)","max(x)","notNA(x)"),
           summ.names = c("Mean","SD","Min","Max","N"),
           # labels=c("Federal Funds Rate","log of Real GDP","log of Core PCE Deflator"),
           title = "Title",
           note = "\\textbf{Descriptive Statistics for BEA data:} Fixed asset tables.",
           anchor = "sumstats_for_var",
           file = "output/tables/bea_sumstats.tex",
           # align = 'p{.3\\textwidth}ccccccc',
           # fit.page = '\\textwidth',
           # note.align = 'p{.3\\textwidth}ccccccc',
           out = "latex"
           # out = "viewer"
)

# Plots -------------------------------------------------------------------

tbl(all_data, "crsp") |>
  count(exchange, date) |>
  ggplot(aes(x = date, y = n, color = exchange, linetype = exchange)) +
  geom_line() +
  labs(
    x = NULL, y = NULL, color = NULL, linetype = NULL,
    title = "Monthly number of securities by listing exchange"
  ) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  scale_y_continuous(labels = comma)

tbl(all_data, "crsp") |>
  left_join(tbl(all_data, "fred"), by = "month") |>
  group_by(month, exchange) |>
  summarize(
    mktcap = sum(mktcap, na.rm = TRUE) / price,
    .groups = "drop"
  ) |>
  collect() |>
  mutate(month = ymd(month)) |>
  ggplot(aes(
    x = month, y = mktcap / 1000,
    color = exchange, linetype = exchange
  )) +
  geom_line() +
  labs(
    x = NULL, y = NULL, color = NULL, linetype = NULL,
    title = "Monthly market cap by listing exchange in billions of Dec 2022 USD"
  ) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  scale_y_continuous(labels = comma)






plot_time_series(
  bea,
  year,
  depreciation.K,
  .title = "Average depreciation rate of physical capital",
  .smooth = FALSE,
  .plotly_slider = TRUE
)

plot_time_series(
  bea,
  year,
  IK,
  .title = "Gross investment rate",
  .smooth = FALSE,
  .plotly_slider = TRUE
)


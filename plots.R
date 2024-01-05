library(vtable)
library(tidyverse)
library(tsibble)
library(tstools)
library(hrbrthemes)
library(latex2exp)
library(forecast)

plot_path = "inst/extdata/output/plots/"
csv_path = "inst/extdata/csv/"
ts <- read.csv(paste0(csv_path,"quarterly_time_series",".csv")) %>%
  as_tibble()
# ts <- ts %>% select(!NA.) %>% mutate(Y=gdp)
# ts <- ts %>% mutate(Y=gdp)

# Split time series by category -------------------------------------------
ts_small <- ts %>%
  filter(date>="1972-01-01" & date<"2023-01-01")

ts_agg <- ts_small %>%
  select(c(
    "date",
    "year",
    "gdp",
    "I",
    "I.SA",
    "I.NSA",
    "K",
    "lag.K",
    "depreciation.K"
  )
  ) %>%
  rename(Y=gdp)
ts_agg_ratios <- ts_small %>%
  select(c(
    "date",
    "year",
    "IK",
    "YK",
    "IY"
  )
  )
ts_firm <- ts_small %>%
  select(c(
    "date",
    "year",
    "firmI",
    "firmCapx",
    "firmK_gross",
    "firmK_net"
  )
  )
ts_firm_ratios <- ts_small %>%
  select(c(
    "date",
    "year",
    "firmIK_gross",
    "firmIK_net",
    "firmCapxK_gross",
    "firmCapxK_net",
  )
  )
ts_firm_xs <- ts_small %>%
    select(c(
      "date",
      "year",
      "xs_avg_firmIK_gross",
      "xs_avg_firmIK_net",
      "xs_avg_firmCapxK_gross",
      "xs_avg_firmCapxK_net",
      "xs_med_firmIK_gross",
      "xs_med_firmIK_net",
      "xs_med_firmCapxK_gross",
      "xs_med_firmCapxK_net"
    )
    )
ts_firm_ret <- ts_small %>%
  select(c(
    "date",
    "year",
    "log_ret_ew_cumret",
    "log_ret_vw_cumret",
    "log_ret_excess_ew_cumret",
    "log_ret_excess_vw_cumret",
    "log_ret_ew_sd",
    "log_ret_vw_sd",
    "log_ret_excess_ew_sd",
    "log_ret_excess_vw_sd"
  )
  )
ts_indices <- ts_small %>%
  select(c(
    "date",
    "year",
    "vwretd_excess_cumret",
    "ewretd_excess_cumret",
    "sprtrn_excess_cumret",
    "vwretd_excess_sd",
    "ewretd_excess_sd",
    "sprtrn_excess_sd"
  )
  )

# Summary stats -----------------------------------------------------------


ts_small %>% st
ts_agg %>% st
ts_agg_ratios %>% st
ts_firm %>% st
ts_firm_ratios %>% st
ts_firm_xs %>% st
ts_firm_ret %>% st
ts_indices %>% st

# Plot time series --------------------------------------------------------

## plot style -----------------------------------------------------------
tt <- init_tsplot_theme()
tt$subtitle_transform <- tolower
tt$subtitle_margin <- 1.5
tt$lwd <- c(3,3,3,3,3,3)
tt$line_colors = c(
  "#e6194b",
  "#3cb44b",
  "#0082c8",
  "#f58231",
  "#ffe119",
  "#911eb4",
  "#46f0f0",
  "#f032e6",
  "#d2f53c",
  "#fabebe",
  "#008080"
)
tt$lwd_quarterly_ticks <- 0

pp <- tt
pp$output_wide <- TRUE
pp$margins <- c(NA, 10/if (pp$output_wide) 1 + 1/3 else 1, 10, 7/if (pp$output_wide) 1 + 1/3 else 1)

## time series for plots -------------------------------------------------------
accum_year <- c("firmI","firmCapx","firmK_gross","firmK_net")
avg_year <- c("xs_avg_firmIK_gross","xs_avg_firmIK_net",
              "xs_avg_firmCapxK_gross","xs_avg_firmCapxK_net",
              "xs_med_firmIK_gross","xs_med_firmIK_net",
              "xs_med_firmCapxK_gross","xs_med_firmCapxK_net")

plot_ts <- ts_small %>%
  group_by(year) %>%
  reframe(
    across(
      {accum_year},
      ~ c(NA,NA,NA,sum(.x)),
      .unpack=TRUE
    ),
    across(
      {avg_year},
      ~ c(NA,NA,NA,mean(.x)),
      .unpack=TRUE
    ),
    across(
      !{accum_year} & !{avg_year},
      ~ .
    )
  ) %>%
  mutate(
    across(!c("date"),~ na.interp(.x,linear=TRUE)),
    date = yearquarter(date)
    ) %>%
  as_tsibble() %>%
  mutate(
    firmI_firmK_gross = firmI/firmK_gross,
    firmI_firmK_net = firmI/firmK_net,
    I_K = I/K,
    across(
      c("firmI_firmK_gross","firmI_firmK_net","I_K","IK","firmIK_net","firmIK_gross"),
      ~(scale(.) %>% as.vector),
      .names = "z_{.col}"
    )
  )

write.csv(ts, paste0(csv_path,"plots_time_series",".csv"), row.names = FALSE)

data_excel <- plot_ts %>%
  group_by(row_number()) %>%
  mutate(
    date=as.Date(date),
    year=year(date),
    quarter = quarter(date),
    month = month(date),
    day = day(date),
    Y=Y,
    I=firmI,
    K=firmK_gross,
    ret=log_ret_excess_ew_cumret,
    ret_rf=log_ret_excess_ew_cumret,
    vol=log_ret_excess_ew_sd,
    rf=rnorm(1),
    nom_rf=rnorm(1),
    dividends=exp(rnorm(1)),
    div_yield=20+exp(rnorm(1)),
    C=1000+100*exp(rnorm(1)),
    CP=1000+100*exp(rnorm(1)),
    CP_FIN=1000+100*exp(rnorm(1)),
    credit=exp(rnorm(1)),
    recession=0,
    earnings_growth=0.05+0.001*rnorm(1),
    deflator=exp(rnorm(1)),
    pi=0.05+0.001*exp(rnorm(1))
  ) %>%
  filter(date>="1974-01-01" & date<"2023-01-01") %>%
  arrange(date) %>%
  relocate(date,year,quarter,month,day,Y,C,I,K,ret,ret_rf,rf,nom_rf,vol,dividends,div_yield,CP,CP_FIN,credit,recession,earnings_growth,deflator,pi)

library("openxlsx")
openxlsx::write.xlsx(data_excel, paste0("/Users/fernandoduarte/Dropbox (Personal)/Research/Investment and Vol/data/data.xlsx"), sheetName = "Sheet1",
                     colNames = TRUE, rowNames= FALSE, append = FALSE)


plot_ts <- plot_ts %>%
  as.ts()

## variables in ts_agg ----------------------------------------------------------------
purrr::pmap(
  list(
  out_fmt=list("plot","pdf"),
  thm=list(tt,pp)
  )
  ,
  \(out_fmt,thm)
   tsplot(
    list(
      "NIPA GDP (left axis)" = plot_ts[,"Y"]/1000,
      "BEA K (left axis)" = plot_ts[,"K"]/1000
    ),
    tsr = list(
      "BEA I (right axis)" = plot_ts[,"I"]/1000,
      "NIPA I seas adj (right axis)" = plot_ts[,"I.SA"]/1000,
      "NIPA I not seas adj (right axis)" = plot_ts[,"I.NSA"]/1000
    ),
    plot_title = "Aggregate Variables",
    plot_subtitle = "Trillions of dollars",
    manual_value_ticks_l = seq(0, 30, by = 5),
    filename = paste0(plot_path,"agg"),
    theme = thm,
    output_format = out_fmt
  )
)

purrr::pmap(
  list(
    out_fmt=list("plot","pdf"),
    thm=list(tt,pp)
  )
  ,
  \(out_fmt,thm)
  tsplot(
    list(
      "BEA depreciation rate" = plot_ts[,"depreciation.K"]
    ),
    plot_title = "Depreciation rate",
    plot_subtitle = "Percentage",
    filename = paste0(plot_path,"depreciation"),
    theme = thm,
    output_format = out_fmt
  )
)

purrr::pmap(
  list(
    out_fmt=list("plot","pdf"),
    thm=list(tt,pp)
  )
  ,
  \(out_fmt,thm)
  tsplot(
    list(
      "Equipment" = plot_ts[,"Equipment.I"]/plot_ts[,"Private.nonresidential.fixed.assets.I"],
      "Structures" = plot_ts[,"Structures.I"]/plot_ts[,"Private.nonresidential.fixed.assets.I"],
      "Intangibles" = (plot_ts[,"Private.nonresidential.fixed.assets.I"]-plot_ts[,"Structures.I"]-plot_ts[,"Equipment.I"])/plot_ts[,"Private.nonresidential.fixed.assets.I"]
    ),
    plot_title = "Components of Private Nonresidential Investment in Fixed Assets",
    plot_subtitle = "share of total",
    filename = paste0(plot_path,"agg_inv_shares"),
    theme = thm,
    output_format = out_fmt,
    manual_value_ticks_l = seq(0, 1, by = 0.2),
    left_as_band = TRUE
  )
)

## variables in ts_agg_ratios  -------------------------------------------------
purrr::pmap(
  list(
    out_fmt=list("plot","pdf"),
    thm=list(tt,pp)
  )
  ,
  \(out_fmt,thm)
  tsplot(
    list(
      "BEA I/K (left axis)" = plot_ts[,"I"]/plot_ts[,"K"],
      "BEA I/Y (left axis)" = plot_ts[,"I"]/plot_ts[,"Y"]
    ),
    tsr = list(
      "Y/K (right axis)" = plot_ts[,"Y"]/plot_ts[,"K"]
    ),
    plot_title = "Ratios of Aggregate Variables",
    plot_subtitle = "Ratio",
    filename = paste0(plot_path,"agg_ratios"),
    theme = thm,
    output_format = out_fmt
  )
)

purrr::pmap(
  list(
    out_fmt=list("plot","pdf"),
    thm=list(tt,pp)
  )
  ,
  \(out_fmt,thm)
  tsplot(
    list(
      "I/K" = plot_ts[,"I"]/plot_ts[,"K"],
      "Investment rate $I_t/K_{t-1}$" = plot_ts[,"IK"]
    ),
    plot_title = "Ratios of Aggregate Variables",
    plot_subtitle = "Ratio",
    filename = paste0(plot_path,"agg_inv_rate"),
    theme = thm,
    output_format = out_fmt
  )
)

## variables in ts_firm  -------------------------------------------------
purrr::pmap(
  list(
    out_fmt=list("plot","pdf"),
    thm=list(tt,pp)
  )
  ,
  \(out_fmt,thm)
  tsplot(
    list(
      "Gross K (left axis)" = plot_ts[,"firmK_gross"]/1000,
      "Net K (left axis)" = plot_ts[,"firmK_net"]/1000
    ),
    tsr = list(
      "I (right axis)" = plot_ts[,"firmI"]/1000,
      "Capx (right axis)" = plot_ts[,"firmCapx"]/1000
    ),
    plot_title = "Compustat Variables",
    plot_subtitle = "Trillions of dollars (sum over all firms)",
    filename = paste0(plot_path,"firm_IK"),
    theme = thm,
    output_format = out_fmt
  )
)

purrr::pmap(
  list(
    out_fmt=list("plot","pdf"),
    thm=list(tt,pp)
  )
  ,
  \(out_fmt,thm)
  tsplot(
    list(
      "Firm gross K/K (left axis)" = plot_ts[,"firmK_gross"]/plot_ts[,"K"],
      "Firm net K/K (left axis)" = plot_ts[,"firmK_net"]/plot_ts[,"K"]
    ),
    tsr = list(
      "Firm I/I (right axis)" = plot_ts[,"firmI"]/plot_ts[,"I"],
      "Capx/I (right axis)" = plot_ts[,"firmCapx"]/plot_ts[,"I"]
    ),
    plot_title = "Economy-wide and Compustat Aggregates",
    plot_subtitle = "Ratio of sum over all firms to economy-wide aggregate",
    filename = paste0(plot_path,"firm_and_agg_IK"),
    theme = thm,
    output_format = out_fmt
  )
)

## variables in ts_firm_ratios  -------------------------------------------------
purrr::pmap(
  list(
    out_fmt=list("plot","pdf"),
    thm=list(tt,pp)
  )
  ,
  \(out_fmt,thm)
  tsplot(
    list(
      "I/Gross K (left axis)" = plot_ts[,"firmI"]/plot_ts[,"firmK_gross"],
      "I/Net K (left axis)" = plot_ts[,"firmI"]/plot_ts[,"firmK_net"]
    ),
    tsr = list(
      "Capx/Gross K (right axis)" = plot_ts[,"firmCapx"]/plot_ts[,"firmK_gross"],
      "Capx/Net K (right axis)" =plot_ts[,"firmCapx"]/plot_ts[,"firmK_net"]
    ),
    plot_title = "Compustat Aggregates",
    plot_subtitle = "Ratio",
    filename = paste0(plot_path,"firm_IK_ratios"),
    theme = thm,
    output_format = out_fmt
  )
)

purrr::pmap(
  list(
    out_fmt=list("plot","pdf"),
    thm=list(tt,pp)
  )
  ,
  \(out_fmt,thm)
  tsplot(
    list(
      "Firm I/gross K (left axis)" = plot_ts[,"firmK_gross"]/plot_ts[,"K"],
      "Firm I/net K (left axis)" = plot_ts[,"firmK_net"]/plot_ts[,"K"],
      "BEA I/K (left axis)" = plot_ts[,"I"]/plot_ts[,"K"]
    ),
    plot_title = "Economy-wide and Compustat I/K ratio",
    plot_subtitle = "Ratio",
    filename = paste0(plot_path,"firm_and_agg_IK_ratios"),
    theme = thm,
    output_format = out_fmt
  )
)

purrr::pmap(
  list(
    out_fmt=list("plot","pdf"),
    thm=list(tt,pp)
  )
  ,
  \(out_fmt,thm)
  tsplot(
    list(
      "Firm I/gross K (left axis)" = plot_ts[,"z_firmI_firmK_gross"],
      "Firm I/net K (left axis)" = plot_ts[,"z_firmI_firmK_net"],
      "BEA I/K (left axis)" = plot_ts[,"z_I_K"]
    ),
    plot_title = "Economy-wide and Compustat I/K ratios",
    plot_subtitle = "Ratios, standardized to mean 0 and variance 1",
    filename = paste0(plot_path,"firm_and_agg_IK_ratios_z"),
    theme = thm,
    output_format = out_fmt
  )
)

## variables in ts_firm_xs  -------------------------------------------------
purrr::pmap(
  list(
    out_fmt=list("plot","pdf"),
    thm=list(tt,pp)
  )
  ,
  \(out_fmt,thm)
  tsplot(
    list(
      "I/Gross K (left axis)" = plot_ts[,"xs_avg_firmIK_gross"],
      "Capx/Gross K  (right axis)" = plot_ts[,"xs_avg_firmCapxK_gross"]
    ),
    tsr = list(
      "I/Net K (left axis)" = plot_ts[,"xs_avg_firmCapxK_net"],
      "Capx/Net K  (right axis)" = plot_ts[,"xs_avg_firmIK_net"]
    ),
    plot_title = "Compustat Cross-sectional Averages",
    plot_subtitle = "Average over all firms",
    filename = paste0(plot_path,"firm_IK_xs"),
    theme = thm,
    output_format = out_fmt
  )
)

purrr::pmap(
  list(
    out_fmt=list("plot","pdf"),
    thm=list(tt,pp)
  )
  ,
  \(out_fmt,thm)
  tsplot(
    list(
      "I/Gross K (left axis)" = plot_ts[,"xs_med_firmIK_gross"],
      "Capx/Gross K  (right axis)" = plot_ts[,"xs_med_firmCapxK_gross"]
    ),
    tsr = list(
      "I/Net K (left axis)" = plot_ts[,"xs_med_firmCapxK_net"],
      "Capx/Net K  (right axis)" = plot_ts[,"xs_med_firmIK_net"]
    ),
    plot_title = "Compustat Cross-sectional Median",
    plot_subtitle = "Median of firm values",
    filename = paste0(plot_path,"firm_IK_xs_med"),
    theme = thm,
    output_format = out_fmt
  )
)








library(devtools)
load_all()
library(hrbrthemes)
library(tidyverse)
library(tstools)
library(tsibble)
library(latex2exp)
library(forecast)
library(sandwich)
library(lmtest)
library(ggpubr)
library(pracma)

csv_path = "inst/extdata/csv/"
ts <- read.csv(paste0(csv_path,"quarterly_time_series",".csv"))

ts_small <- ts %>%
  select(c(
      "date",
      "year",
      "gdp",
      "I",
      "I.SA",
      "I.NSA",
      "K",
      "lag.K",
      "depreciation.K",
      "IK",
      "YK",
      "IY",
      "firmI",
      "firmCapx",
      "firmK_gross",
      "firmK_net",
      "firmIK_gross",
      "firmIK_net",
      "firmCapxK_gross",
      "firmCapxK_net",
      "xs_avg_firmIK_gross",
      "xs_avg_firmIK_net",
      "xs_avg_firmCapxK_gross",
      "xs_avg_firmCapxK_net",
      "log_ret_ew_cumret",
      "log_ret_ew_sd",
      "log_ret_excess_ew_cumret",
      "log_ret_excess_ew_sd",
      "log_ret_vw_cumret",
      "log_ret_vw_sd",
      "log_ret_excess_vw_cumret",
      "log_ret_excess_vw_sd",
      "vwretd_excess_cumret",
      "vwretd_excess_sd",
      "ewretd_excess_cumret",
      "ewretd_excess_sd",
      "sprtrn_excess_cumret",
      "sprtrn_excess_sd"
  ))


ts_a <- ts %>%
  filter(date>="1950-01-01" & date<"2023-01-01") %>%
  as_tibble() %>%
  mutate(
    K = na.interp(K,linear=TRUE),
    I = na.interp(I,linear=TRUE),
    IK = na.interp(IK,linear=TRUE),
    YK = na.interp(YK,linear=TRUE),
    IY = na.interp(IY,linear=TRUE),
    IKq.SA = K/I.SA,
    IKq.NSA = K/I.NSA
  ) %>%
  mutate(
    date = lubridate::ymd(date, truncated = 2L)
  )

ts_long <- ts %>%
  pivot_longer(
    !date,
    names_to = "series",
    values_to = "value"
  ) %>%
  drop_na()

ts_a_long <- ts_a %>%
  mutate(year=year(date)) %>%
  select(!date) %>%
  pivot_longer(
    !year,
    names_to = "series",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  mutate(
    date = lubridate::ymd(year, truncated = 2L)
  ) %>%
  select(!year)

# Plots -------------------------------------------------------------------

csv_path = "inst/extdata/csv/"
ts <- read.csv(paste0(csv_path,"quarterly_time_series",".csv"))
plot_path = "inst/extdata/output/plots/"

plot_ts <-  ts_a %>%
  filter(date>="1972-01-01" & date<"2023-01-01") %>%
  mutate(
    K = K/1000,
    firmK_gross = firmK_gross/1000,
    firmK_net = firmK_net/1000,
    firmCapx = firmCapx/1000,
    gdp = gdp/1000,
    YfirmK_gross = gdp/firmK_gross,
    YfirmK_net = gdp/firmK_net,
    across(
      c("YfirmK_net","YfirmK_gross","YK","IK","firmIK_net","firmIK_gross"),
      ~(scale(.) %>% as.vector),
      .names = "z_{.col}"
    ),
    across(
      c("YfirmK_net","YfirmK_gross","YK","IK","firmIK_net","firmIK_gross"),
      ~ (detrend(as.vector(.), 'linear') %>% as.vector),
      .names = "detrend_{.col}"
    )
  ) %>%
  mutate(
    date = yearquarter(date)
  ) %>%
  as_tsibble() %>%
  as.ts()

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

purrr::map(
  c("plot","pdf"),
~ tsplot(
  list(
    "BEA K" = plot_ts[,"K"],
    "Compustat net K" = plot_ts[,"firmK_net"],
    "Compustat gross K" = plot_ts[,"firmK_gross"]
  ),
  plot_title = "Aggregate Capital",
  plot_subtitle = "Trillions of dollars",
  manual_value_ticks_l = seq(0, 30, by = 5),
  manual_ticks_x = as.vector(time(plot_ts)),
  theme = tt,
  filename = paste0(plot_path,"capital"),
  output_format = .
)
)

purrr::map(
  c("plot","pdf"),
  ~ tsplot(
    list(
      "BEA I" = plot_ts[,"I"],
      "Compustat I" = plot_ts[,"firmI"],
      "Compustat Capx" = plot_ts[,"firmCapx"]
    ),
    plot_title = "Aggregate Investment",
    plot_subtitle = "Billions of dollars",
    manual_ticks_x = as.vector(time(plot_ts)),
    theme = tt,
    filename = paste0(plot_path,"invest"),
    output_format = .
  )
)

purrr::map(
  c("plot","pdf"),
  ~ tsplot(
    list(
      "BEA I/K" = plot_ts[,"IK"],
      "Compustat I/K" = plot_ts[,"firmIK_net"],
      "Compustat I/K" = plot_ts[,"firmIK_gross"]
    ),
    plot_title = "Investment Rate",
    plot_subtitle = TeX("Ratio $I_t/K_{t-1}$"),
    manual_ticks_x = as.vector(time(plot_ts)),
    manual_value_ticks_l = seq(0.03, 0.15, by = 0.03),
    theme = tt,
    filename = paste0(plot_path,"ir"),
    output_format = .
  )
)

purrr::map(
  c("plot","pdf"),
  ~ tsplot(
    list(
      "Standardized BEA I/K" = plot_ts[,"z_IK"],
      "Standardized Compustat I/K" = plot_ts[,"z_firmIK_net"],
      "Standardized Compustat I/K" = plot_ts[,"z_firmIK_gross"]
    ),
    plot_title = "Standardized Investment Rate",
    plot_subtitle = "Ratio, standardized to mean 0 and std dev 1",
    manual_ticks_x = as.vector(time(plot_ts)),
    theme = tt,
    filename = paste0(plot_path,"std_ir"),
    output_format = .
  )
)

purrr::map(
  c("plot","pdf"),
  ~ tsplot(
    list(
      "Detrended BEA I/K" = plot_ts[,"detrend_IK"],
      "Detrended Compustat I/K" = plot_ts[,"detrend_firmIK_net"],
      "Detrended Compustat I/K" = plot_ts[,"detrend_firmIK_gross"]
    ),
    plot_title = "Detrended Investment Rate",
    plot_subtitle = "Ratio, linear time trend removed",
    manual_ticks_x = as.vector(time(plot_ts)),
    theme = tt,
    filename = paste0(plot_path,"detrend_ir"),
    output_format = .
  )
)


purrr::map(
  c("plot","pdf"),
  ~ tsplot(
    list(
      "Mean of $I_it/K_{i,t-1}$ net" = plot_ts[,"xs_avg_firmIK_net"],
      "Mean of $I_it/K_{i,t-1}$ gross" = plot_ts[,"xs_avg_firmIK_gross"]
    ),
    plot_title = "Investment Rate",
    plot_subtitle = "Ratio",
    manual_ticks_x = as.vector(time(plot_ts)),
    theme = tt,
    filename = paste0(plot_path,"xs_ir"),
    output_format = .
  )
)


purrr::map(
  c("plot","pdf"),
  ~ tsplot(
    list(
      "Y/BEA K" = plot_ts[,"YK"],
      "Y/Compustat K net" = plot_ts[,"YfirmK_net"],
      "Y/Compustat K gross" = plot_ts[,"YfirmK_gross"]
    ),
    plot_title = "Y/K",
    plot_subtitle = "Ratio",
    manual_ticks_x = as.vector(time(plot_ts)),
    manual_value_ticks_l = seq(0, 5, by = 1),
    theme = tt,
    filename = paste0(plot_path,"yk"),
    output_format = .
  )
)


purrr::map(
  c("plot","pdf"),
  ~ tsplot(
    list(
      "Standardized Y/BEA K" = plot_ts[,"z_YK"],
      "Standardized Y/Compustat K net" = plot_ts[,"z_YfirmK_net"],
      "Standardized Y/Compustat K gross" = plot_ts[,"z_YfirmK_gross"]
    ),
    plot_title = "Standardized Y/K",
    plot_subtitle = "Ratio, standardized to mean 0 and std dev 1",
    manual_ticks_x = as.vector(time(plot_ts)),
    theme = tt,
    filename = paste0(plot_path,"std_yk"),
    output_format = .
  )
)




s1 <- ggplot(plot_ts %>% as_tibble, aes(x=I, y=firmI)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()
s1


s2 <- ggplot(plot_ts %>% as_tibble, aes(x=K, y=firmK_gross)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()
s2

s3 <- ggplot(plot_ts %>% as_tibble, aes(x=IK, y=firmCapxK_gross)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()
s3

s4 <- ggplot(plot_ts %>% as_tibble, aes(x=IK, y=firmCapxK_net)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()
s4


# regressions -------------------------------------------------------------


reg_ts <- ts %>%
  filter(date>="1972-01-01" & date<"2023-01-01") %>%
  arrange(date) %>%
  mutate(
    k = na.interp(log(K),linear=TRUE),
    i = na.interp(log(NIPA.Structures.I.SA + NIPA.Equipment.I.SA),linear=TRUE),
    y = na.interp(log(tsY),linear=TRUE),
    firm_ik_gross = log(firmIK_gross),
    firm_ik_net = log(firmIK_net),
    firm_ik = firm_ik_gross,
    agg_yk = y-k,
    agg_ik = y-i,
    ret_rf = log_ret_excess_vw_avg,
    vol = log_ret_excess_vw_sd,
    sumret = cumsum(replace_na(ret_rf, 0)),
    sumvol = cumsum(replace_na(vol, 0)),
    across(c("agg_yk","agg_ik"), ~(scale(.) %>% as.vector), .names = "z_{.col}"),
    across(c("agg_yk","agg_ik"), ~ (detrend(as.vector(.),'linear') %>% as.vector), .names = "detrend_{.col}"),
    ik = detrend_agg_ik,
    yk = detrend_agg_yk,
    date = yearquarter(date),
    .keep = "used"
  ) %>%
  drop_na(ret_rf) %>%
  drop_na(ik) %>%
  as_tsibble()

pred_reg <- function(data,y,x,k, nw_lags = 3, overlapTransform = FALSE){
  lag <- k
  if (overlapTransform){
    reg_data <- data %>%
      as_tibble() %>%
      select(all_of({{x}})) %>%
      add_column(ones=1, .before=1) %>%
      reframe(
        across(
          everything(),
          ~ transformX(as.matrix(.),{{k}}),
          .unpack = TRUE
        )
      ) %>%
      mutate(
        reg_y = data$ret_rf[2:length(data$ret_rf)]
      )
    fmla <- as.formula(
      paste(
        "reg_y ~ ones +", paste({{ x }}, collapse= "+"), "-1"
      )
    )
  } else {
    reg_data <- data %>%
      mutate(
        cumsum_y = cumsum(replace_na(.data[[y]], 0)),
        reg_y = difference(cumsum_y, lag = lag),
        across(all_of({{ x }}), ~ lag(., lag + 1)),
      ) %>%
      select(
        {{y}},
        cumsum_y,
        reg_y,
        all_of({{x}})
      ) %>%
      drop_na()

    fmla <- as.formula(
      paste(
        "reg_y ~ ", paste({{ x }}, collapse= "+")
      )
    )
  }
  fit <- lm(fmla,data=reg_data)
  tstat <- coeftest(fit, vcov = NeweyWest(fit,lag=nw_lags,prewhite=FALSE))
}

ret_ik<-list()
ret_ik_yk<-list()
vol_ik<-list()
vol_ik_yk<-list()

firm_ret<-list()
firm_ret_ik<-list()
firm_ret_ik_yk<-list()
firm_vol<-list()
firm_vol_ik<-list()
firm_vol_ik_yk<-list()

max_h <- 16
for (k in 1:max_h){
  # aggregate vars only
  ret_ik[[k]] <- pred_reg(reg_ts,c("ret_rf"),c("ik"),k)
  ret_ik_yk[[k]] <- pred_reg(reg_ts,c("ret_rf"),c("ik","yk"),k)
  vol_ik[[k]] <- pred_reg(reg_ts,c("vol"),c("ik"),k)
  vol_ik_yk[[k]] <- pred_reg(reg_ts,c("vol"),c("ik","yk"),k)

  # firm and aggregate
  firm_ret[[k]] <- pred_reg(reg_ts,c("ret_rf"),c("firm_ik"),k)
  firm_ret_ik[[k]] <- pred_reg(reg_ts,c("ret_rf"),c("firm_ik","ik"),k)
  firm_ret_ik_yk[[k]] <- pred_reg(reg_ts,c("ret_rf"),c("firm_ik","ik","yk"),k)
  firm_vol[[k]] <- pred_reg(reg_ts,c("vol"),c("firm_ik"),k)
  firm_vol_ik[[k]] <- pred_reg(reg_ts,c("vol"),c("firm_ik","ik"),k)
  firm_vol_ik_yk[[k]] <- pred_reg(reg_ts,c("vol"),c("firm_ik","ik","yk"),k)
}

# aggregate vars only
c1=bind_rows(
  purrr::imap(
    list(
      ret_ik = ret_ik,
      vol_ik = vol_ik
    ),
    \(x,i)
    tibble(
      horizon = 1:max_h,
      a1 = sapply(x, "[", "ik","Estimate"),
      t1 = sapply(x, "[", "ik","t value"),
      a2 = NA,
      t2 = NA,
      name = i,
      nreg= 1
    )
  )
)
c2=bind_rows(
  purrr::imap(
    list(
      ret_ik_yk = ret_ik_yk,
      vol_ik_yk = vol_ik_yk
    ),
    \(x,i)
    tibble(
      horizon = 1:max_h,
      a1 = sapply(x, "[", "ik","Estimate"),
      t1 = sapply(x, "[", "ik","t value"),
      a2 = sapply(x, "[", "yk","Estimate"),
      t2 = sapply(x, "[", "yk","t value"),
      name = i,
      nreg=2
    )
  )
)
coeffs_df = bind_rows(c1,c2) %>%
  mutate(
    yvar = str_extract(name,"vol|ret")
  )
coeffs_df


coeff_ret_ik<-ggplot(
  coeffs_df %>% filter(name=="ret_ik"),
  aes(x=horizon, y=a1, color=name)
) +
  geom_line() +
  theme_bw() +
  labs(
    title = TeX(c("$ret = a_0 + a_1 log(I/K) + e$")),
    y = TeX(c("$a_1$"))
  ) + theme(legend.position = "none")

tstat_ret_ik<-ggplot(
  coeffs_df %>% filter(name=="ret_ik"),
  aes(x=horizon, y=t1, color=name)
) +
  geom_line() +
  theme_bw() +
  labs(
    title = TeX(c("$ret = a_0 + a_1 log(I/K) + e$")),
    y = TeX(c("tstat for $a_1$"))
  ) + theme(legend.position = "none")

coeff_vol_ik<-ggplot(
  coeffs_df %>% filter(name=="vol_ik"),
  aes(x=horizon, y=a1, color=name)
) +
  geom_line() +
  theme_bw() +
  labs(
    title = TeX(c("$vol = a_0 + a_1 log(I/K) + e$")),
    y = TeX(c("$a_1$"))
  ) + theme(legend.position = "none")

tstat_vol_ik<-ggplot(
  coeffs_df %>% filter(name=="vol_ik"),
  aes(x=horizon, y=t1, color=name)
) +
  geom_line() +
  theme_bw() +
  labs(
    title = TeX(c("$vol = a_0 + a_1 log(I/K) + e$")),
    y = TeX(c("tstat for $a_1$"))
  ) + theme(legend.position = "none")


figure1 <- ggarrange(coeff_ret_ik, coeff_vol_ik,tstat_ret_ik,tstat_vol_ik,
                     ncol = 2, nrow = 2
)
figure1


coeff_ret_ik_yk<-ggplot(
  coeffs_df %>% filter(name=="ret_ik_yk"),
  aes(x=horizon, y=a1, color=name)
) +
  geom_line(color = "blue") +
  theme_bw() +
  labs(
    title = TeX(c("$ret = a_0 + a_1 log(I/K) + a_2 log(Y/K) + e$")),
    y = TeX(c("$a_1$"))
  ) + theme(legend.position = "none")

tstat_ret_ik_yk<-ggplot(
  coeffs_df %>% filter(name=="ret_ik_yk"),
  aes(x=horizon, y=t1, color=name)
) +
  geom_line(color = "blue") +
  theme_bw() +
  labs(
    title = TeX(c("$ret = a_0 + a_1 log(I/K)+ a_2 log(Y/K) + e$")),
    y = TeX(c("tstat for $a_1$"))
  ) + theme(legend.position = "none")

coeff_vol_ik_yk<-ggplot(
  coeffs_df %>% filter(name=="vol_ik_yk"),
  aes(x=horizon, y=a1, color=name)
) +
  geom_line(color = "blue") +
  theme_bw() +
  labs(
    title = TeX(c("$vol = a_0 + a_1 log(I/K)+ a_2 log(Y/K) + e$")),
    y = TeX(c("$a_1$"))
  ) + theme(legend.position = "none")

tstat_vol_ik_yk<-ggplot(
  coeffs_df %>% filter(name=="vol_ik_yk"),
  aes(x=horizon, y=t1, color=name)
) +
  geom_line(color = "blue") +
  theme_bw() +
  labs(
    title = TeX(c("$vol = a_0 + a_1 log(I/K)+ a_2 log(Y/K) + e$")),
    y = TeX(c("tstat for $a_1$"))
  ) + theme(legend.position = "none")


figure2 <- ggarrange(coeff_ret_ik_yk, coeff_vol_ik_yk,tstat_ret_ik_yk,tstat_vol_ik_yk,
                     # labels = c(
                     #   "Coefficient on log(I/K)",
                     #   "t-stat for coefficient on log(I/K)",
                     #   "Coefficient on log(Y/K)",
                     #   "t-stat for coefficient on log(Y/K)"
                     #   ),
                     ncol = 2, nrow = 2
)
figure2


# -------------------------------------------------------------------------

# firm and aggregate
c3=bind_rows(
  purrr::imap(
    list(
      firm_ret = firm_ret,
      firm_vol = firm_vol
    ),
    \(x,i)
    tibble(
      horizon = 1:max_h,
      a1 = sapply(x, "[", "firm_ik","Estimate"),
      t1 = sapply(x, "[", "firm_ik","t value"),
      a2 = NA,
      t2 = NA,
      name = i,
      nreg= 1
    )
  )
)
c4=bind_rows(
  purrr::imap(
    list(
      firm_ret_ik = firm_ret_ik,
      firm_vol_ik = firm_vol_ik
    ),
    \(x,i)
    tibble(
      horizon = 1:max_h,
      a1 = sapply(x, "[", "firm_ik","Estimate"),
      t1 = sapply(x, "[", "firm_ik","t value"),
      a2 = sapply(x, "[", "ik","Estimate"),
      t2 = sapply(x, "[", "ik","t value"),
      name = i,
      nreg=2
    )
  )
)
c5=bind_rows(
  purrr::imap(
    list(
      firm_ret_ik_yk = firm_ret_ik_yk,
      firm_vol_ik_yk = firm_vol_ik_yk
    ),
    \(x,i)
    tibble(
      horizon = 1:max_h,
      a1 = sapply(x, "[", "firm_ik","Estimate"),
      t1 = sapply(x, "[", "firm_ik","t value"),
      a2 = sapply(x, "[", "ik","Estimate"),
      t2 = sapply(x, "[", "ik","t value"),
      a3 = sapply(x, "[", "yk","Estimate"),
      t3 = sapply(x, "[", "yk","t value"),
      name = i,
      nreg=3
    )
  )
)

coeffs_firm_df = bind_rows(c3,c4,c5) %>%
  mutate(
    yvar = str_extract(name,"firm_vol|firm_ret")
  )
coeffs_firm_df


coeff_ret_firm<-ggplot(
  coeffs_firm_df %>% filter(name=="firm_ret"),
  aes(x=horizon, y=a1, color=name)
) +
  geom_line() +
  theme_bw() +
  labs(
    title = TeX(c("$ret = a_0 + a_1 log(I^{firms}/ firm K^{firms}) + e$")),
    y = TeX(c("$a_1$"))
  ) + theme(legend.position = "none")

tstat_ret_firm<-ggplot(
  coeffs_firm_df %>% filter(name=="firm_ret"),
  aes(x=horizon, y=t1, color=name)
) +
  geom_line() +
  theme_bw() +
  labs(
    title = TeX(c("$ret = a_0 + a_1 log(I^{firms}/ firm K^{firms}) + e$")),
    y = TeX(c("tstat for $a_1$"))
  ) + theme(legend.position = "none")

coeff_vol_firm<-ggplot(
  coeffs_firm_df %>% filter(name=="firm_vol"),
  aes(x=horizon, y=a1, color=name)
) +
  geom_line() +
  theme_bw() +
  labs(
    title = TeX(c("$vol = a_0 + a_1 log(I^{firms}/ firm K^{firms})  + e$")),
    y = TeX(c("$a_1$"))
  ) + theme(legend.position = "none")

tstat_vol_firm<-ggplot(
  coeffs_firm_df %>% filter(name=="firm_vol"),
  aes(x=horizon, y=t1, color=name)
) +
  geom_line() +
  theme_bw() +
  labs(
    title = TeX(c("$vol = a_0 + a_1 log(I^{firms}/ firm K^{firms})  + e$")),
    y = TeX(c("tstat for $a_1$"))
  ) + theme(legend.position = "none")


figure3 <- ggarrange(coeff_ret_firm, coeff_vol_firm,tstat_ret_firm,tstat_vol_firm,
                     ncol = 2, nrow = 2
)
figure3


coeff_ret_firm_ik<-ggplot(
  coeffs_firm_df %>% filter(name=="firm_ret_ik"),
  aes(x=horizon, y=a1, color=name)
) +
  geom_line(color = "blue") +
  theme_bw() +
  labs(
    title = TeX(c("$ret = a_0 + a_1 log(I^{firms}/ firm K^{firms}) + a_2 log(I/K) + e$")),
    y = TeX(c("$a_1$"))
  ) + theme(legend.position = "none")

tstat_ret_firm_ik<-ggplot(
  coeffs_firm_df %>% filter(name=="firm_ret_ik"),
  aes(x=horizon, y=t1, color=name)
) +
  geom_line(color = "blue") +
  theme_bw() +
  labs(
    title = TeX(c("$ret = a_0 + a_1 log(I^{firms}/ firm K^{firms}) + a_2 log(I/K) + e$")),
    y = TeX(c("tstat for $a_1$"))
  ) + theme(legend.position = "none")

coeff_vol_firm_ik<-ggplot(
  coeffs_firm_df %>% filter(name=="firm_vol_ik"),
  aes(x=horizon, y=a1, color=name)
) +
  geom_line(color = "blue") +
  theme_bw() +
  labs(
    title = TeX(c("$vol = a_0 + a_1 log(I^{firms}/ firm K^{firms}) + a_2 log(I/K) + e$")),
    y = TeX(c("$a_1$"))
  ) + theme(legend.position = "none")

tstat_vol_firm_ik<-ggplot(
  coeffs_firm_df %>% filter(name=="firm_vol_ik"),
  aes(x=horizon, y=t1, color=name)
) +
  geom_line(color = "blue") +
  theme_bw() +
  labs(
    title = TeX(c("$vol = a_0 + a_1 log(I^{firms}/ firm K^{firms}) + a_2 log(I/K) + e$")),
    y = TeX(c("tstat for $a_1$"))
  ) + theme(legend.position = "none")


figure4 <- ggarrange(coeff_ret_firm_ik,
                     coeff_vol_firm_ik,
                     tstat_ret_firm_ik,
                     tstat_vol_firm_ik,
                     # labels = c(
                     #   "Coefficient on log(I/K)",
                     #   "t-stat for coefficient on log(I/K)",
                     #   "Coefficient on log(Y/K)",
                     #   "t-stat for coefficient on log(Y/K)"
                     #   ),
                     ncol = 2, nrow = 2
)
figure4


coeff_ret_firm_ik_yk<-ggplot(
  coeffs_firm_df %>% filter(name=="firm_ret_ik_yk"),
  aes(x=horizon, y=a1, color=name)
) +
  geom_line(color = "blue") +
  theme_bw() +
  labs(
    title = TeX(c("$ret = a_0 + a_1 log(I^{firms}/ firm K^{firms}) + a_2 log(I/K) + a_3 log(Y/K) + e$")),
    y = TeX(c("$a_1$"))
  ) + theme(legend.position = "none")

tstat_ret_firm_ik_yk<-ggplot(
  coeffs_firm_df %>% filter(name=="firm_ret_ik_yk"),
  aes(x=horizon, y=t1, color=name)
) +
  geom_line(color = "blue") +
  theme_bw() +
  labs(
    title = TeX(c("$ret = a_0 + a_1 log(I^{firms}/ firm K^{firms}) + a_2 log(I/K) + a_3 log(Y/K) + e$")),
    y = TeX(c("tstat for $a_1$"))
  ) + theme(legend.position = "none")

coeff_vol_firm_ik_yk<-ggplot(
  coeffs_firm_df %>% filter(name=="firm_vol_ik_yk"),
  aes(x=horizon, y=a1, color=name)
) +
  geom_line(color = "blue") +
  theme_bw() +
  labs(
    title = TeX(c("$vol = a_0 + a_1 log(I^{firms}/ firm K^{firms}) + a_2 log(I/K) + a_3 log(Y/K) + e$")),
    y = TeX(c("$a_1$"))
  ) + theme(legend.position = "none")

tstat_vol_firm_ik_yk<-ggplot(
  coeffs_firm_df %>% filter(name=="firm_vol_ik_yk"),
  aes(x=horizon, y=t1, color=name)
) +
  geom_line(color = "blue") +
  theme_bw() +
  labs(
    title = TeX(c("$vol = a_0 + a_1 log(I^{firms}/ firm K^{firms}) + a_2 log(I/K) + a_3 log(Y/K)  + e$")),
    y = TeX(c("tstat for $a_1$"))
  ) + theme(legend.position = "none")


figure5 <- ggarrange(coeff_ret_firm_ik_yk,
                     coeff_vol_firm_ik_yk,
                     tstat_ret_firm_ik_yk,
                     tstat_vol_firm_ik_yk,
                     # labels = c(
                     #   "Coefficient on log(I/K)",
                     #   "t-stat for coefficient on log(I/K)",
                     #   "Coefficient on log(Y/K)",
                     #   "t-stat for coefficient on log(Y/K)"
                     #   ),
                     ncol = 2, nrow = 2
)
figure5


# scatter -----------------------------------------------------------------
library(hrbrthemes)
# with linear trend
p2 <- ggplot(reg_ts, aes(x=exp(i), y=exp(firmIK_net))) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()
p2

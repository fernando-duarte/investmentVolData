library(devtools)
load_all()

# Libraries
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(tidyverse)
library(RSQLite)
library(scales)
library(ggplot2)
library(ggeasy)
library(vtable)
library(timetk)
library(latex2exp)
library(TSstudio)

csv_path = "inst/extdata/csv/"
ts <- read.csv(paste0(csv_path,"quarterly_time_series",".csv"))

ts_a <- ts %>%
  as_tibble() %>%
  mutate( year = lubridate::year(date)) %>%
  select(!date) %>%
  summarize(
    # across(everything(),~ mean(., na.rm = TRUE)),
    across(everything(),~ last(., na_rm = TRUE)),
    .by = year
  ) %>%
  mutate(
    date = lubridate::ymd(year, truncated = 2L)
  )

ts_long <- ts %>%
  pivot_longer(
    !date,
    names_to = "series",
    values_to = "value"
  ) %>%
  drop_na()

ts_a_long <- ts_a %>%
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


ts_plot(
  ts_a %>%
    select(c("date","Equipment.K","Structures.K","FA.Equipment.K","FA.Structures.K")) %>%
    mutate(across(!date,~ ./10^3))
  ,
  title = "Fixed Asset Tables: Stock of Fixed Assets",
  Ytitle = "Trillions of dollars"
)

ts_plot(
  ts_a %>%
    select(c("date","Equipment.I","Structures.I")) %>%
    mutate(across(!date,~ ./10^3))
  ,
  title = "Fixed Asset Tables: Investment",
  Ytitle = "Trillions of dollars"
)

ts_plot(
  ts_a %>%
    select(c("date","NIPA.Structures.I","NIPA.Structures.I.SA","NIPA.Structures.I.NSA","Structures.I")) %>%
    mutate(across(!date,~ ./10^3))
  ,
  title = "Investment in Structures",
  Ytitle = "Trillions of dollars"
)

ts_plot(
  ts_a %>%
    select(c("date","NIPA.Equipment.I","NIPA.Equipment.I.SA","NIPA.Equipment.I.NSA","Equipment.I")) %>%
    mutate(across(!date,~ ./10^3))
  ,
  title = "Investment in Equipment",
  Ytitle = "Trillions of dollars"
)

ts_plot(
  ts_a %>%
    select(c("date","Equipment.D","Structures.D","FA.Structures.D","FA.Equipment.D")) %>%
    mutate(across(!date,~ ./10^3))
  ,
  title = "Fixed Asset Tables: Depreciation",
  Ytitle = "Trillions of dollars"
)

ts_plot(
  ts_a %>%
    select(c("date","firmI","I" ))%>%
    filter(date>="1972-01-01" & date<"2023-01-01")
  ,
  title = "Investment",
  Ytitle = "Billions of dollars"
)


ts_plot(
  ts_a %>%
    select(c("date","firmI","I" )) %>%
    mutate(ratio=firmI/I) %>%
    select(date,ratio) %>%
    filter(date>="1972-01-01" & date<"2023-01-01")
  ,
  title = "BEA investment / Compustat investment",
  Ytitle = "Ratio"
)

ts_plot(
  ts_a %>%
    select(c("date","K","firmK_gross","firmK_net" ))%>%
    filter(date>="1972-01-01" & date<"2023-01-01"),
  title = "Capital",
  Ytitle = "Trillions of dollars"
)


ts_plot(
  ts_a %>%
    select(c("date","IK","firmIK_gross","firmIK_net")) %>%
    filter(date>="1972-01-01" & date<"2023-01-01"),
  title = "Investment rate",
  Ytitle = "Trillions of dollars"
)

ts_plot(
  ts_a %>%
    select(c("date","IK","firmIK_gross","firmIK_net")) %>%
    filter(date>="1972-01-01" & date<"2023-01-01")%>%
    mutate(
      ratio_gross=firmIK_gross/IK,
      ratio_net=firmIK_net/IK
    ) %>%
    select(date,ratio_gross,ratio_net),
  title = "Ratio of I/K in Compustat and I/K in BEA",
  Ytitle = "Ratio"
)

ts_plot(
  ts_a %>%
    select(c("date","xs_avg_firmIK_net","xs_avg_firmIK_gross")) %>%
    filter(date>="1972-01-01" & date<"2023-01-01"),
  title = "Investment rate",
  Ytitle = "Millions of dollars"
)

ts_plot(
  ts_a %>%
    select(c("date","depreciation.K"))%>%
    filter(date>="1972-01-01" & date<"2023-01-01")
  ,
  title = "Depreciation rate of physical capital",
  Ytitle = "% of K"
)

ts_plot(
  ts_a %>%
    select(c("date","YK","IK")) %>%
    filter(date>="1972-01-01" & date<"2023-01-01")
  ,
  title = "Ratios Y/K and I/K",
  Ytitle = "Ratio"
)

ts_plot(
  ts_a %>%
    select(c("date","YK"))
  ,
  title = "Ratio Y/K",
  Ytitle = "Ratio"
)

ts_plot(
  ts_a %>%
    select(c("date","IY"))
  ,
  title = "Ratio I/Y",
  Ytitle = "Ratio"
)

ts_plot(
  ts_a %>%
    select(c("date","Private.nonresidential.fixed.assets.I","Equipment.I","Structures.I")) %>%
    mutate(
      ratio=(Equipment.I+Structures.I)/Private.nonresidential.fixed.assets.I,
    ) %>%
    select(date,ratio),
  title = "Ratio of equipment and structures in private nonresidential fixed assets",
  Ytitle = "Ratio"
)

# regressions -------------------------------------------------------------
library(tsibble)
library(forecast)
library(sandwich)
library(lmtest)
library(modelsummary)

reg_ts <- ts %>%
  filter(date>="1972-01-01" & date<"2023-01-01") %>%
  arrange(date) %>%
  mutate(
    k = na.interp(log(K),linear=TRUE),
    i = na.interp(log(I),linear=TRUE),
    y = na.interp(log(tsY),linear=TRUE),
    ik = log(IK),
    yk = y-k,
    ret_rf = log_ret_excess_vw_avg,
    vol = log_ret_excess_vw_sd,
    sumret = cumsum(replace_na(ret_rf, 0)),
    sumvol = cumsum(replace_na(vol, 0)),
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
  tstat <- coeftest(fit, vcov = NeweyWest)
}

ret_ik<-list()
ret_ik_yk<-list()
vol_ik<-list()
vol_ik_yk<-list()
max_h <- 16
for (k in 1:max_h){
 ret_ik[[k]] <- pred_reg(reg_ts,c("ret_rf"),c("ik"),k)
 ret_ik_yk[[k]] <- pred_reg(reg_ts,c("ret_rf"),c("ik","yk"),k)
 vol_ik[[k]] <- pred_reg(reg_ts,c("vol"),c("ik"),k)
 vol_ik_yk[[k]] <- pred_reg(reg_ts,c("vol"),c("ik","yk"),k)
}

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


# old code snippets ----------------------------------------------------------------------
#
#
# reg_ts %>% autoplot(ik)
# reg_ts %>% autoplot(vol)
#
# reg_ts |>
#   model(TSLM(ret_rf ~ lag(ik))) |>
#   report()
#
# reg_ts |>
#   model(TSLM(vol ~ lag(ik))) |>
#   report()
#
# regh <- function(data,y,x,h) {
#   model(TSLM(y-lag(y,h) ~ lag(x,h))) |>
#   report()
# }
# regh(reg_ts,sumvol,ik,1)
#
# volreg <- vector("numeric", 60L)
# for(h in 1:60){
#   volreg[h]<- reg_ts |>
#     model(TSLM(sumvol-lag(sumvol,h) ~ lag(ik,h)))|>
#     report()
# }
#
# volreg[[1]][[1]]$fit$coefficients[[2]]
#
#
# library(sandwich)
# volreg <- lm(sumvol-lag(sumvol,2) ~ lag(ik,2),data=reg_ts)
# coeftest(volreg, vcov = NeweyWest(volreg, lag = 12, prewhite = FALSE))
#
#
# msummary(volreg, vcov = vc$Clustered, stars = TRUE)
#
# mutate(
#   date = yearquarter(date)
# ) %>%
#   as_tsibble() %>%
#
# reg_ik <- tslm(
#   ret_rf ~ ik,
#   data=reg_ts)
# summary(reg_ik)
#
# reg_vol <- lm(
#   vol ~ ik,
#   data=reg_ts)
# summary(reg_vol)
#
#
# reg_ts %>%
#   tsdisplay()
#
# reg.ts <- reg_ts %>%
#   mutate(
#     date = yearquarter(reg_ts$date)
#   ) %>%
#   as_tsibble() %>%
#   select(date,ik) %>%
#   as.ts
# ggtsdisplay(reg.ts, plot.type="scatter")
#
#
# reg_ts %>%
#   as.data.frame() %>%
#   ggplot(aes(x=ik, y=ret_rf)) +
#   ylab("Excess return of CRSP value-weighted portfolio") +
#   xlab("Log(Investment/Capital)") +
#   geom_point() +
#   geom_smooth(method="lm", se=FALSE)
#
#
#
# autoplot(uschange[,'Consumption'], series="Data") +
#   autolayer(fitted(fit.consMR), series="Fitted") +
#   xlab("Year") + ylab("") +
#   ggtitle("Percent change in US consumption expenditure") +
#   guides(colour=guide_legend(title=" "))
#
#
#
# cbind(Data = uschange[,"Consumption"],
#       Fitted = fitted(fit.consMR)) %>%
#   as.data.frame() %>%
#   ggplot(aes(x=Data, y=Fitted)) +
#   geom_point() +
#   ylab("Fitted (predicted values)") +
#   xlab("Data (actual values)") +
#   ggtitle("Percent change in US consumption expenditure") +
#   geom_abline(intercept=0, slope=1)
#
#
# ## Fama-French -------------------------------------------------------------
#
#
# ## CRSP --------------------------------------------------------------------
#
# ## Compustat ---------------------------------------------------------------
#
# ###########
# rf_daily %>%
#   mutate(rf = rf*252) %>%
#   timetk::plot_time_series(.date_var = date, .value = rf, .smooth = FALSE)
#
# rf_monthly %>%
#   mutate(rf = rf*12) %>%
#   timetk::plot_time_series(.date_var = date, .value = rf, .smooth = FALSE)
#
# crsp_compustat_agregate %>%
#   timetk::plot_time_series(.date_var = date, .value = firmIK, .smooth = FALSE)
#
# # load
# all_data <- dbConnect(
#   SQLite(),
#   "inst/extdata/all_data.sqlite",
#   extended_types = TRUE
# )
# dbListTables(all_data)
# ts <- tbl(all_data, "ts") %>%
#   collect()
#
#
#
#
# # Plots -------------------------------------------------------------------
# ts %>% select(IK, firmIK_gross, firmIK_net,date) %>%
#   pivot_longer(c(IK, firmIK_gross, firmIK_net)) %>%
#   drop_na() %>%
#   ggplot(aes(x=date,y=value,color=name))+
#   geom_path()+
#   easy_labs() +
#   ylab(NULL)+
#   xlab(NULL)+
#   theme(
#     legend.title=element_blank()) +
#   scale_color_discrete(
#     labels=c(
#       IK = TeX(r"( $I_t / K_{t-1}$  from Fixed Asset Tables)" ),
#       firmIK_gross = TeX(r"(Average $I_t / K^{gross}_{t-1}$  over Compustat firms)" ),
#       firmIK_net = TeX(r"(Average $I_t / K^{net}_{t-1}$  over Compustat firms)" )
#     )
#   )
#
#
#
#
# # BEA data ----------------------------------------------------------------
# bea_st <-  tbl(all_data, "bea") %>%
#   collect() %>%
#   as_tibble()
#
# l1<- gsub('\"', "", "$I_t$", fixed = TRUE)
# vtable::st(bea,
#            vars = c("IK","depreciation.K"),
#            labels = c(
#             "I/K",
#             "Depreciation rate"
#            ),
#            digits = 2,
#            summ=c("mean(x)","sd(x)","min(x)","max(x)","notNA(x)"),
#            summ.names = c("Mean","SD","Min","Max","N"),
#            note = paste0(
#              labelled::get_variable_labels(bea)$K," ",
#              labelled::get_variable_labels(bea)$I
#              ),
#            out="kable",
#            file = "IK.tex"
# )
#
# bea_ts <-  tbl(all_data, "bea") %>%
#   collect
#
# bea %>% select(I,K,date) %>%
#   mutate(across(!date,~ .x)) %>%
#   pivot_longer(c("I","K")) %>%
#   ggplot(aes(x=date,y=value,color=name))+geom_line()
#
#
#
# # Summary stats -----------------------------------------------------------
# library(vtable)
#
# bea <- tbl(all_data, "bea") %>%  collect()
# compustat_crsp_aggregate <- tbl(all_data, "bea") %>%  collect()
#
# vtable::st(bea_vars,
#            digits = 2,
#            summ=c("mean(x)","sd(x)","min(x)","max(x)","notNA(x)"),
#            summ.names = c("Mean","SD","Min","Max","N"),
#            # labels=c("Federal Funds Rate","log of Real GDP","log of Core PCE Deflator"),
#            title = "Title",
#            note = "\\textbf{Descriptive Statistics for BEA data:} Fixed asset tables.",
#            anchor = "sumstats_for_var",
#            file = "output/tables/bea_sumstats.tex",
#            # align = 'p{.3\\textwidth}ccccccc',
#            # fit.page = '\\textwidth',
#            # note.align = 'p{.3\\textwidth}ccccccc',
#            out = "latex"
#            # out = "viewer"
# )
#
# # Plots -------------------------------------------------------------------
#
# tbl(all_data, "crsp") |>
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
# tbl(all_data, "crsp") |>
#   left_join(tbl(all_data, "fred"), by = "month") |>
#   group_by(month, exchange) |>
#   summarize(
#     mktcap = sum(mktcap, na.rm = TRUE) / price,
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
#
#
#
#
#
#
#
# ts_a %>% select(date,IK,xs_avg_firmIK_gross,xs_avg_firmIK_net,firmIK_gross,firmIK_net) %>%
#   pivot_longer(c("IK","firmIK_gross","firmIK_net")) %>%
#   ggplot(aes(x=date,y=value,color=name))+
#   geom_line()+
#   easy_labs() +
#   ylab(NULL)+
#   xlab(NULL)+
#   theme(
#     legend.title=element_blank()) +
#   scale_color_discrete(
#     labels=c(
#       "IK",
#       "firmIK_gross",
#       "firmIK_net"
#     )
#   )
# ts_a %>% select(date,xs_avg_firmIK_gross,xs_avg_firmIK_net,firmIK_gross,firmIK_net) %>%
#   pivot_longer(c("xs_avg_firmIK_gross","xs_avg_firmIK_net")) %>%
#   ggplot(aes(x=date,y=value,color=name))+
#   geom_line()+
#   easy_labs() +
#   ylab(NULL)+
#   xlab(NULL)+
#   theme(
#     legend.title=element_blank()) +
#   scale_color_discrete(
#     labels=c(
#       "xs_avg_firmIK_gross",
#       "xs_avg_firmIK_net"
#     )
#   )
#
# plotE<-plot_time_series(
#   ts_a,
#   date,
#   FA.Equipment.K,
#   .title = "Fixed Asset Tables: Equipment",
#   .smooth = FALSE,
#   .plotly_slider = TRUE
# )
#
# plotS<-plot_time_series(
#   ts_a %>% drop_na(FA.Structures.K),
#   date,
#   FA.Structures.K,
#   .title = "Fixed Asset Tables: Structures",
#   .smooth = FALSE,
#   .plotly_slider = TRUE
# )
#
# library(patchwork)
# plotE + plotS

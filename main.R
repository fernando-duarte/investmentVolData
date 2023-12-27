library(tidyverse)
library(RSQLite)
library(scales)
library(tsibble)
library(fable)
library(timetk)
library(ggplot2)

# load
all_data <- dbConnect(
  SQLite(),
  "inst/extdata/all_data.sqlite",
  extended_types = TRUE
)
dbListTables(all_data)

# # next: add descriptions to bea tibble
# K = "Physical capital $K$ is the sum of the current-cost net
# stocks of private non-residential equipment (line 2) and structures (line 3)
# from the BEA\'s Fixed Asset Tables, Table 4.1., at annual frequency in billions
# of dollars.",
# I = "Investment $I$ is the sum of current-dollars gross private
# investment flows in non-residential equipment (line 2) and structures (line 3)
# from the BEA\'s Fixed Asset Tables, Table 4.7., at annual frequency in billions
# of dollars.",
# IK = "$I/K$"
# label_I_NIPA <- "$I_{NIPA}$ is the sum of gross private domestic
# investment in non-residential equipment (line 10) and structures (line 11)
# from NIPA Table 1.1.5. in current-dollars (billions), at quarterly frequency,
# seasonally adjusted at annual rates."
# label_firmK_net <- "$K_{compustat,net}$ is the sum of net property, plan
# and equipment (variable ppent) over all the firms in our Compustat sample."
# label_firmK_gross <- "$K_{compustat,gross}$ is the sum of gross property,
# plan and equipment (variable ppegt) over all the firms in our Compustat sample."
# label_firmI <- "$I_{compustat}$ is the sum of gross invesment over all
# firms in our Compustat sample, where gross investment is capital
# expenditures (variable capx) minus sales of property, plant and equipment (variable sppe)."
#
# label_firmIK_net <- "$I_{compustat}/K_{compustat,net}$"
# label_firmIK_gross <- "$I_{compustat}/K_{compustat,gross}$"
#




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


#' ---
#' output: github_document
#' params:
#'   x: !r transact
#' ---


#' # 12 Month OOS


opts_chunk$set(cache=FALSE)

library(forecast)
pkgload::load_all(".")

library(tidyverse)
library(tsbox)


# dta_raw <- read_csv("~/git/snb/seasadj/data/SIC_ts4XCSAX_d_extern.csv", col_types = cols())

# dta <-
#   filter(dta_raw, PaymentCategory == "all") %>%
#   transmute(time = SettlementDateSic, value = trx_count / 1e3)



# dta <-
#   filter(dta_raw, PaymentCategory == "Retail") %>%
#   transmute(time = SettlementDateSic, value = trx_CHF_sum / 1e6) %>%
#   add_effects() %>%
#   select(time, value, wday) %>%
#   ts_long()


x <- params$x

x

# #' ## seas_dummy
# z_dummy <- oos_evals(x, seas_dummy)
# kable(summary_oos_evals(z_dummy))
# plot_oos_evals(z_dummy)

#' ## seas_loess
z_loess <-  oos_evals(x, seas_loess5)
kable(summary_oos_evals(z_loess))
plot_oos_evals(z_loess)

# #' ## seas_dsa
z_dsa <-  oos_evals(x, seas_dsa)
kable(summary_oos_evals(z_dsa))
plot_oos_evals(z_dsa)

# #' ## seas_prophet
z_prophet <- oos_evals(x, seas_prophet)
kable(summary_oos_evals(z_prophet))
plot_oos_evals(z_prophet)


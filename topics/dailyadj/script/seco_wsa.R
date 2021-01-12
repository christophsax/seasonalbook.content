#' ---
#' output: github_document
#' ---

library(cowplot)
library(dailyadj)
library(tempdisagg)
library(tsbox)
library(tidyverse)

dta <- read_csv(here::here("data/seco.csv")) %>%
  select(id, time, value)


wwa <-
  dta %>%
  ts_pick("SECO-WWA unadjusted") %>%
  select(-id) %>%
  ts_regular()  # NAs for missing dates


# fill in two NAs
ts_na_interpolation <- ts_(function(x, ...) imputeTS::na_interpolation(x, ...))
wwa_interpol <- wwa %>%
  ts_na_interpolation()

wwa$value[is.na(wwa$value)] <- wwa_interpol$value[is.na(wwa$value)]


library(tempdisagg)
# 1. disaggregate to daily

# fast means faster than the other methods, but week to day is still slow.
m_daily <- td(wwa ~ 1, to = "day", method = "fast")
wwa_daily <- predict(m_daily)




# Temporary: tsbox should be able to do `ts_frequency(x, "week")
# https://github.com/christophsax/tsbox/issues/183

first_of_week <- function(time) {
  by = "7 days"
  first_day <- time[data.table::wday(time) == 1][1]
  first_day_minus_1 <- seq(first_day, length.out = 2, by = paste0("-", by))[2]
  first_days <-  seq(first_day_minus_1, time[length(time)], by = by)
  all_days <- seq(first_day_minus_1, time[length(time)], by = "day")
  time_first <- data.table::data.table(time = first_days, first = first_days)[data.table::data.table(time = all_days), on = "time", roll = TRUE]
  z <- data.table:::merge.data.table(data.table::data.table(time = time), time_first, by = "time", all.x = TRUE)$first
  stopifnot(length(z) == length(time))
  z
}

ts_frequency_week <- function(x, fun = function(e) mean(e, na.rm = TRUE)) {
  library(dplyr)
  x <- ts_default(ts_tbl(x))
  x %>%
    group_by(time = first_of_week(time)) %>%
    summarize(value = fun(value), .groups = "drop")
}

# series are the same (week starts one day off)
ts_frequency_week(ts_lag(wwa_daily, -1), sum) %>%
  mutate(time = tsbox:::time_shift(time, "1 day"))


# 2. Seasonally adjust artificial daily data
wwa_daily_sa <- seas_daily(wwa_daily, span_month = 20, span_trend = 0.4)

plot_components(wwa_daily_sa)

# 3. Aggregate back to weekly
wwa_sa <-
  wwa_daily_sa %>%
  ts_pick("adj") %>%
  select(-id) %>%
  ts_lag(-1) %>%
  ts_frequency_week(sum) %>%
  mutate(time = tsbox:::time_shift(time, "1 day"))

ts_plot(wwa, wwa_sa)







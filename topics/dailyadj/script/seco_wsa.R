#' ---
#' output: github_document
#' ---

library(cowplot)
library(dailyadj)
library(tempdisagg)
library(tsbox)
library(tidyverse)

load(here::here("data/sbb.Rdata"))



# fill in NAs
interpol_weeks <- function(x) {
  ts_na_interpolation <- ts_(function(x, ...) imputeTS::na_interpolation(x, ...))
  x_interpol <- x %>%
    ts_na_interpolation()
  x$value[is.na(x$value)] <- x_interpol$value[is.na(x$value)]
  x
}

raw <-
  sbb_sax %>%
  ts_pick("raw") %>%
  select(-id) %>%
  ts_regular() %>%
  interpol_weeks()

wsa <-
  sbb_sax %>%
  ts_pick("wsa") %>%
  select(-id) %>%
  ts_regular() %>%
  interpol_weeks()


# filter(wsa, is.na(value))
# filter(raw, is.na(value))

# library(tempdisagg)
# # 1. disaggregate to daily

# # fast means faster than the other methods, but week to day is still slow.
m_daily <- td(raw ~ 1, to = "day", method = "fast")
raw_daily <- predict(m_daily)


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
raw_alt <-
  ts_frequency_week(ts_lag(raw_daily, -1), sum) %>%
  mutate(time = tsbox:::time_shift(time, "1 day"))

# raw %ts-% raw_alt

# 2. Seasonally adjust artificial daily data
sa_daily <- seas_daily(raw_daily)

plot_components(sa_daily)

# 3. Aggregate back to weekly
sa <-
  sa_daily %>%
  ts_pick("adj") %>%
  select(-id) %>%
  # ts_pcy() %>%
  # ts_na_omit() %>%
  # ts_lag(-1) %>%
  ts_frequency_week(sum)

ts_plot(raw, sa)


ts_plot(ts_span(ts_c(sa, wsa), 2005))



# sa$value <- zoo::rollmean(sa$value, 3, align="right", fill=NA)






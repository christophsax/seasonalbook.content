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


sbb_nettotonnenkm <-
  dta %>%
  ts_pick("sbb_nettotonnenkm") %>%
  select(-id) %>%
  ts_regular()


wwa_ntkm <-
  dta %>%
  ts_pick("wwa_ntkm") %>%
  select(-id) %>%
  ts_regular()



# fill in NAs
ts_na_interpolation <- ts_(function(x, ...) imputeTS::na_interpolation(x, ...))
wwa_ntkm_interpol <- wwa_ntkm %>%
  ts_na_interpolation()
wwa_ntkm$value[is.na(wwa_ntkm$value)] <- wwa_ntkm_interpol$value[is.na(wwa_ntkm$value)]



# filter(wwa_ntkm, is.na(value))


# library(tempdisagg)
# # 1. disaggregate to daily

# # fast means faster than the other methods, but week to day is still slow.
# m_daily <- td(sbb ~ 1, to = "day", method = "fast")
# sbb_nettotonnenkm <- predict(m_daily)




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
sbb_nettotonnenkm_nsa <-
  ts_frequency_week(ts_lag(sbb_nettotonnenkm, -1), sum) %>%
  mutate(time = tsbox:::time_shift(time, "1 day"))


# 2. Seasonally adjust artificial daily data
sbb_nettotonnenkm_sa_raw <- seas_daily(sbb_nettotonnenkm)

plot_components(sbb_nettotonnenkm_sa_raw)

# 3. Aggregate back to weekly
sbb_nettotonnenkm_sa <-
  sbb_nettotonnenkm_sa_raw %>%
  ts_pick("adj") %>%
  select(-id) %>%
  # ts_pcy() %>%
  # ts_na_omit() %>%
  # ts_lag(-1) %>%
  ts_frequency_week(sum)

ts_plot(sbb_nettotonnenkm_nsa, sbb_nettotonnenkm_sa)



ts_plot(wwa_ntkm)

ts_plot(sbb_nettotonnenkm_sa)

ts_plot(sbb_nettotonnenkm_nsa)


ts_scale(ts_c(
ts_frequency(wwa_ntkm, "year"),
ts_frequency(sbb_nettotonnenkm_nsa, "year"),
ts_frequency(sbb_nettotonnenkm_sa, "year")
)) %>%
ts_plot()






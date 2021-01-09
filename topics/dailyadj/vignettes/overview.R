## ---- include = FALSE---------------------------------------------------------
library(dailyadj)
library(tsbox)
library(tidyverse)


## -----------------------------------------------------------------------------
library(dailyadj)
library(tsbox)
library(tidyverse)

casualties


## -----------------------------------------------------------------------------
ts_plot(casualties)


## ---- dummy-matrix, cache = TRUE----------------------------------------------
dums <-
  casualties %>%
  mutate(wday = lubridate::wday(time, label = TRUE)) %>%
  mutate(month = lubridate::month(time, label = TRUE)) %>%
  select(time, wday, month) %>%
  fastDummies::dummy_cols("wday", remove_selected_columns = TRUE) %>%
  fastDummies::dummy_cols("month", remove_selected_columns = TRUE) %>%
  select(-wday_Mon, -month_Jan)

dums


## ---- arimax, cache = TRUE----------------------------------------------------

fit <- auto.arima(casualties$value, seasonal = FALSE, xreg = as.matrix(dums[, -1]))
adj <- casualties
adj$value <- as.numeric(fit$fitted)

ts_plot(casualties, adj)



## ---- coeff-plots-------------------------------------------------------------

enframe(coef(fit)) %>%
  filter(grepl("wday", name)) %>%
  mutate(name = gsub("wday_", "", name)) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  ggplot(aes(x = name, y = value)) +
    geom_col() +
    ggtitle("Weekday effects", subtitle = "Baseline: Monday")


enframe(coef(fit)) %>%
  filter(grepl("month", name)) %>%
  mutate(name = gsub("month_", "", name)) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  ggplot(aes(x = name, y = value)) +
    geom_col() +
    ggtitle("Month effects", subtitle = "Baseline: January")



## ---- stl, cache = TRUE-------------------------------------------------------

casualties %>%
  seas_daily() %>%
  ts_pick("orig", "adj") %>%
  ts_plot()



## ---- dsa, cache = TRUE-------------------------------------------------------

library(dsa)
z <- dsa::dsa(ts_xts(casualties))
plot(z, dy = FALSE)



## ---- prophet, cache = TRUE---------------------------------------------------
library(prophet)

df <- rename(casualties, ds = time, y = value)
m <-
  prophet(daily.seasonality = FALSE) %>%
  add_country_holidays(country_name = 'UK') %>%
  fit.prophet(df)



## ---- prophet-components, cache = TRUE----------------------------------------
# not strictly needed, but will include forecast too
future <- make_future_dataframe(m, periods = 31)
forecast <- as_tibble(predict(m, future))

forecast %>%
  transmute(
    time = as.Date(ds),
    additive_terms,
    yhat
  ) %>%
  left_join(casualties, by = "time") %>%
  mutate(adj = value - additive_terms) %>%
  select(time, value, adj) %>%
  ts_long() %>%
  ts_plot()


## ---- ts_ts-------------------------------------------------------------------
x_ts <- ts_ts(casualties)
head(x_ts)


## ---- tbats, cache = TRUE-----------------------------------------------------

fit <- tbats(x_ts)
adj <- fit$fitted
ts_plot(casualties, adj)


## ---- echo = FALSE, messages = FALSE------------------------------------------


env <- new.env()
load(file = here::here("script/oos_eval_summary_ans.RData"),  envir = env)

library(tidyverse)


summary_per_series <- function(x) {
  lapply(x, summary_oos_evals) %>%
  bind_rows(.id = "model") %>%
  filter(period == "Mean") %>%
  select(model, mpce)
}

bind_rows(lapply(as.list(env), summary_per_series), .id = "series") %>%
  mutate(series = gsub("z_", "", series)) %>%
  pivot_wider(names_from = "series", values_from = "mpce") %>%
  kable()



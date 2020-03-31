library(tidyverse)
library(tsbox)

x <- read_csv("data/data.csv", col_types = cols())

# Baisc Stepwise adjustment ----------------------------------------------------

myloess <- function(y, ...) {
  if (length(y) < 12) {
    z <- y
    z[] <- mean(y)
    return(z)
  }
  predict(loess(y ~ seq_along(y), ...))
}

x_adj_loess <-
  x %>%
  filter(!(data.table::month(time) == 2 & data.table::mday(time) == 29)) %>%
  # removing trend
  mutate(orig = value, trend_adj = myloess(value), value = value - trend_adj)  %>%
  mutate(
    wday = data.table::wday(time),
    mday = data.table::mday(time),
    yday = data.table::yday(time),
    year = data.table::year(time)
  ) %>%
  group_by(year) %>%
  mutate(yday = seq_along(yday)) %>%
  ungroup() %>%
  group_by(wday) %>%
  # removing intra-week effect
  mutate(week_adj = myloess(value), value = value - week_adj) %>%
  ungroup() %>%
  group_by(mday) %>%
  # removing intra-month effect
  mutate(month_adj = myloess(value, span = 0.8), value = value - month_adj) %>%
  ungroup() %>%
  # group_by(yday) %>%
  # # removing intra-year effect
  # mutate(year_adj = myloess(value, span = 0.9), value = value - year_adj) %>%
  # ungroup() %>%
  # re-add trend
  mutate(final = value + trend_adj)

x_adj_loess %>%
  select(-wday, -mday, -yday, -year) %>%
  ts_long() %>%
  filter(time > "2014-06-01") %>%
  ggplot(aes(x = time, y = value)) +
  geom_line() +
  facet_wrap("id")



# Plot individual effects ------------------------------------------------------

# quite a strong change in weekly pattern
x_adj_loess %>%
  filter(time > "2014-06-01") %>%
  mutate(year_week =  paste0(lubridate::year(time), "-", lubridate::week(time))) %>%
  ggplot(aes(x = wday, y = week_adj, color = year_week)) +
  geom_line()

# would be better to look at something like SI plot? Maybe, buts let check out
# literature first!!
















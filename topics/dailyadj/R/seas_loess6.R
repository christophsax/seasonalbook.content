seas_loess6 <- function(x, h = 35, adj0 = x) {

validate_seas_input(x)

stopifnot(nrow(filter(x, is.na(value))) == 0)

x_effects <-
  x %>%
  add_days(n = h) %>%

  # only to get yday right....
  mutate(value = coalesce(value, -999)) %>%  # to identify add days :-(
  filter(!(data.table::month(time) == 2 & data.table::mday(time) == 29)) %>%   # FIXME feb 29 hack
  ts_regular() %>%
  mutate(tmp_add = is.na(value)) %>%
  mutate(value = na_if(value, -999)) %>%
  rename(orig = value) %>%
  mutate(
  wday = data.table::wday(time),
  mday = data.table::mday(time),
  yday = data.table::yday(time),
  year = data.table::year(time)
  ) %>%
  group_by(year) %>%
  mutate(yday = seq_along(yday)) %>%
  ungroup() %>%
  filter(!tmp_add) %>%
  select(-tmp_add)

# select(x_effects,time,orig,wday) %>%
#   ts_long() %>%
#   ts_dygraphs()


x_trend <-
  x_effects %>%
  left_join(rename(adj0, adj0 = value), by = "time") %>%
  # removing trend  0.15
  mutate(trend = smooth_and_forecast(adj0,span = 0.15)) %>%
  select(-adj0) %>%
  mutate(irreg = orig - trend)


x_trend_week_month <-
  x_trend %>%
    group_by(wday) %>%
    # removing intra-week effect
    mutate(seas_w = smooth_and_forecast2(irreg,span = 0.3)) %>%
    ungroup() %>%
    mutate(irreg = irreg - seas_w) %>%
    group_by(mday) %>%
    # removing intra-month effect
    mutate(seas_m = smooth_and_forecast2(irreg, span = 0.7)) %>%
    ungroup()  %>%
    # mutate(seas_m = stats::filter(seas_m, rep(1, 3))) %>%
    mutate(irreg = irreg - seas_m)

# select(x_trend_week_month,time,orig,seas_m, irreg) %>%
#   ts_long() %>%
#   ts_dygraphs()


x_trend_week_month_year <-
    x_trend_week_month %>%
    # mutate(seas_y = 0) %>%

    group_by(yday) %>%
    # removing intra-year effect
    mutate(seas_y = smooth_and_forecast2(irreg, span = 1)) %>%
    ungroup() %>%
    mutate(seas_y = smooth_and_forecast2(seas_y, span = 0.03)) %>%

    group_by(yday) %>%
    mutate(seas_y = mean(seas_y, na.rm = TRUE)) %>%
    ungroup() %>%

    mutate(irreg = irreg - seas_y) %>%
    # re-add trend
    mutate(adj = irreg + trend)


# x_trend_week_month_year %>%
#   select(time, yday, seas_y)


# select(x_trend_week_month_year,time,orig,seas_y, irreg) %>%
#   ts_long() %>%
#   ts_dygraphs()


z_wide <- x_trend_week_month_year %>%
  mutate(seas_x = 0) %>%
  select(-wday,-mday,-yday,-year) %>%
  select(time,orig,adj,everything()) %>%
  mutate(seas = seas_w + seas_m + seas_y) %>%
  # tail()
  # select(-seas_w,-seas_m,-seas_y) %>%
  mutate(irreg_fct = irreg_forecast(irreg)) %>%
  mutate(irreg = if_else(is.na(irreg),irreg_fct,irreg)) %>%
  select(-irreg_fct) %>%
  mutate(fct = trend + irreg + seas) %>%
  select(-irreg) %>%
  select(time,!! .exp_cols_comp)

# select(z_wide,time,orig,fct, irreg) %>%
#   ts_long() %>%
#   ts_dygraphs()

z <- ts_long(z_wide)



validate_seas_output(z)
}


add_days <- function(x, n = 10) {
  if (n == 0) return(x)
  last_day <- utils::tail(x$time, 1)
  future_days <- seq(last_day, length.out = ceiling(n * 1.5), by = "day")[-1]
  stopifnot(length(future_days) >= n)
  bind_rows(arrange(x, time), tibble(time = future_days[1:n], value = NA_real_))
}



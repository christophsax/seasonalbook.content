# x <- transact

#' @export
seas_daily <- function(x,
  h = 35,
  holiday_df = NULL,
  span_trend = 0.25,
  span_week = 0.3,
  span_month = 0.7,
  span_intrayear =  0.02
  ) {

  if (is.null(holiday_df)) {
    holiday_df <- bind_rows(
      mutate(holidays(), holiday = paste(holiday, "-1"), time = time - 1),
      holidays()
    )
  }

  validate_seas_input(x)
  stopifnot(nrow(filter(x, is.na(value))) == 0)

  x_effects <-
    x %>%
    add_days(n = h) %>%
    rename(orig = value) %>%
    mutate(
    wday = data.table::wday(time),
    mday = data.table::mday(time),
    yday = yday_leap(time),
    year = data.table::year(time)
    ) %>%
    group_by(year) %>%
    mutate(yday = seq_along(yday)) %>%
    ungroup()

  # select(x_effects,time,orig,wday) %>%
  #   ts_long() %>%
  #   ts_dygraphs()


  x_trend <-
    x_effects %>%
    left_join(x, by = "time") %>%
    # removing trend  0.15
    mutate(trend = smooth_and_forecast(value, span = span_trend)) %>%
    mutate(irreg = orig - trend)

  x_trend_week <-
    x_trend %>%
    group_by(wday) %>%
    # removing intra-week effect
    mutate(seas_w = smooth_and_forecast2(irreg, span = span_week)) %>%
    ungroup() %>%
    mutate(irreg = irreg - seas_w)

  # holiday adjustment (applied on weekday and trend adjusted data, as proposed
  # in bundenbank paper)
  seas_x <-
    x_trend_week %>%
    select(time, value = irreg) %>%
    seas_x(holiday_df)

  x_trend_week_x <-
    x_trend_week %>%
    left_join(seas_x, by = "time") %>%
    mutate(irreg = irreg - seas_x)

  x_trend_week_month <-
    x_trend_week_x %>%
    group_by(mday) %>%
    # removing intra-month effect
    mutate(seas_m = smooth_and_forecast2(irreg, span = span_month)) %>%
    ungroup()  %>%
    mutate(irreg = irreg - seas_m)

x_trend_week_month_year <-
    x_trend_week_month %>%
    group_by(yday) %>%
    mutate(seas_y = mean(irreg, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(seas_y = smooth_and_forecast2(seas_y, span = span_year)) %>%
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
  # mutate(seas_x = 0) %>%
  select(-wday,-mday,-yday,-year) %>%
  select(time,orig,adj,everything()) %>%
  mutate(seas = seas_w + seas_m + seas_y + seas_x) %>%
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



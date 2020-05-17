#' Seasonal adjustment,using Loess (daily or monthly)
#'
#' @param x ts-boxable time series,an object of class ts,xts,zoo,data.frame,data.table,tbl,tbl_ts,tbl_time,tis,irts or timeSeries.
#' @param h forecast horizon
#'
#' @author XCSAX
#'
#' @export
seas_loess2 <- function(x,h = 35, adj0 = x) {

validate_seas_input(x)

x_effects <-
  x %>%
  add_weekdays(n = h) %>%
  filter(!(data.table::month(time) == 2 & data.table::mday(time) == 29)) %>%   # FIXME feb 29 hack
  rename(orig = value) %>%
  mutate(
  wday = data.table::wday(time),
  mday = data.table::mday(time),
  yday = data.table::yday(time),
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
  left_join(rename(adj0, adj0 = value), by = "time") %>%
  # removing trend  0.15
  mutate(trend = smooth_and_forecast2(adj0,span = 0.05)) %>%
  select(-adj0) %>%
  mutate(irreg = orig - trend)

# select(x_trend,time,orig,trend) %>%
#   ts_long() %>%
#   ts_dygraphs()


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
    mutate(seas_y = smooth_and_forecast(irreg, span = 1)) %>%
    ungroup() %>%
    mutate(seas_y = smooth_and_forecast(seas_y, span = 0.005)) %>%
    mutate(irreg = irreg - seas_y) %>%
    # re-add trend
    mutate(adj = irreg + trend)


select(x_trend_week_month_year,time,orig,seas_y, irreg) %>%
  ts_long() %>%
  ts_dygraphs()


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


seas_loess_it <- function(x, h = 35) {
  z0 <- seas_loess2(x, h = h)
  x1 <- seas_loess2(x, h = h, adj0 = select(ts_pick(z0, "adj"), -id))
  x1
}


irreg_forecast <- function(x) {
  x_hist <- zoo::na.trim(x,sides = "right")
  x[is.na(x)] <- mean(tail(x_hist, 30))
  x
}

# # A tibble: 6 x 6
#   time        orig   adj   trend    seas     fct
#   <date>     <dbl> <dbl>   <dbl>   <dbl>   <dbl>
# 1 2015-01-28    NA    NA 118436. -19061.  99760.
# 2 2015-01-29    NA    NA 118434.  22962. 141783.
# 3 2015-01-30    NA    NA 118432.  30222. 149042.
# 4 2015-02-02    NA    NA 118429.   2597. 121416.
# 5 2015-02-03    NA    NA 118427.  31064. 149883.
# 6 2015-02-04    NA    NA 118425. -11103. 107714.


# y <- c(mdeaths,NA,NA)
# plot(y); lines(smooth_and_forecast(y,span = 0.5))
smooth_and_forecast2 <- function(y,span = 0.5) {
# use mean for short series
  if ((length(y)) < 10) {
  ans <- y
  ans[] <- mean(y,na.rm = TRUE)
  return(ans)
  }
  y_hist <- zoo::na.trim(y,sides = "right")

  h <- length(y) - length(y_hist )
  if (h > 0) {
    y_fct <- c(y_hist,forecast(ets(y_hist),h = h)$mean)
  } else {
    y_fct <- y_hist
  }

  x <- seq_along(y_fct)

  x_hist <- seq_along(y_hist)
  m <- loess(y_fct ~ x,surface = "direct",span = span)
  y_smooth <- predict(m,newdata = x)
  if (h == 0) return(y_smooth)
  y_smooth
}




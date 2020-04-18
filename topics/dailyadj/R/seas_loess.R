#' Seasonal adjustment, using Loess (daily or monthly)
#'
#' @param x ts-boxable time series, an object of class ts, xts, zoo, data.frame, data.table, tbl, tbl_ts, tbl_time, tis, irts or timeSeries.
#' @param h forecast horizon
#'
#' @author XCSAX
#'
#' @export
#' @examples
#' # compare adjustment of montly series with X13
#' library(seasonal)
#' library(tsbox)
#' library(tidyverse)
#'
#' md_x13 <- seas(mdeaths, forecast.save = "fct", x11 = "")
#'
#' md_x13_fct <- ts_bind(mdeaths, series(md_x13, "forecast.forecasts")[, 'forecast'])
#' md_x13_adj <- final(md_x13)
#'
#' seas_loess(mdeaths, h = 12) %>%
#'     select(time, orig, sadj) %>%
#'     ts_long() %>%
#'     ts_c(md_x13_adj, md_x13_fct) %>%
#'     ts_ggplot()
#'
#' # also works for longer series
#' ap_x13 <- seas(AirPassengers, forecast.save = "fct", x11 = "")
#' ap_x13_fct <- ts_bind(AirPassengers, series(ap_x13, "forecast.forecasts")[, 'forecast'])
#' ap_x13_adj <- final(ap_x13)
#'
#' seas_loess(AirPassengers, h = 12) %>%
#'     select(time, orig, sadj) %>%
#'     ts_long() %>%
#'     ts_c(ap_x13_adj, ap_x13_fct) %>%
#'     ts_ggplot()
#'
#'
#' # compare daily adjustments to montly adjustments X13
#' ca_loess <- seas_loess(casualties, h = 360)
#'
#' ca_loess_m <-
#'     ca_loess %>%
#'     ts_long() %>%
#'     ts_frequency(to = "month", aggregate = "mean", na.rm = TRUE)
#'
#' ca_m <- ts_ts(ts_frequency(casualties, to = "month", aggregate = "mean"))
#' ca_x13 <- seas(ca_m, x11 = "")
#'
#' ca_x13_fct <- ts_bind(ca_m, series(ca_x13, "forecast.forecasts")[, 'forecast'])
#' ca_x13_adj <- final(ca_x13)
#'
#' ca_loess_m %>%
#'     ts_pick(c("orig", "sadj")) %>%
#'     ts_c(ca_x13_adj, ca_x13_fct) %>%
#'     ts_ggplot()
seas_loess <- function(x, h = 25) {
    x <- ts_tbl(x)

    diff <- ts_summary(x)$diff

    if (diff %in% c("1 month", "3 month")) {
        add_fct <- add_months_qrts
        span_trend <- 2
        span_year <- 3
    } else {
        add_fct <- add_weekdays
        span_trend <- 0.5
        span_year <- 5
    }

    x_effects <-
        x %>%
        add_fct(n = h) %>%
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

    x_trend <-
        x_effects %>%
        # removing trend
        mutate(trend = smooth_and_forecast(orig, span = span_trend)) %>%
        mutate(irreg = orig - trend)


    if (diff %in% c("1 month", "3 month")) {
        x_trend_week_month <-
            x_trend %>%
            mutate(seas_w = 0) %>%
            mutate(seas_m = 0)
    } else {
        x_trend_week_month <-
            x_trend %>%
            group_by(wday) %>%
            # removing intra-week effect
            mutate(seas_w = smooth_and_forecast(irreg, span = 1.5)) %>%
            ungroup() %>%
            mutate(irreg = irreg - seas_w) %>%
            group_by(mday) %>%
            # removing intra-month effect
            mutate(seas_m = smooth_and_forecast(irreg, span = 5)) %>%
            ungroup() %>%
            mutate(irreg = irreg - seas_m)
    }

    x_trend_week_month_year <-
        x_trend_week_month %>%
        group_by(yday) %>%
        # removing intra-year effect
        mutate(seas_y = smooth_and_forecast(irreg, span = span_year)) %>%
        ungroup() %>%
        mutate(irreg = irreg - seas_y) %>%
        # re-add trend
        mutate(adj = irreg + trend)

    z_wide <- x_trend_week_month_year %>%
        select(-wday, -mday, -yday, -year) %>%
        select(time, orig, adj, everything()) %>%
        mutate(seas = seas_w + seas_m + seas_y) %>%
        select(-seas_w, -seas_m, -seas_y) %>%
        mutate(irreg_fct = smooth_and_forecast(irreg, span = 1.5)) %>%
        mutate(irreg = if_else(is.na(irreg), irreg_fct, irreg)) %>%
        select(-irreg_fct) %>%
        mutate(fct = trend + irreg + seas) %>%
        select(-irreg) %>%
        select(time, !! .exp_cols)


      z <- ts_long(z_wide)

      validate_seas_output(z)
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



# y <- c(mdeaths, NA, NA)
# plot(y); lines(smooth_and_forecast(y, span = 0.5))
smooth_and_forecast <- function(y, ...) {
    # use mean for short series
    if ((length(y)) < 10) {
        ans <- y
        ans[] <- mean(y, na.rm = TRUE)
        return(ans)
    }
    y_hist <- zoo::na.trim(y, sides = "right")
    x <- seq_along(y)
    x_hist <- seq_along(y_hist)
    m <- loess(y_hist ~ x_hist, surface = "direct", ...)
    predict(m, newdata = x)
}


add_weekdays <- function(x, n = 10) {
    last_day <- utils::tail(x$time, 1)
    future_days <- seq(last_day, length.out = ceiling(n * 1.5), by = "day")[-1]
    future_weekdays <- future_days[data.table::wday(future_days) %in% 2:6]  # only monday to friday
    # FIXME use SIC calendar
    stopifnot(length(future_weekdays) >= n)
    bind_rows(arrange(x, time), tibble(time = future_weekdays[1:n], value = NA_real_))
}

add_months_qrts <- function(x, n = 10) {
    ts_bind(x, rep(NA_real_, n))
}


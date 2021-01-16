# Daily seasonal adjustment
#
# This is intentionally kept in one file, so it can be synced via mail.


# imports ----------------------------------------------------------------------

# library(tidyverse)
# library(tsbox)
# library(cowplot)
# library(forecast)

#' @import ggplot2 cowplot dplyr tidyr tsbox
NULL


# example ----------------------------------------------------------------------

# load("data/casualties.rda")
# seas_daily(casualties)
# z <-  oos_evals(casualties, seas_daily)
# summary_oos_evals(z)
# plot_oos_evals(z)


# main function ----------------------------------------------------------------

#' @export
seas_daily <- function(x,
                       h = 35,
                       holiday_df = NULL,
                       span_scale = scale_factor(x),
                       span_trend = 0.25,
                       span_week = 0.3,
                       span_month = 0.7,
                       span_within_year = 0.02) {
  if (is.null(holiday_df)) {
    holiday_df <- bind_rows(
      mutate(holidays(), holiday = paste(holiday, "-1"), time = time - 1),
      holidays()
    )
  }

  span_trend <- span_trend / span_scale
  span_week <- span_trend / span_scale
  span_month <- span_month / span_scale
  span_within_year <- span_within_year / span_scale

  # span_within_year should not be scaled, it is independent of series length

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
    ungroup() %>%
    mutate(irreg = irreg - seas_m)

  x_trend_week_month_year <-
    x_trend_week_month %>%
    group_by(yday) %>%
    mutate(seas_y = mean(irreg, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(seas_y = smooth_and_forecast2(seas_y, span = span_within_year)) %>%
    group_by(yday) %>%
    mutate(seas_y = mean(seas_y, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(irreg = irreg - seas_y) %>%
    # re-add trend
    mutate(adj = irreg + trend)

  z_wide <- x_trend_week_month_year %>%
    # mutate(seas_x = 0) %>%
    select(-wday, -mday, -yday, -year) %>%
    select(time, orig, adj, everything()) %>%
    mutate(seas = seas_w + seas_m + seas_y + seas_x) %>%
    # tail()
    # select(-seas_w,-seas_m,-seas_y) %>%
    mutate(irreg_fct = irreg_forecast(irreg)) %>%
    mutate(irreg = if_else(is.na(irreg), irreg_fct, irreg)) %>%
    select(-irreg_fct) %>%
    mutate(fct = trend + irreg + seas) %>%
    select(-irreg) %>%
    select(time, !!.exp_cols_comp)

  z <- ts_long(z_wide)

  validate_seas_output(z)
}


# helper function --------------------------------------------------------------

seas_daily_iteration <- function(x, h = 35) {
  z0 <- seas_daily(x, h = h)
  x1 <- seas_daily(x, h = h, adj0 = select(ts_pick(z0, "adj"), -id))
  x1
}

add_days <- function(x, n = 10) {
  if (n == 0) {
    return(x)
  }
  last_day <- utils::tail(x$time, 1)
  future_days <- seq(last_day, length.out = ceiling(n * 1.5), by = "day")[-1]
  stopifnot(length(future_days) >= n)
  bind_rows(arrange(x, time), tibble(time = future_days[1:n], value = NA_real_))
}

irreg_forecast <- function(x) {
  x_hist <- zoo::na.trim(x, sides = "right")
  x[is.na(x)] <- mean(tail(x_hist, 30))
  x
}

# y <- c(mdeaths,NA,NA)
smooth_and_forecast2 <- function(y, span = 0.5) {
  # use mean for short series
  if ((length(y)) < 10) {
    ans <- y
    ans[] <- mean(y, na.rm = TRUE)
    return(ans)
  }
  y_hist <- zoo::na.trim(y, sides = "right")

  h <- length(y) - length(y_hist)
  if (h > 0) {
    y_fct <- c(y_hist, forecast(ets(y_hist), h = h)$mean)
  } else {
    y_fct <- y_hist
  }

  x <- seq_along(y_fct)

  x_hist <- seq_along(y_hist)
  m <- loess(y_fct ~ x, surface = "direct", span = span)
  y_smooth <- predict(m, newdata = x)
  if (h == 0) {
    return(y_smooth)
  }
  y_smooth
}

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
  h <- length(y) - length(y_hist)
  # message(h)
  x <- seq_along(y)
  x_hist <- seq_along(y_hist)
  m <- loess(y_hist ~ x_hist, surface = "direct", ...)
  y_smooth <- predict(m, newdata = x_hist)
  if (h == 0) {
    return(y_smooth)
  }
  y_fct <- forecast(ets(y_smooth), h = h)$mean
  c(y_smooth, as.numeric(y_fct))
}

# scale_factor(casualties)
#' @export
scale_factor <- function(x) {
  tss <- ts_summary(x)
  no_of_years <- lubridate::year(tss$end) - lubridate::year(tss$start)
  no_of_years / 10
}

# return 28 at feb 29
# yday_leap(x)
yday_leap <- function(x) {
  is_leap_year <- lubridate::leap_year(x)
  z <- lubridate::yday(x)
  affected <- is_leap_year & (z >= 60)
  z[affected] <- z[affected] - 1
  z
}


# external regressors ----------------------------------------------------------

# @param df data.frame of the form
#  A tibble: 900 x 2
#   holiday             time
#   <chr>               <date>
# 1 Neujahrestag -1     1994-12-31
# 2 Karfreitag -1       1995-04-13
# 3 Ostern -1           1995-04-15
# 4 Ostermontag -1      1995-04-16
# 5 Auffahrt -1         1995-05-24

# @example
# df <- bind_rows(
#   mutate(holidays(), holiday = paste(holiday, "-1"), time = time - 1),
#   holidays()
# )
# seas_x(transact, df)

# holiday adjustment
seas_x <- function(x, df) {
  xreg <-
    df %>%
    group_by(holiday) %>%
    summarize(genhol_daily(time)) %>%
    ungroup() %>%
    ts_wide()

  dta_m <-
    x %>%
    ts_default() %>%
    left_join(xreg, by = "time")

  # estimate a few times and only keep significant vars
  m_ols <- lm(value ~ ., data = dta_m[, -1])
  m_ols <- lm(value ~ ., data = select(dta_m[, -1], !!significant_vars(m_ols)$term, value))
  m_ols <- lm(value ~ ., data = select(dta_m[, -1], !!significant_vars(m_ols)$term, value), na.action = na.exclude)

  # prediction, based on final model
  dta_m %>%
    select(time) %>%
    mutate(seas_x = unname(predict(m_ols, newdata = dta_m[, -1])) - coef(m_ols)["(Intercept)"])
}

genhol_daily <- function(x) {
  x <- as.Date(x)
  from <- as.Date(paste0(data.table::year(x[1]), "-01-01"))
  to <- as.Date(paste0(data.table::year(rev(x)[1]), "-12-31"))
  tibble(time = seq(from, to, by = "day")) %>%
    left_join(tibble(time = x, fit = x), by = "time") %>%
    mutate(value = as.integer(!is.na(fit))) %>%
    select(-fit)
}

significant_vars <- function(x) {
  broom::tidy(x) %>%
    filter(p.value < 0.05) %>%
    select(term, estimate) %>%
    mutate(term = gsub("TRUE", "", term)) %>%
    filter(term != "(Intercept)") %>%
    mutate(term = gsub("\\`", "", term))
}

holidays <- function(country = "CH") {
  as_tibble(prophet::generated_holidays) %>%
    filter(country == !!country) %>%
    mutate(time = as.Date(as.character(ds)), .keep = "unused") %>%
    mutate(holiday = as.character(holiday)) %>%
    select(-year, -country)
}


# plots ------------------------------------------------------------------------

plot_pattern_trend <- function(z) {
  ts_pick(z, "trend") %>%
    ggplot(aes(x = time, y = value, color)) +
    geom_line() +
    theme_cowplot()
}

plot_pattern_w <- function(z) {
  ts_pick(z, "seas_w") %>%
    add_effects() %>%
    group_by(year = as.factor(year), wday) %>%
    summarize(value = mean(value)) %>%
    ungroup() %>%
    ggplot(aes(x = wday, y = value, color = year)) +
    geom_line() +
    theme_cowplot()
}

plot_pattern_m <- function(z) {
  ts_pick(z, "seas_m") %>%
    add_effects() %>%
    group_by(year = as.factor(year), mday) %>%
    summarize(value = mean(value)) %>%
    ungroup() %>%
    ggplot(aes(x = mday, y = value, color = year)) +
    geom_line() +
    theme_cowplot()
}

plot_pattern_y <- function(z) {
  ts_pick(z, "seas_y") %>%
    add_effects() %>%
    group_by(year = as.factor(year), yday) %>%
    summarize(value = mean(value)) %>%
    ungroup() %>%
    ggplot(aes(x = yday, y = value, color = year)) +
    geom_line() +
    theme_cowplot()
}

plot_pattern_x <- function(z) {
  ts_pick(z, "seas_x") %>%
    ggplot(aes(x = time, y = value, color)) +
    geom_line() +
    theme_cowplot()
}

plot_pattern_i <- function(z) {
  ts_wide(z) %>%
    transmute(time, value = orig - trend - seas) %>%
    ggplot(aes(x = time, y = value, color)) +
    geom_line() +
    theme_cowplot()
}

#' @export
seas_components <- function(z) {
  if ("series" %in% colnames(z)) {
    # still dont know how to use title in plot_grid...
    title <- unique(z$series)
    stopifnot(length(title) == 1)
    z <- select(z, -series)
  }

  plot_grid(
    plot_pattern_trend(z),
    plot_pattern_y(z),
    plot_pattern_m(z),
    plot_pattern_w(z),
    plot_pattern_x(z),
    plot_pattern_i(z),
    labels = c("T", "Y", "M", "W", "H", "I"),
    ncol = 2
  )
}

add_effects <- function(x) {
  x %>%
    mutate(
      wday = data.table::wday(time),
      mday = data.table::mday(time),
      yday = data.table::yday(time),
      year = data.table::year(time)
    )
}

#' @export
seas_dygraph <- function(z, main = deparse(substitute(z))) {
  z %>%
    ts_pick(c("adj", "orig")) %>%
    ts_dygraphs(main = main)
}


# OOS Evals --------------------------------------------------------------------

#' Evalutate OOS forecast errors over multiple periods
#'
#' @examples
#' oos_evals(casualities, seas_daily)
#' oos_evals(transact, seas_daily)
#' @export
oos_evals <- function(x, seas_fun, ...) {
  by <- "-1 month"
  # noramlize
  x <- ts_tbl(ts_default(x))

  # determine the last 12 periods
  last_period_start <- as.Date(paste(lubridate::year(max(x$time)), lubridate::month(max(x$time)), "1", sep = "-"))
  periods <- seq(tsbox:::time_shift(last_period_start, "1 month"), by = by, length.out = 12) - 1

  xs <- setNames(lapply(periods, function(period) ts_span(x, end = period)), periods)
  z <- lapply(xs, oos_eval, seas_fun = seas_fun, ...)

  bind_rows(z, .id = "period") %>%
    ts_regular()
}

#' @export
summary_oos_evals <- function(x) {
  # also allow period to be of class Date
  if (inherits(x$period, "Date")) x$period <- as.character(x$period)

  z <-
    x %>%
    pivot_wider(names_from = id, values_from = value) %>%
    mutate(diff = abs(fct - x)) %>%
    mutate(diff_sq = diff^2) %>%
    mutate(pc = diff / x) %>%
    group_by(period) %>%
    summarize(
      mrse = sqrt(mean(diff_sq, na.rm = TRUE)),
      mae = mean(diff, na.rm = TRUE),
      mpce = mean(pc, na.rm = TRUE)
    ) %>%
    ungroup()

  bind_rows(
    z,
    add_column(summarize_at(z, vars(-period), mean), period = "Mean", .before = 1)
  )
}

#' @export
plot_oos_evals <- function(x) {
  ggplot(x, aes(x = time, y = value)) +
    geom_line(aes(color = id)) +
    facet_wrap(vars(period), scales = "free_x")
}

#' Evalutate OOS forecast error
#'
#' This should work with every `seas_` function in the package. Typically, the
#' series is shortened by one month. The series is then forecasted and can be
#' compared to the actual values.
#'
#' @examples
#' oos_eval(casualities, seas_daily)
#' oos_eval(transact, seas_daily)
#' @export
oos_eval <- function(x, seas_fun, ...) {
  end_short <- tsbox:::time_shift(tsbox:::time_shift(ts_summary(x)$end, "1 day"), "-1 month") - 1

  x_short <- ts_span(x, end = end_short)

  out <- seas_fun(x_short, ...)

  out %>%
    ts_pick("fct") %>%
    ts_c(x) %>%
    ts_wide() %>%
    mutate(fct = if_else(is.na(x), NA_real_, fct)) %>%
    ts_long() %>%
    ts_span(tsbox:::time_shift(end_short, "1 day")) %>%
    ts_span(end = ts_summary(x)$end)
}

summary_oos_eval <- function(x) {
  x %>%
    ts_wide() %>%
    mutate(diff = abs(fct - x)) %>%
    mutate(diff_sq = diff^2) %>%
    mutate(pc = diff / x) %>%
    summarize(
      mrse = sqrt(mean(diff_sq, na.rm = TRUE)),
      mae = mean(diff, na.rm = TRUE),
      mpce = mean(pc, na.rm = TRUE)
    )
}

oos_summary <- function(x) {
  .Defunct("summary_oos_eval")
  summary_oos_eval(x)
}


# seas_fun ---------------------------------------------------------------------

# Principles of seas_fun:
# - x, a single daily times, as a data.frame
# - output: z

.exp_cols <- c("trend", "seas", "fct", "orig", "adj")
.exp_cols_comp <- c("trend", "seas", "fct", "orig", "adj", "seas_w", "seas_m", "seas_y", "seas_x")
.extra_cols <- setdiff(.exp_cols_comp, .exp_cols)

validate_seas_input <- function(x) {
  stopifnot(inherits(x, "data.frame"))
  stopifnot(identical(colnames(x), c("time", "value")))
  stopifnot(tsbox::ts_boxable(x))
  stopifnot(tsbox::ts_summary(x)$diff == "1 day")
  x
}

validate_seas_output <- function(z) {
  stopifnot(inherits(z, "data.frame"))
  stopifnot(identical(colnames(z), c("id", "time", "value")))
  stopifnot(tsbox::ts_boxable(z))
  stopifnot(all(tsbox::ts_summary(z)$diff == "1 day"))

  if (!all(.extra_cols %in% unique(z$id))) {
    message("adding extra cols")
    z <-
      z %>%
      ts_wide() %>%
      mutate(
        seas_w = 0,
        seas_m = 0,
        seas_y = 0,
        seas_x = seas
      ) %>%
      ts_long()
  }

  stopifnot(length(setdiff(.exp_cols, unique(z$id))) == 0)
  z
}

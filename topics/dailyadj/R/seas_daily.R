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


# seas_fun ---------------------------------------------------------------------

holidays_ch <-structure(list(holiday = c("Neujahrestag", "Karfreitag", "Ostern",
"Ostermontag", "Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten", "Neujahrestag", "Karfreitag", "Ostern", "Ostermontag",
"Auffahrt", "Pfingsten", "Pfingstmontag", "Nationalfeiertag",
"Weihnachten"), time = structure(c(9131, 9234, 9236, 9237, 9275,
9285, 9286, 9343, 9489, 9496, 9591, 9593, 9594, 9632, 9642, 9643,
9709, 9855, 9862, 9948, 9950, 9951, 9989, 9999, 10000, 10074,
10220, 10227, 10326, 10328, 10329, 10367, 10377, 10378, 10439,
10585, 10592, 10683, 10685, 10686, 10724, 10734, 10735, 10804,
10950, 10957, 11068, 11070, 11071, 11109, 11119, 11120, 11170,
11316, 11323, 11425, 11427, 11428, 11466, 11476, 11477, 11535,
11681, 11688, 11775, 11777, 11778, 11816, 11826, 11827, 11900,
12046, 12053, 12160, 12162, 12163, 12201, 12211, 12212, 12265,
12411, 12418, 12517, 12519, 12520, 12558, 12568, 12569, 12631,
12777, 12784, 12867, 12869, 12870, 12908, 12918, 12919, 12996,
13142, 13149, 13252, 13254, 13255, 13293, 13303, 13304, 13361,
13507, 13514, 13609, 13611, 13612, 13650, 13660, 13661, 13726,
13872, 13879, 13959, 13961, 13962, 14000, 14010, 14011, 14092,
14238, 14245, 14344, 14346, 14347, 14385, 14395, 14396, 14457,
14603, 14610, 14701, 14703, 14704, 14742, 14752, 14753, 14822,
14968, 14975, 15086, 15088, 15089, 15127, 15137, 15138, 15187,
15333, 15340, 15436, 15438, 15439, 15477, 15487, 15488, 15553,
15699, 15706, 15793, 15795, 15796, 15834, 15844, 15845, 15918,
16064, 16071, 16178, 16180, 16181, 16219, 16229, 16230, 16283,
16429, 16436, 16528, 16530, 16531, 16569, 16579, 16580, 16648,
16794, 16801, 16885, 16887, 16888, 16926, 16936, 16937, 17014,
17160, 17167, 17270, 17272, 17273, 17311, 17321, 17322, 17379,
17525, 17532, 17620, 17622, 17623, 17661, 17671, 17672, 17744,
17890, 17897, 18005, 18007, 18008, 18046, 18056, 18057, 18109,
18255, 18262, 18362, 18364, 18365, 18403, 18413, 18414, 18475,
18621, 18628, 18719, 18721, 18722, 18760, 18770, 18771, 18840,
18986, 18993, 19097, 19099, 19100, 19138, 19148, 19149, 19205,
19351, 19358, 19454, 19456, 19457, 19495, 19505, 19506, 19570,
19716, 19723, 19811, 19813, 19814, 19852, 19862, 19863, 19936,
20082, 20089, 20196, 20198, 20199, 20237, 20247, 20248, 20301,
20447, 20454, 20546, 20548, 20549, 20587, 20597, 20598, 20666,
20812, 20819, 20903, 20905, 20906, 20944, 20954, 20955, 21031,
21177, 21184, 21288, 21290, 21291, 21329, 21339, 21340, 21397,
21543, 21550, 21638, 21640, 21641, 21679, 21689, 21690, 21762,
21908, 21915, 22023, 22025, 22026, 22064, 22074, 22075, 22127,
22273, 22280, 22380, 22382, 22383, 22421, 22431, 22432, 22492,
22638, 22645, 22730, 22732, 22733, 22771, 22781, 22782, 22858,
23004, 23011, 23115, 23117, 23118, 23156, 23166, 23167, 23223,
23369, 23376, 23472, 23474, 23475, 23513, 23523, 23524, 23588,
23734, 23741, 23822, 23824, 23825, 23863, 23873, 23874, 23953,
24099, 24106, 24207, 24209, 24210, 24248, 24258, 24259, 24319,
24465, 24472, 24564, 24566, 24567, 24605, 24615, 24616, 24684,
24830, 24837, 24949, 24951, 24952, 24990, 25000, 25001, 25049,
25195, 25202, 25299, 25301, 25302, 25340, 25350, 25351, 25414,
25560, 25567, 25656, 25658, 25659, 25697, 25707, 25708, 25780,
25926, 25933, 26041, 26043, 26044, 26082, 26092, 26093, 26145,
26291, 26298, 26391, 26393, 26394, 26432, 26442, 26443, 26510,
26656, 26663, 26748, 26750, 26751, 26789, 26799, 26800, 26875,
27021, 27028, 27133, 27135, 27136, 27174, 27184, 27185, 27241,
27387), class = "Date")), row.names = c(NA, -450L), class = c("tbl_df",
"tbl", "data.frame"))

holidays <- function(country = "CH") {
  holidays_ch

  # as_tibble(prophet::generated_holidays) %>%
  #   filter(country == !!country) %>%
  #   mutate(time = as.Date(as.character(ds)), .keep = "unused") %>%
  #   mutate(holiday = as.character(holiday)) %>%
  #   select(-year, -country)
}


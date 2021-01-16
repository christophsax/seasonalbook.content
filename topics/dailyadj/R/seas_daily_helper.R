

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

# y <- c(mdeaths,NA,NA)
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
    h <- length(y) - length(y_hist )
    # message(h)
    x <- seq_along(y)
    x_hist <- seq_along(y_hist)
    m <- loess(y_hist ~ x_hist, surface = "direct", ...)
    y_smooth <- predict(m, newdata = x_hist)
    if (h == 0) return(y_smooth)
    y_fct <- forecast(ets(y_smooth), h = h)$mean
    c(y_smooth, as.numeric(y_fct))
}


add_weekdays <- function(x, n = 10) {
    if (n == 0) return(x)
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



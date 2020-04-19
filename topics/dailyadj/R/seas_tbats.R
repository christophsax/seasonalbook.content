#' Seasonal adjustment stlf
#'
#' @export
seas_tbats <- function(x, h = 30) {

  validate_seas_input(x)

  x_ts <- ts_ts(x)


  library(tsbox)
  # make `imputeTS::na_interpolation` ts_boxable
  ts_na_interpolation <- ts_(function(x, ...) imputeTS::na_interpolation(x, ...))


  fit <- tbats(ts_na_interpolation(x_ts))
  mfct <- forecast(fit)

  adj <- fit$fitted
  fct <- ts_tbl(mfct$mean)

  z_wide <-
    ts_c(orig = x, fct, adj) %>%
    ts_wide() %>%
    mutate(seas = orig - adj, trend = NA_real_) %>%
    select(time, !! .exp_cols)

  z <- ts_long(z_wide)

  validate_seas_output(z)
}

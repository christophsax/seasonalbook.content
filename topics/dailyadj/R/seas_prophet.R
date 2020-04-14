#' Prophet
#'
#' Seasonal adjustment of daily time series, using prophet
#'
#' @exports
#' @examples seas_prophet(transact)
seas_prophet <- function(x) {

  library(prophet)

  x <- validate_seas_input(x)

  df <- rename(x, ds = time, y = value)

  m <-
    prophet(daily.seasonality = FALSE) %>%
    # including swiss holidays, which seems to have no effect
    add_country_holidays(country_name = 'CH') %>%
    fit.prophet(df)

  future <- make_future_dataframe(m, periods = 31)

  forecast <- as_tibble(predict(m, future))
  # prophet_plot_components(m, forecast)

  z_wide <-
    forecast %>%
    transmute(time = as.Date(ds), trend, seas = additive_terms, fct = yhat) %>%    # additive_terms = yhat - trend,
    left_join(x, by = "time") %>%
    rename(orig = value) %>%
    mutate(adj = orig - seas)

  # z_wide %>%
  #   select(time, yhat, orig) %>%
  #   ts_long() %>%
  #   ts_dygraphs()

  z <- ts_long(z_wide)

  validate_seas_output(z)

}

#' Daily Seasonal Adjustment, Using prophet
#'
#' Seasonal adjustment of daily time series, using the prophet package from Facebook.
#' The functions uses the same syntax as `seas_daily()` but does not support multiple time series.
#'
#' @export
#' @examples
#'
#' data(casualties)
#' ans <- seas_prophet(casualties)
#' seas_components(ans)
#'
#'
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
    transmute(
      time = as.Date(ds),
      trend,
      seas = additive_terms,
      seas_x = holidays,
      seas_y = yearly,
      seas_m = 0,
      seas_w = weekly,
      fct = yhat
    ) %>%    # additive_terms = yhat - trend,
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

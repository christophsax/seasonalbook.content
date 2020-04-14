seas_prophet <- function(data) {


  df <- rename(data, ds = time, y = value)

  m <-
    # prophet(holidays = holidays, daily.seasonality = FALSE) %>%
    prophet(daily.seasonality = FALSE) %>%
    add_country_holidays(country_name = 'CH') %>%
    fit.prophet(df)

  # forecast <- predict(m, df)
  # prophet_plot_components(m, forecast)

  z <- predict(m)

  sa <-
    z %>%
    transmute(time = as.Date(ds), trend, seas_comp = additive_terms) %>%    # additive_terms = yhat - trend,
    left_join(data, by = "time") %>%
    rename(orig = value) %>%
    mutate(seas_adj = orig - seas_comp) %>%
    ts_long()

}

#' Seasonal adjustment
#'
#' @export
#' @examples
#' # oos_eval(casualities, seas_dummy)
#' # oos_eval(transact, seas_dummy)
#' # oos_evals(casualities, seas_dummy)
#' # oos_evals(transact, seas_dummy)
seas_dummy <- function(x, h = 30) {

  validate_seas_input(x)

  library(tsbox)

  dums <-
    timmermans_dummies(x, h = 0) %>%
    ts_wide() %>%
    select(-time) %>%
    fastDummies::dummy_cols(remove_first_dummy = TRUE, remove_selected_columns = T)  %>%
    as.matrix()


  td_extra <-
    timmermans_dummies(x, h = h) %>%
    ts_span(start = tsbox:::time_shift(ts_summary(x)$end, "1 day")) %>%
    ts_wide()

  dums_extra <-
    td_extra %>%
    select(-time) %>%
    fastDummies::dummy_cols(remove_first_dummy = TRUE, remove_selected_columns = T)  %>%
    as.matrix()

  y <- x$value

  stopifnot(identical(colnames(dums), colnames(dums_extra)))

  stopifnot(NROW(y) == NROW(dums))

  fit <- auto.arima(y, stationary = TRUE, seasonal = FALSE, xreg = dums)
  mfct <- forecast(fit, xreg = dums_extra)

  adj <- x
  adj$value <- as.numeric(fit$fitted)

  fct <- td_extra %>%
    select(time, value = wday)
  fct$value <-  as.numeric(mfct$mean)

  z_wide <-
    ts_c(orig = x, fct, adj) %>%
    ts_regular() %>%
    ts_wide() %>%
    mutate(seas = orig - adj, trend = NA_real_) %>%
    select(time, !! .exp_cols)

  z <- ts_long(z_wide)

  validate_seas_output(z)
}




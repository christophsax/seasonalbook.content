#' Seasonal adjustment stlf
#'
#' @export
seas_stlf <- function(x, h = 30) {

    validate_seas_input(x)


  m <- stlf(ts_ts(x))
  fct <- ts_tbl(m$mean)
  adj <- m$fitted

  z_wide <-
    ts_c(orig = x, fct, adj) %>%
    ts_wide() %>%
    mutate(seas = orig - adj, trend = NA_real_) %>%
    select(time, !! .exp_cols)

    z <- ts_long(z_wide)

    validate_seas_output(z)
}

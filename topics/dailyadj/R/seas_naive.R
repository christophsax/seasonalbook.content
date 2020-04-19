#' Seasonal adjustment
#'
#' @export
seas_naive <- function(x, h = 30) {

  validate_seas_input(x)

  fct <- x %>%
    add_weekdays(n = h) %>%
    fill(value)


  z_wide <-
    ts_c(orig = x, fct, adj = fct) %>%
    ts_wide() %>%
    mutate(seas = orig - adj, trend = NA_real_) %>%
    select(time, !! .exp_cols)

  z <- ts_long(z_wide)

  validate_seas_output(z)
}

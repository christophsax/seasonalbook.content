#' Seasonal adjustment dsa
#'
#' Daily Seasonal Adjustment, using the *dsa* package from Bundesbank.
#'
#' https://cran.r-project.org/web/packages/dsa/vignettes/dsa-vignette.html
#'
#' @param x ts-boxable time series, an object of class ts, xts, zoo, data.frame, data.table, tbl, tbl_ts, tbl_time, tis, irts or timeSeries.
#' @param h forecast horizon.
#'
#' @author XCSAX
#'
#' @export
#' @examples
#' if (interactive()) {
#'   sadj_dsa(casualties)
#' }
seas_dsa <- function(x, h = 30) {

    validate_seas_input(x)

    res <- dsa::dsa(ts_xts(x))

    z_wide <-
      res$output %>%
      ts_tbl() %>%
      ts_span(end = ts_summary(x)$end + h) %>%
      ts_c(x) %>%
      ts_wide() %>%
      transmute(
        time,
        orig = x,
        fct = original,
        adj = seas_adj,
        trend,
        # irreg = fct - trend - sc_fac,
        seas = sc_fac
      ) %>%
      select(time, !! .exp_cols)

    z <- ts_long(z_wide)

    validate_seas_output(z)
}

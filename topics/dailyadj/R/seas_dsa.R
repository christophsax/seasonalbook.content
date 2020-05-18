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
#'   x <- transact
#' }
seas_dsa <- function(x, h = 40) {

    validate_seas_input(x)

    res <- dsa::dsa(ts_xts(x))

    effects <- dsa_seas_effects(res)


    z_wide <-
      res$output %>%
      ts_tbl() %>%
      ts_span(end = ts_summary(x)$end + h) %>%
      ts_c(x) %>%
      ts_c(effects) %>%
      ts_wide() %>%
      transmute(
        time,
        orig = x,
        fct = original,
        adj = seas_adj,
        trend,
        # irreg = fct - trend - sc_fac,
        seas = sc_fac,
        seas_x = 0,
        seas_w,
        seas_m,
        seas_y
      ) %>%
      # mutate(fct = if_else(is.na(orig), NA_real_, fct)) %>%
      # mutate(adj = if_else(is.na(orig), NA_real_, adj)) %>%
      # mutate(seas = if_else(is.na(orig), NA_real_, seas)) %>%
      # mutate(trend = if_else(is.na(orig), NA_real_, trend)) %>%
      select(time, !! .exp_cols_comp)

    z <- ts_na_omit(ts_long(z_wide))

    validate_seas_output(z)
}

#' Seasonal adjustment stlf
#'
#' @export
seas_harmon <- function(x, h = 30) {

    validate_seas_input(x)

    x_ts <- ts_ts(x)


    bestfit <- list(aicc=Inf)
    # for(K in seq(25)) {
    #   message(".", appendLF = FALSE)
    #   fit <- auto.arima(x_ts, xreg=fourier(x_ts, K=K),
    #     seasonal=FALSE)
    #   if(fit[["aicc"]] < bestfit[["aicc"]]) {
    #     bestfit <- fit
    #     bestK <- K
    #   }
    # }

    # plug in solution
    for(K in 24) {
      message(".", appendLF = FALSE)
      fit <- auto.arima(x_ts, xreg=fourier(x_ts, K=K),
        seasonal=FALSE)
      if(fit[["aicc"]] < bestfit[["aicc"]]) {
        bestfit <- fit
        bestK <- K
      }
    }

    adj <- bestfit$fitted

    mfct <- forecast(bestfit,
      xreg=fourier(x_ts, K=bestK, h=104))

    fct <- ts_tbl(mfct$mean)

  z_wide <-
    ts_c(orig = x, fct, adj) %>%
    ts_wide() %>%
    mutate(seas = orig - adj, trend = NA_real_) %>%
    select(time, !! .exp_cols)

    z <- ts_long(z_wide)

    validate_seas_output(z)
}

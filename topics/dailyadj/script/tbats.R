#' ---
#' output: github_document
#' ---



# tbats ------------------------------------------------------------------------


library(forecast)

pkgload::load_all(".")

x <- transact

# stlf -------------------------------------------------------------------------

# not bad
ans_stlf <- oos_eval(x, seas_stlf)
ts_plot(ans_stlf)
oos_summary(ans_stlf)


# harmon ------------------------------------------------------------------------

# https://otexts.com/fpp2/weekly.html
ans_harmon <- oos_eval(x, seas_harmon)
ts_plot(ans_harmon)
oos_summary(ans_harmon)


# tbats ------------------------------------------------------------------------

# https://otexts.com/fpp2/weekly.html
ans_tbats <- oos_eval(x, seas_tbats)
ts_plot(ans_tbats)
oos_summary(ans_tbats)


# ets ------------------------------------------------------------------------

# https://otexts.com/fpp2/weekly.html
ans_ets <- oos_eval(x, seas_ets)
ts_plot(ans_ets)
oos_summary(ans_ets)


# auto.arima ------------------------------------------------------------------------

# https://otexts.com/fpp2/weekly.html
ans_auto.arima <- oos_eval(x, seas_auto.arima)
ts_plot(ans_auto.arima)
oos_summary(ans_auto.arima)








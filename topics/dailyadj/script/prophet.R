#' ---
#' output: github_document
#' ---


pkgload::load_all(".")

#' ## Basics or prophet

# basic example
out_pr <- seas_prophet(transact)

# we don't get the end of month effects
out_pr %>%
  ts_pick("orig", "fct") %>%
  ts_span(2013) %>%
  ts_plot()


#' ## Basic OSS


x <- transact

z <- seas_prophet(transact)

seas_fun <- seas_prophet
ans <- oos_eval(x, seas_fun)

ts_plot(ans)
oos_summary(ans)


ans_prophet <- oos_eval(x, seas_prophet)
ts_plot(ans_prophet)
oos_summary(ans_prophet)

#' fixed seasonality' does not work well. Applys some pattern that is not really
#' there but misses that pattern that is present.

ans_loess <- oos_eval(x, seas_loess)
ts_plot(ans_loess)
oos_summary(ans_loess)


#' Too much noise, probably because of the way 'yearly effects' are calculated
#' for each days. But it seems we get the december pattern.




ans_dsa <- oos_eval(x, seas_dsa)
ts_plot(ans_dsa)
oos_summary(ans_dsa)

#' Best: gets the december pattern, and does not introduce additional noise.


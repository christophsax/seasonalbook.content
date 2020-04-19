#' ---
#' output: github_document
#' ---



library(forecast)

pkgload::load_all(".")

ans_dummy <- oos_eval(x,seas_dummy, by = "-1 month")
ts_ggplot(ans_dummy)
oos_summary(ans_dummy)


z <- oos_evals(x,seas_dummy)
plot_oos_evals(z)







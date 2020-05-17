#' ---
#' output: github_document
#' ---


#' # 12 Month OOS


opts_chunk$set(cache=TRUE)

library(forecast)
pkgload::load_all(".")


x <- transact

#' ## seas_dummy
z <- oos_evals(x, seas_dummy)
summary_oos_evals(z)
plot_oos_evals(z)


#' ## seas_loess
z <-  oos_evals(x, seas_loess5)
summary_oos_evals(z)
plot_oos_evals(z)

#' ## seas_dsa
z <-  oos_evals(x, seas_dsa)
summary_oos_evals(z)
plot_oos_evals(z)

#' ## seas_prophet
z <- oos_evals(x, seas_prophet)
summary_oos_evals(z)
plot_oos_evals(z)

#' ## seas_stlf
z <- oos_evals(x, seas_stlf)
summary_oos_evals(z)
plot_oos_evals(z)

#' ## seas_harmon
z <- oos_evals(x, seas_harmon)
summary_oos_evals(z)
plot_oos_evals(z)

#' ## seas_naive
z <- oos_evals(x, seas_naive)
summary_oos_evals(z)
plot_oos_evals(z)



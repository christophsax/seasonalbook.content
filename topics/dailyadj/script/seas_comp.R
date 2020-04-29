#' ---
#' output: github_document
#' ---


#' # Components of seasonal adjustment


opts_chunk$set(cache=TRUE)

library(forecast)
pkgload::load_all(".")


x <- transact

z_loess <- seas_loess(x)
comp_plot_draft(z_loess)

z_dsa <- seas_dsa(x)
comp_plot_draft(z_loess)


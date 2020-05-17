#' ---
#' output: github_document
#' ---


#' # Components of seasonal adjustment


opts_chunk$set(cache=TRUE)

library(forecast)
pkgload::load_all(".")


x <- transact

z <- seas_loess2(x)
comp_plot_draft(z)


res <- dsa::dsa(ts_xts(x))
output(res)


# z_dsa <- seas_dsa(x)
# comp_plot_draft(z_loess)
output(res)

library(dsa)
m <- dsa::dsa(ts_xts(x))

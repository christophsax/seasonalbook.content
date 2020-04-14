pkgload::load_all(".")


# prophet ----------------------------------------------------------------------

out_pr <- seas_prophet(transact)


# we don't get the end of month effects
out_pr %>%
  ts_pick("orig", "fct") %>%
  ts_span(2013) %>%
  ts_plot()



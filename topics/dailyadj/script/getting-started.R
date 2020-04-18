pkgload::load_all(".")


# prophet ----------------------------------------------------------------------

out_pr <- seas_prophet(transact)


# we don't get the end of month effects
out_pr %>%
  ts_pick("orig", "fct") %>%
  ts_span(2013) %>%
  ts_plot()


# oos --------------------------------------------------------------------------

x <- transact

z <- seas_prophet(transact)

seas_fun <- seas_prophet
ans <- oos_eval(x, seas_fun)

ts_plot(ans)
oos_summary(ans)


ans_prophet <- oos_eval(x, seas_prophet)
ts_plot(ans_prophet)
oos_summary(ans_prophet)
#      mae   mrse  mpce
#    <dbl>  <dbl> <dbl>
# 1 16452. 21599. 0.113

ans_loess <- oos_eval(x, seas_loess)
ts_plot(ans_loess)
oos_summary(ans_loess)
#      mae   mrse  mpce
#    <dbl>  <dbl> <dbl>
# 1 21192. 26283. 0.155

ans_dsa <- oos_eval(x, seas_dsa)
ts_plot(ans_dsa)
oos_summary(ans_dsa)
#      mae   mrse   mpce
#    <dbl>  <dbl>  <dbl>
# 1 14222. 17639. 0.0995


# dsa is clear winner













# More example series from trendecon
trendecon <-
  read_csv("https://raw.githubusercontent.com/trendecon/data/master/daily/trendecon_keywords.csv", col_types = cols()) %>%
  filter(id == "orig") %>%
  select(-id)


ans <- oos_eval(ts_tbl(ts_ts(ts_pick(trendecon, "Wirtschaftskrise"))), seas_fun)
ts_plot(ans)





# dsa --------------------------------------------------------------------------

x = transact
res <- dsa(x, cval=7, model=c(3,1,0),fourier_number = 13, reg.create=NULL)

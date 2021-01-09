library(forecast)
pkgload::load_all(".")


oos_evals_all <- function(x) {
  list(
    dummy = oos_evals(x, seas_dummy),
    daily =  oos_evals(x, seas_daily),
    dsa =  oos_evals(x, seas_dsa),
    prophet =  oos_evals(x, seas_prophet),
    harmon =  oos_evals(x, seas_harmon),
    tbats =  oos_evals(x, seas_tbats),
    naive =  oos_evals(x, seas_naive)
  )
}

# intra year start does not work (yet)
nzimmigration_arr <- ts_span(select(ts_pick(nzimmigration, "arr"), -id), 1998)
nzimmigration_dep <- ts_span(select(ts_pick(nzimmigration, "dep"), -id), 1998)

z_nzimmigration_arr <- oos_evals_all(nzimmigration_arr)
z_nzimmigration_dep <- oos_evals_all(nzimmigration_dep)

z_casualties <- oos_evals_all(casualties)
z_transact <- oos_evals_all(transact)

save(z_casualties, z_transact, z_nzimmigration_arr, z_nzimmigration_dep, file = "script/oos_eval_summary_ans.RData")



oo <- oos_evals(nzimmigration_dep, seas_daily)
summary_oos_evals(oo)


oo1 <- oos_evals(nzimmigration_dep, seas_naive)
summary_oos_evals(oo1)

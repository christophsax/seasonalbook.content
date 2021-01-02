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



z_casualties <- oos_evals_all(casualties)
z_transact <- oos_evals_all(transact)

save(z_casualties, z_transact, file = "script/oos_eval_summary_ans.RData")


# Perform OOS evals for slow methods. Store in script/data

pkgload::load_all(".")


library(tidyverse)

dta <- lst(
  transact,
  transact_inter,
  transact_retail,

  count,
  count_inter,
  count_retail,

  trendecon,

  casualties
)


# apply to all series

# oos_daily <- bind_rows(lapply(dta, oos_evals, seas_fun = seas_daily), .id = "series")
# write_csv(oos_daily, "script/data/oos_daily.csv")

# oos_prophet <- bind_rows(lapply(dta, oos_evals, seas_fun = seas_prophet), .id = "series")
# write_csv(oos_prophet, "script/data/oos_prophet.csv")

# oos_dsa <- bind_rows(lapply(dta, oos_evals, seas_fun = seas_dsa), .id = "series")
# write_csv(oos_dsa, "script/data/oos_dsa.csv")


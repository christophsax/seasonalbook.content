# OOS experiments

pkgload::load_all(".")


library(tidyverse)

dta <- lst(

  casualties,

  count,
  count_inter,
  count_retail,

  transact,
  transact_inter,
  transact_retail,

  trendecon

)

# sort(names(dta))


oos_prophet <- read_csv("script/data/oos_prophet.csv", col_types = cols())
ans_prophet <- read_csv("script/data/ans_prophet.csv", col_types = cols())

oos_daily <- read_csv("script/data/oos_daily.csv", col_types = cols())
ans_daily <- read_csv("script/data/ans_daily.csv", col_types = cols())

oos_dsa <- read_csv("script/data/oos_dsa.csv", col_types = cols())
ans_dsa <- read_csv("script/data/ans_dsa.csv", col_types = cols())


# what can we look at

# - monthly OOS
plot_oos_evals(filter(oos_dsa, series == "casualties"))
plot_oos_evals(filter(oos_prophet, series == "casualties"))
plot_oos_evals(filter(oos_daily, series == "casualties"))

# - OOS summary stats
summary_oos_evals(filter(oos_dsa, series == "casualties"))
summary_oos_evals(filter(oos_prophet, series == "casualties"))
summary_oos_evals(filter(oos_daily, series == "casualties"))

# - look at components
plot_components(filter(ans_dsa, series == "casualties"))
plot_components(filter(ans_daily, series == "casualties"))
plot_components(filter(ans_dsa, series == "casualties"))



# Model tweak on casualties


ans <- seas_daily(
  casualties,
  span_trend = 0.25,
  span_week = 0.3,
  span_month = 2,
  span_intrayear =  0.02,
)
plot_components(ans)

oos <- oos_evals(casualties, seas_daily,
  span_trend = 0.25,
  span_week = 0.3,
  span_month = 2,
  span_intrayear =  0.02,
)
summary_oos_evals(oos)
plot_components(oos)










# OOS experiments

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





oos_prophet <- read_csv("script/data/oos_prophet.csv", col_types = cols())


oos_daily <- read_csv("script/data/oos_daily.csv", col_types = cols())


# what can we look at

ll <- group_split(oos_daily, series, .keep = F)

# - monthly OOS
plot_oos_evals(ll[[1]])

# - OOS summary stats
summary_oos_evals(ll[[1]])


# - look at components
plot_components(ll[[1]])



summary_oos_evals(z)
plot_oos_evals(z)







summary_oos_evals(z)
plot_oos_evals(z)


ll[[1]]

z

ll[[1]]




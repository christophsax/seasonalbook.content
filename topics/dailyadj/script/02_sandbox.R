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



# Tweak Parameters: Causalities ------------------------------------------------

ans <- seas_daily(
  casualties,
  span_trend = 0.4,
  span_week = 0.3,
  span_month = 2,
  span_within_year =  0.008
)
plot_components(ans)

oos <- oos_evals(casualties, seas_daily,
  span_trend = 0.4,
  span_week = 0.3,
  span_month = 2,
  span_within_year =  0.008
)
summary_oos_evals(oos)
plot_oos_evals(oos)


# discussion

#  - span_within_year should be relatively small (0.8% -> 0.8% of obs of the series are used)
#  - trend a bit wider make sure we do not follow any seasonal pattern. The wider the trend, the lower the need for an iterative procedure
#  - these values could be probably used as a default


# Size adjusmtent --------------------------------------------------------------

# we now do a size adjustment for series length. This seems to work well.

ans_short <- seas_daily(
  ts_span(casualties, 2010),
  span_trend = 0.4,
  span_week = 0.3,
  span_month = 2,
  span_within_year =  0.008
)
plot_components(ans_short)
plot_components(ans)


# How do the tweaked parameters work for other series --------------------------
















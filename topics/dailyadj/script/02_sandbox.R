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


seas_daily_tweaked <- function(x) {
  seas_daily(
    x,
    span_trend = 0.4,
    span_week = 0.3,
    span_month = 2,
    span_within_year =  0.008
  )
}

# oos_daily <- lapply(dta, function(x) oos_evals(x, seas_daily))
oos_daily_tweaked <- lapply(dta, function(x) oos_evals(x, seas_daily_tweaked))


oos_overview <- function(ll) {
  lapply(ll, summary_oos_evals) %>%
    bind_rows(.id = "series") %>%
    filter(period == "Mean")
}

# oos_overview(oos_daily)
oos_overview(oos_daily_tweaked)


# overview of model performance

oos_daily_orig <- read_csv("script/data/oos_daily.csv", col_types = cols())
oos_dsa <- read_csv("script/data/oos_dsa.csv", col_types = cols())
oos_prophet <- read_csv("script/data/oos_prophet.csv", col_types = cols())
oos_naive <- read_csv("script/data/oos_naive.csv", col_types = cols())

overview <-
  list(
    # daily = oos_overview(oos_daily),
    daily_tweaked = oos_overview(oos_daily_tweaked),
    daily_orig = oos_overview(split(oos_daily_orig, oos_daily_orig$series)),
    prophet = oos_overview(split(oos_prophet, oos_prophet$series)),
    dsa = oos_overview(split(oos_dsa, oos_dsa$series)),
    naive = oos_overview(split(oos_naive, oos_naive$series))
  ) %>%
  bind_rows(.id = "name") %>%
  select(name, series, value = mpce) %>%
  pivot_wider()


# # A tibble: 8 x 5
#   series          daily_tweaked daily_orig prophet     dsa
#   <chr>                   <dbl>      <dbl>   <dbl>   <dbl>
# 1 transact                0.116      0.111  0.117   0.102
# 2 transact_inter          0.118      0.115  0.120   0.104
# 3 transact_retail         0.171      0.174  0.224   0.192
# 4 count                   0.224      0.222  0.506   0.757
# 5 count_inter             0.123      0.125  0.0954  0.104
# 6 count_retail            0.235      0.233  0.537   0.589
# 7 trendecon              -0.904     -0.937 -0.0122 -0.432
# 8 casualties              0.109      0.110  0.105   0.0976

# overall, daily_tweaked is beaten by prophet only
summarize(overview, across(-series, function(e) mean(abs(e))))

# # A tibble: 1 x 5
#   daily_tweaked daily_orig prophet   dsa naive
#           <dbl>      <dbl>   <dbl> <dbl> <dbl>
# 1         0.250      0.253   0.214 0.297 0.408

# discussion
#
# - tweaks to not have a major impact on performance, but may make it more
#   robust towards longer and shorter series (not in the sample)




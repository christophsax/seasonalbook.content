# seasadj

## Getting started

```r
library(dailyadj)

# example series
count
count_inter
count_retail

transact
transact_inter
transact_retail

# plot using dygraphs
ts_dygraphs(transact)
ts_dygraphs(count)


# adjustment using stl (recomended)
ans_count <- seas_daily(count)

# alternative adjustments
# seas_dummy(count)
# seas_prophet(count)  # slow
# seas_dsa(count)      # very slow



plot_components(ans_count)

ts_plot(oos_eval(count, seas_daily))
ts_plot(oos_eval(count, seas_dummy))



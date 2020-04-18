oos_eval <- function(x, seas_fun, by = "-1 month", end_short = tsbox:::time_shift(ts_summary(x)$end, by)) {

  x_short <- ts_span(x, end = end_short)

  out <- seas_fun(x_short)

  out %>%
    ts_pick("fct") %>%
    ts_c(x) %>%
    ts_span(tsbox:::time_shift(end_short, "1 day"))
}



oos_summary <- function(x){
  x %>%
    ts_wide() %>%
    mutate(diff = abs(fct - x)) %>%
    mutate(diff_sq = diff^2) %>%
    mutate(pc = diff / x) %>%
    summarize(
      mae = mean(diff, na.rm = TRUE),
      mrse = sqrt(mean(diff_sq, na.rm = TRUE)),
      mpce = mean(pc, na.rm = TRUE)
    )
}

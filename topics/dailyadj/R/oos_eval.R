oos_eval <- function(x, seas_fun, by = "-1 month", end_short = tsbox:::time_shift(ts_summary(x)$end, by)) {

  x_short <- ts_span(x, end = end_short)

  out <- seas_fun(x_short)

  out %>%
    ts_pick("fct") %>%
    ts_c(x) %>%
    ts_span(tsbox:::time_shift(end_short, "1 day")) %>%
    ts_span(end = ts_summary(x)$end)

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


oos_evals <- function(x, seas_fun, by = "-1 month") {

  ends <- seq(as.Date("2015-01-01"), as.Date("2014-02-01"), by = by)
  xs <- setNames(lapply(ends, function(end) ts_span(x, end = end)), ends)
  z <- lapply(xs, oos_eval, seas_fun = seas_fun, by = by)

  bind_rows(z, .id = "end") %>%
    ts_regular()

}


plot_oos_evals <- function(x) {
  ggplot(x, aes(x = time, y = value)) +
  geom_line(aes(color = id)) +
  facet_wrap(vars(end), scales = "free_x")
}



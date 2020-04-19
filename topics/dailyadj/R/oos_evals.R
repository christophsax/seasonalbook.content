

oos_evals <- function(x, seas_fun, by = "-1 month") {

  ends <- seq(as.Date("2015-01-01"), as.Date("2014-02-01"), by = by)
  xs <- setNames(lapply(ends, function(end) ts_span(x, end = end)), ends)
  z <- lapply(xs, oos_eval, seas_fun = seas_fun, by = by)

  bind_rows(z, .id = "end") %>%
    ts_regular()

}


summary_oos_evals <- function(x){
  z <-
    x %>%
    pivot_wider(names_from = id, values_from = value) %>%
    mutate(diff = abs(fct - x)) %>%
    mutate(diff_sq = diff^2) %>%
    mutate(pc = diff / x) %>%
    group_by(end) %>%
    summarize(
      mrse = sqrt(mean(diff_sq, na.rm = TRUE)),
      mae = mean(diff, na.rm = TRUE),
      mpce = mean(pc, na.rm = TRUE)
    ) %>%
    ungroup()

  bind_rows(
    z,
    add_column(summarize_at(z, vars(-end), mean), end = "Mean", .before = 1)
  )

}

plot_oos_evals <- function(x) {
  ggplot(x, aes(x = time, y = value)) +
  geom_line(aes(color = id)) +
  facet_wrap(vars(end), scales = "free_x")
}



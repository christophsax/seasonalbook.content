

oos_evals <- function(x, seas_fun, by = "-1 month") {

  periods <- seq(as.Date("2015-01-01"), as.Date("2014-02-01"), by = by)
  xs <- setNames(lapply(periods, function(period) ts_span(x, end = period)), periods)
  z <- lapply(xs, oos_eval, seas_fun = seas_fun, by = by)

  bind_rows(z, .id = "period") %>%
    ts_regular()

}


summary_oos_evals <- function(x){
  z <-
    x %>%
    pivot_wider(names_from = id, values_from = value) %>%
    mutate(diff = abs(fct - x)) %>%
    mutate(diff_sq = diff^2) %>%
    mutate(pc = diff / x) %>%
    group_by(period) %>%
    summarize(
      mrse = sqrt(mean(diff_sq, na.rm = TRUE)),
      mae = mean(diff, na.rm = TRUE),
      mpce = mean(pc, na.rm = TRUE)
    ) %>%
    ungroup()

  bind_rows(
    z,
    add_column(summarize_at(z, vars(-period), mean), period = "Mean", .before = 1)
  )

}

plot_oos_evals <- function(x) {
  ggplot(x, aes(x = time, y = value)) +
  geom_line(aes(color = id)) +
  facet_wrap(vars(period), scales = "free_x")
}



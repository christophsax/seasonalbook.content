#' Evalutate OOS forecast errors over multiple periods
#'
#' @examples
#' oos_evals(casualities, seas_daily)
#' oos_evals(transact, seas_daily)
#' @exports
oos_evals <- function(x, seas_fun) {

  by = "-1 month"
  # noramlize
  x <- ts_tbl(ts_default(x))

  # determine the last 12 periods
  last_period_start <- as.Date(paste(lubridate::year(max(x$time)), lubridate::month(max(x$time)), "1", sep = "-"))
  periods <- seq(tsbox:::time_shift(last_period_start, "1 month"), by = by, length.out = 12) - 1

  xs <- setNames(lapply(periods, function(period) ts_span(x, end = period)), periods)
  z <- lapply(xs, oos_eval, seas_fun = seas_fun)

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



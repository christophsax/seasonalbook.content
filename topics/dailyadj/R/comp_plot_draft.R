comp_plot_draft <- function(z) {
  w_time <-
    ts_pick(z, "seas_w") %>%
    add_effects() %>%
    ggplot(aes(x = time, y = value)) +
    geom_point() +
    facet_wrap(vars(wday))

  w_pattern <-
    ts_pick(z, "seas_w") %>%
    add_effects() %>%
    ggplot(aes(x = wday, y = value)) +
    geom_point() +
    facet_wrap(vars(year))

  m_time <-
  ts_pick(z, "seas_m") %>%
    add_effects() %>%
    ggplot(aes(x = time, y = value)) +
    geom_point() +
    facet_wrap(vars(mday))

  m_pattern <-
  ts_pick(z, "seas_m") %>%
    add_effects() %>%
    ggplot(aes(x = mday, y = value)) +
    geom_point() +
    facet_wrap(vars(year))

  # y_time <-
  # ts_pick(z, "seas_y") %>%
  #   add_effects() %>%
  #   ggplot(aes(x = time, y = value)) +
  #   geom_point() +
  #   facet_wrap(vars(yday))

  y_pattern <-
  ts_pick(z, "seas_y") %>%
    add_effects() %>%
    ggplot(aes(x = yday, y = value)) +
    geom_line() +
    facet_wrap(vars(year))

  list(
    w_time = w_time,
    w_pattern = w_pattern,
    m_time = m_time,
    m_pattern = m_pattern,
    # y_time = y_time,
    y_pattern = y_pattern
  )
}

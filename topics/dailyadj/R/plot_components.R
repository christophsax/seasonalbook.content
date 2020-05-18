plot_pattern_trend <- function(z) {
  ts_pick(z, "trend") %>%
  ggplot(aes(x = time, y = value, color)) +
  geom_line() +
  theme_cowplot()
}

plot_pattern_w <- function(z) {
  ts_pick(z, "seas_w") %>%
  add_effects() %>%
  group_by(year = as.factor(year), wday) %>%
  summarize(value = mean(value)) %>%
  ungroup() %>%
  ggplot(aes(x = wday, y = value, color = year)) +
  geom_line() +
  theme_cowplot()
}

plot_pattern_m <- function(z) {
  ts_pick(z, "seas_m") %>%
  add_effects() %>%
  group_by(year = as.factor(year), mday) %>%
  summarize(value = mean(value)) %>%
  ungroup() %>%
  ggplot(aes(x = mday, y = value, color = year)) +
  geom_line() +
  theme_cowplot()
}

plot_pattern_y <- function(z) {
  ts_pick(z, "seas_y") %>%
  add_effects() %>%
  group_by(year = as.factor(year), yday) %>%
  summarize(value = mean(value)) %>%
  ungroup() %>%
  ggplot(aes(x = yday, y = value, color = year)) +
  geom_line() +
  theme_cowplot()
}

plot_pattern_x <- function(z) {
  ts_pick(z, "seas_x") %>%
  ggplot(aes(x = time, y = value, color)) +
  geom_line() +
  theme_cowplot()
}

#' @export
plot_components <- function(z) {
  library(ggplot2)
  library(cowplot)

  plot_grid(
    plot_pattern_trend(z),
    plot_pattern_y(z),
    plot_pattern_m(z),
    plot_pattern_w(z),
    plot_pattern_x(z),
    labels = c('trend', 'year', 'monht', 'week', "holiday"),
    ncol = 2
  )
}


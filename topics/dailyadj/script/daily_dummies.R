library(tsbox)

x_wide <- ts_wide(seas_loess10(x))

x_wide %>%
  select(time, orig, adj) %>%
  ts_long() %>%
  ts_dygraphs()

hol_ch <-
  as_tibble(prophet::generated_holidays) %>%
  filter(country == "CH") %>%
  mutate(time = as.character(ds), .keep = "unused") %>%
  mutate(holiday = as.character(holiday)) %>%
  select(-year, -country)

genhol_daily <- function(x) {
  x <- as.Date(x)
  from <- as.Date(paste0(data.table::year(x[1]), "-01-01"))
  to <- as.Date(paste0(data.table::year(rev(x)[1]), "-12-31"))
  tibble(time = seq(from, to, by = "day")) %>%
    left_join(tibble(time = x, fit = x), by = "time") %>%
    mutate(is_holiday = as.integer(!is.na(fit))) %>%
    select(-fit)
}



xreg <-
  hol_ch %>%
  group_by(holiday) %>%
  summarize(genhol_daily(time)) %>%
  ungroup() %>%
  ts_wide() %>%
  mutate(time = lag(time)) %>%
  filter(!is.na(time))

dta_m <-
  x_wide %>%
  select(time, value = adj) %>%
  left_join(xreg)

m <- lm(value ~ ., data = select(dta_m, -1), na.action = na.exclude)
dta_m %>%
  select(time) %>%
  mutate(seas_x = unname(predict(m)) - coef(m)['(Intercept)'])



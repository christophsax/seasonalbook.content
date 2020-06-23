# @param df data.frame of the form
#  A tibble: 900 x 2
#   holiday             time
#   <chr>               <date>
# 1 Neujahrestag -1     1994-12-31
# 2 Karfreitag -1       1995-04-13
# 3 Ostern -1           1995-04-15
# 4 Ostermontag -1      1995-04-16
# 5 Auffahrt -1         1995-05-24


# @example
# df <- bind_rows(
#   mutate(holidays(), holiday = paste(holiday, "-1"), time = time - 1),
#   holidays()
# )
# seas_x(transact, df)


# holiday adjustment
seas_x <- function(x, df) {
  xreg <-
    hol %>%
    group_by(holiday) %>%
    summarize(genhol_daily(time)) %>%
    ungroup() %>%
    ts_wide()

  dta_m <-
    x_wide %>%
    select(time, value = adj) %>%
    left_join(xreg, by = "time")

  # estimate a few times and only keep significant vars
  m_ols <- lm(value ~ ., data = dta_m[,-1])
  m_ols <- lm(value ~ ., data = select(dta_m[,-1], !!significant_vars(m_ols)$term, value))
  m_ols <- lm(value ~ ., data = select(dta_m[,-1], !!significant_vars(m_ols)$term, value), na.action = na.exclude)

  # prediction, based on final model
  dta_m %>%
    select(time) %>%
    mutate(seas_x = unname(predict(m_ols)) - coef(m_ols)['(Intercept)'])
}


genhol_daily <- function(x) {
  x <- as.Date(x)
  from <- as.Date(paste0(data.table::year(x[1]), "-01-01"))
  to <- as.Date(paste0(data.table::year(rev(x)[1]), "-12-31"))
  tibble(time = seq(from, to, by = "day")) %>%
    left_join(tibble(time = x, fit = x), by = "time") %>%
    mutate(value = as.integer(!is.na(fit))) %>%
    select(-fit)
}

significant_vars <- function(x) {
  broom::tidy(x) %>%
  filter(p.value < 0.05) %>%
  select(term, estimate) %>%
  mutate(term = gsub("TRUE", "", term)) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = gsub("\\`", "", term))
}


holidays <- function(country = "CH") {
  as_tibble(prophet::generated_holidays) %>%
  filter(country == "CH") %>%
  mutate(time = as.Date(as.character(ds)), .keep = "unused") %>%
  mutate(holiday = as.character(holiday)) %>%
  select(-year, -country)
}

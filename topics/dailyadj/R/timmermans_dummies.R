timmermans_dummies <- function(x, h = 30) {

  # ARIMA with dummy variables for days of the week and first,middle and last
  # three days of the month (Dummy model 1).
  x_dummies <-
    x %>%
    add_weekdays(n = h) %>%
    rename(orig = value) %>%
    mutate(
      wday = data.table::wday(time),
      year = data.table::year(time),
      month = data.table::month(time)
    ) %>%
    group_by(year,month) %>%
    mutate(start = seq(n())) %>%
    mutate(end = rev(seq(n()))) %>%
    mutate(middle = (start - as.integer(median(start)) + 2L)) %>%
    ungroup() %>%
    select(time,wday,start,end,middle) %>%
    mutate(start = if_else(start %in% c(1,2,3),start,0L)) %>%
    mutate(end = if_else(end %in% c(1,2,3),end,0L)) %>%
    mutate(middle = if_else(middle %in% c(1,2,3),middle,0L))

  stopifnot((NROW(x) + h) == NROW(x_dummies))

  # ts_interpolate <- ts_(function(x,...) imputeTS::na_interpolation(x,...))
  ts_dum <-
    x_dummies %>%
    ts_long()  %>%
    # ts_regular() %>%
    # mutate(value = as.character(coalesce(value,0L)))
    mutate(value = as.character(value,0L))


  ts_dum
}

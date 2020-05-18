#' Seasonal adjustment
#'
#' @export
seas_naive <- function(x, h = 30) {

  validate_seas_input(x)


  last_year <- max(lubridate::year(x$time)) - 1

  monthly_means <- filter(x, lubridate::year(time) %in% c(!!last_year:(!!last_year - 3))) %>%
    group_by(month = lubridate::month(time)) %>%
    summarize(fct_naive = mean(value, na.rm = TRUE)) %>%
    ungroup()


  z_wide <-
    x %>%
    add_weekdays(n = h) %>%
    mutate(month = lubridate::month(time)) %>%
    left_join(monthly_means, by = "month") %>%
    mutate(orig = value) %>%
    mutate(fct = if_else(is.na(value), fct_naive, value)) %>%
    mutate(trend = mean(orig, na.rm = TRUE)) %>%
    mutate(seas_m = 0) %>%
    mutate(seas_y = (fct_naive - trend)) %>%
    mutate(seas_w = 0) %>%
    mutate(seas_x = 0) %>%
    mutate(adj = orig - seas_y) %>%
    mutate(seas = fct_naive) %>%
    select(-month, -fct_naive, -value) %>%
    select(time, !! .exp_cols_comp)

  z <- ts_long(z_wide)

  validate_seas_output(z)
}


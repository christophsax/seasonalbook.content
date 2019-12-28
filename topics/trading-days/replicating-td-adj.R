#' ---
#' title: Trading Days
#' output: github_document
#' ---
#'
#' ## Key Questions
#'
#' - Can we replicate X-13 trading days adjustment
#' - Can we include the regressors in seasonal, with an example
#'
#'
#' ## Conclusion
#'
#' Replication is possible. Adding the regressors to seasonal may be useful.

suppressPackageStartupMessages(library(tidyverse))
library(tsbox)
library(seasonal)


#' ## Examples
#'
#' ### Constructing weekday regressors

dates <- seq(as.Date("1931-01-01"), as.Date("2030-12-31"), by = "day")

first_of_month <- function(x) {
  as.Date(paste(
    data.table::year(dates),
    data.table::month(dates),
    1,
    sep = "-"
  ))
}

#' td1nolpyear
#' : Include the weekday-weekend contrast variable (monthly and quarterly flow
#'   eries only): (no. of weekdays) −(5/2) (no. of Saturdays and Sundays).
#'
#' tdnolpyear
#' : Include the six day-of-week contrast variables (monthly and quarterly flow
#'   series only): (no. of Mondays) − (no. of Sundays), . . . , (no. of Saturdays) −
#'   (no. of Sundays).

td_m_tbl <-
  tibble(dates, wd = as.POSIXlt(dates)$wday, name = weekdays(dates)) %>%
  group_by(time = first_of_month(dates)) %>%
  summarize(
    td1 = sum(wd %in% 1:5) - 5 / 2 * sum(wd %in% c(6, 0)),
    mon = sum(wd == 1) - sum(wd == 0),
    tue = sum(wd == 2) - sum(wd == 0),
    wed = sum(wd == 3) - sum(wd == 0),
    thu = sum(wd == 4) - sum(wd == 0),
    fri = sum(wd == 5) - sum(wd == 0),
    sat = sum(wd == 6) - sum(wd == 0)
  )

td_m_tbl

#' 'Trading day adjustment' removes the effect of the weekdays, and but does not
#' include holidays, such as Christmas or Easter. These are handled separately
#' (Easter) or dealt with by standard seasonal adjustment (Christmas).

td1nolpyear <-
  td_m_tbl %>%
  select(time, value = td1) %>%
  ts_ts()

tdnolpyear <-
  td_m_tbl %>%
  select(-td1) %>%
  ts_long() %>%
  ts_ts()


#' ### Single coef

m1 <- seas(
  AirPassengers,
  xreg = td1nolpyear,
  regression.aictest = NULL,
  outlier = NULL,
  regression.usertype = "td"
)
summary(m1)

m2 <- seas(
  AirPassengers,
  regression.aictest = NULL,
  outlier = NULL,
  regression.variables = c("td1nolpyear", outlier = NULL)
)
summary(m2)


#' ### All coefs

m3 <- seas(
  AirPassengers,
  xreg = tdnolpyear,
  regression.aictest = NULL,
  outlier = NULL,
  regression.usertype = "td"
)
summary(m3)

m4 <- seas(
  AirPassengers,
  regression.aictest = NULL,
  outlier = NULL,
  regression.variables = c("tdnolpyear", outlier = NULL)
)
summary(m4)


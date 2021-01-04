#' span_scale(casualties)
span_scale <- function(x) {
  tss <- ts_summary(x)
  no_of_years <- lubridate::year(tss$end) - lubridate::year(tss$start)
  no_of_years / 10
}

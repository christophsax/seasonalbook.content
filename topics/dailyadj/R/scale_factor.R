# scale_factor(casualties)
#' @export
scale_factor <- function(x) {
  tss <- ts_summary(x)
  no_of_years <- lubridate::year(tss$end) - lubridate::year(tss$start)
  no_of_years / 10
}

add_effects <- function(x) {
  x %>%
    mutate(
      wday = data.table::wday(time),
      mday = data.table::mday(time),
      yday = data.table::yday(time),
      year = data.table::year(time)
    )
}

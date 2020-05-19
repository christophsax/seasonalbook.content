# x <- transact$time


# return 28 at feb 29
# mday_leap(x)
mday_leap <- function(x) {
  # is_feb29 <- (data.table::month(x) == 2) & (data.table::mday(x) == 29)
  z <- data.table::mday(x)
  # z[is_feb29] <- 28
  z
}



# return 28 at feb 29
# yday_leap(x)
yday_leap <- function(x) {
  is_leap_year <- lubridate::leap_year(x)
  z <- lubridate::yday(x)
  affected <- is_leap_year & (z >= 60)
  z[affected] <- z[affected] - 1
  z
}



# Principles of seas_fun:
# - x, a single daily times, as a data.frame
# - output: z


.exp_cols <- c("trend", "seas", "fct", "orig", "adj")

validate_seas_input <- function(x) {
  stopifnot(inherits(x, "data.frame"))
  stopifnot(identical(colnames(x), c("time", "value")))
  stopifnot(tsbox::ts_boxable(x))
  stopifnot(tsbox::ts_summary(x)$diff == "1 day")
  x
}



validate_seas_output <- function(z) {
  stopifnot(inherits(z, "data.frame"))
  stopifnot(identical(colnames(z), c("id", "time", "value")))
  stopifnot(tsbox::ts_boxable(z))
  stopifnot(all(tsbox::ts_summary(z)$diff == "1 day"))
  stopifnot(length(setdiff(.exp_cols, unique(z$id))) == 0)
  z
}

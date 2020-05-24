# Using seas with multiple series, interface mockup

# call -------------------------------------------------------------------------

# this seems straigtforward

mdta <- cbind(a = AirPassengers, b = AirPassengers)

# multiple series, one spec
seas(x = mdta, x11 = "")

# alternatively, using list =
seas(x = mdta, list = list(x11 = ""))

# multiple series, multiples specs
seas(
  x = mdta,
  # lengths of list must be equal to number of series
  list = list(
    list(x11 = ""),
    list()
  )
)

# alternatively, use x in lists
seas(
  # lengths of list must be equal to number of series
  list = list(
    list(x = AirPassengers, x11 = ""),
    list(x = AirPassengers)
  )
)

# composite spec ---------------------------------------------------------------





# extractor functions ----------------------------------------------------------

m <- seas(x = mdta, x11 = "")
final(m)
#                 a        b
# Jan 1949 122.7133 122.7133
# Feb 1949 124.7657 124.7657
# Mar 1949 125.0734 125.0734
# Apr 1949 127.5286 127.5286
# May 1949 127.3584 127.3584


as.tslist <- function(x) {
  stopifnot(is.list(x))
  class(x) <- "tslist"
  x
}

tsbox::ts_ts(as.tslist(lapply(tsbox::ts_tslist(mdta), function(e) final(seas(e)))))

original(m)
#            a   b
# Jan 1949 112 112
# Feb 1949 118 118
# Mar 1949 132 132


tsbox::ts_ts(as.tslist(lapply(tsbox::ts_tslist(mdta), function(e) resid(seas(e)))))

resid(m)
#                      a             b
# Jan 1949 -0.0006860706 -0.0006860706
# Feb 1949  0.0064511533  0.0064511533
# Mar 1949 -0.0052868236 -0.0052868236
# Apr 1949  0.0083328630  0.0083328630


multiseas


lapply(tsbox::ts_tslist(mdta), function(e) fivebestmdl(seas(e)))


fivebestmdl(m)

# $a
#            arima    bic
# 1 (0 1 0)(0 1 1) -4.007
# 2 (1 1 1)(0 1 1) -3.986
# 3 (0 1 1)(0 1 1) -3.979
# 4 (1 1 0)(0 1 1) -3.977
# 5 (0 1 2)(0 1 1) -3.970

# $b
#            arima    bic
# 1 (0 1 0)(0 1 1) -4.007
# 2 (1 1 1)(0 1 1) -3.986
# 3 (0 1 1)(0 1 1) -3.979
# 4 (1 1 0)(0 1 1) -3.977
# 5 (0 1 2)(0 1 1) -3.970


static(m)

# seas(
#   list = list(
#     list(
#       x = AirPassengers,
#       regression.variables = c("td1coef", "easter[1]", "ao1951.May"),
#       arima.model = "(0 1 1)(0 1 1)",
#       regression.aictest = NULL,
#       outlier = NULL,
#       transform.function = "log"
#     ),
#     list(
#       x = AirPassengers,
#       regression.variables = c("td1coef", "easter[1]", "ao1951.May"),
#       arima.model = "(0 1 1)(0 1 1)",
#       regression.aictest = NULL,
#       outlier = NULL,
#       transform.function = "log"
#     )
#   )
# )

# ideally, a mulitseas object is just a list of seas objects, so we could still
# use all the functions on individual elements. So we may not need e.g. static()







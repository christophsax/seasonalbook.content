#' ---
#' output: github_document
#' ---

library(seasonal)
library(microbenchmark)


devtools::load_all()

# z <- multiseas(tibble::lst(mdeaths, fdeaths))


multiseas(tibble::lst(mdeaths, fdeaths), dir = "~/Desktop/tmp")


library(ggplot2)

n_series <- c(1, 10, 100, 1000)
n_runs <- c(100, 10, 1, 1, 1)

series <- rep(list(AirPassengers), max(n_series))
names(series) <- sprintf("series%d", seq(max(n_series)))


times_seas <- lapply(seq_along(n_series), function(i) {
  message(sprintf("Normal seas, %d series...", n_series[i]))
  microbenchmark::microbenchmark(
    lapply(series[seq(n_series[i])], seas),
    times = n_runs[i],
    unit = "ms"
  )
})

times_multiseas <- lapply(seq_along(n_series), function(i) {
  message(sprintf("Multiseas %d series...", n_series[i]))
  microbenchmark::microbenchmark(
    multiseas(series[seq(n_series[i])]),
    times = n_runs[i],
    unit = "ms"
  )
})


times_seas
times_multiseas

# If the batch mode is

# > Only write the series that are needed in the output (seas does not respect the save parameter for x11 and always stores all series)

# > In the interest of completeness, seas reads all output produced by the x13 binary program (.mdl, .udg, .html …). Again if only the series are of interest then these do not need to be read.

# > seas reads the resulting series twice (once into the series element of the output and once into data). This overhead can be avoided in batch use.

# > The optimal number of series to be processed in a batch needs to be found. multiseas could then do chunking internally.



basics.R
================
christoph
2020-05-24

``` r
library(seasonal)
library(microbenchmark)


devtools::load_all()
```

``` r
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
```

    ## Normal seas, 1 series...

    ## Normal seas, 10 series...

    ## Normal seas, 100 series...

    ## Normal seas, 1000 series...

``` r
times_multiseas <- lapply(seq_along(n_series), function(i) {
  message(sprintf("Multiseas %d series...", n_series[i]))
  microbenchmark::microbenchmark(
    multiseas(series[seq(n_series[i])]),
    times = n_runs[i],
    unit = "ms"
  )
})
```

    ## Multiseas 1 series...

    ## Multiseas 10 series...

    ## Multiseas 100 series...

    ## Multiseas 1000 series...

``` r
times_seas
```

    ## [[1]]
    ## Unit: milliseconds
    ##                                    expr      min       lq     mean  median
    ##  lapply(series[seq(n_series[i])], seas) 125.5606 129.2331 133.1408 132.317
    ##        uq      max neval
    ##  135.4938 169.6872   100
    ##
    ## [[2]]
    ## Unit: milliseconds
    ##                                    expr      min       lq     mean   median
    ##  lapply(series[seq(n_series[i])], seas) 1309.351 1328.966 1354.063 1360.944
    ##        uq      max neval
    ##  1373.011 1388.121    10
    ##
    ## [[3]]
    ## Unit: milliseconds
    ##                                    expr     min      lq    mean  median      uq
    ##  lapply(series[seq(n_series[i])], seas) 13219.4 13219.4 13219.4 13219.4 13219.4
    ##      max neval
    ##  13219.4     1
    ##
    ## [[4]]
    ## Unit: milliseconds
    ##                                    expr      min       lq     mean   median
    ##  lapply(series[seq(n_series[i])], seas) 132598.8 132598.8 132598.8 132598.8
    ##        uq      max neval
    ##  132598.8 132598.8     1

``` r
times_multiseas
```

    ## [[1]]
    ## Unit: milliseconds
    ##                                 expr      min       lq     mean  median      uq
    ##  multiseas(series[seq(n_series[i])]) 125.0481 130.3679 133.8142 132.148 134.754
    ##       max neval
    ##  246.6131   100
    ##
    ## [[2]]
    ## Unit: milliseconds
    ##                                 expr      min       lq     mean   median
    ##  multiseas(series[seq(n_series[i])]) 1124.333 1139.272 1148.959 1148.671
    ##        uq      max neval
    ##  1161.528 1168.556    10
    ##
    ## [[3]]
    ## Unit: milliseconds
    ##                                 expr      min       lq     mean   median
    ##  multiseas(series[seq(n_series[i])]) 11318.32 11318.32 11318.32 11318.32
    ##        uq      max neval
    ##  11318.32 11318.32     1
    ##
    ## [[4]]
    ## Unit: milliseconds
    ##                                 expr      min       lq     mean   median
    ##  multiseas(series[seq(n_series[i])]) 116129.2 116129.2 116129.2 116129.2
    ##        uq      max neval
    ##  116129.2 116129.2     1

``` r
# If the batch mode is

# > Only write the series that are needed in the output (seas does not respect the save parameter for x11 and always stores all series)

# > In the interest of completeness, seas reads all output produced by the x13 binary program (.mdl, .udg, .html …). Again if only the series are of interest then these do not need to be read.

# > seas reads the resulting series twice (once into the series element of the output and once into data). This overhead can be avoided in batch use.

# > The optimal number of series to be processed in a batch needs to be found. multiseas could then do chunking internally.
```

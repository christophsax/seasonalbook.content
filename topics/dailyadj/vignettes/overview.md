
# Seasonal adjustment of daily time series in R: An overview

Automated data processing and the Internet have brought an enormous
increase in data that is processed on a high frequency, e.g., at a
daily, hourly or even higher frequency. While some higher frequency
series have been used in the past (e.g., Fisher 1923, cited by Ladiray
2018) these series are much more abundant now. X-13ARIMA-SEATS offers a
well tested and time proven way of adjusting monthly, quarterly (or
bi-annual) series, but it cannot deal with data at a higher frequency.

This \[article/chaper/post\] discusses how to perform seasonal
adjustment on a higher frequency. We focus on daily data, as this is the
most common use case, but will briefly discuss some challenges involving
weekly or intra-day adjustments.

Despite the large interest, there is not much consensus on the
appropriate adjustment method for daily series. Adjusting daily series
often involves a substantial amount of trial and error, subjective
judgment and exploration. This \[article/chapter/post\] gives an
overview of the tools that are currently (2020) available in R.

## Problems

Daily seasonal adjustment comes with a few challenges that are not
present in lower frequency data. Let’s focus on daily ice-cream sales.

First, daily data comes at multiple periodicities: There is an annual
periodicity, such as the effect of the temperature in the summer. Then
there is a weekly periodicity. Ice-cream sales may be higher during the
weekends. For some series, there may be also a monthly periodicity. If
people are get their salaries by the end of the month, they may be more
likely to perform certain investments.

Second, many daily data series are available for a few years only.
Whily, e.g., the SEATS adjustment requires a minimal series length of XX
years, many daily series are shorter.

Third, higher frequency series are generally more volatile and prone to
outliers.

Fourth, the effect of individual holiday is challenging to estimate.
Often, economic effects of holidays may occur before or after a holiday,
thus lagging or leading them is crucial.

## Parametric versus Non-parametric Models

Various attempts to seasonly adjust data can be broadly distinguished
into parametric and non-parametric approaches. Non-parametric approaches
seem to be the more obvious candidates to use with the irregular
structure of daily data. Parametric models require the time units to be
regularly spaced. Non-parametric estimation is also what is used in the
X-11 method of X-13.

## R Packages

As mentioned before, there is no accepted consensus on how to perform
daily seasonal adjustment. In the following, we discuss various
possibilities to adjust series in R. We focus on a single time series,
and describe the concrete steps that are required to perform an
adjustment.

``` r
library(dailyadj)
library(tsbox)

x <- casualities


ts_plot(x)
```

![](overview_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

### ARIMA + Month, Weekday / Holiday Dummies

Let’s start with a simple model.

Perhaps because of their simplicity, these kind of adjustments are
frequently found in the literature. E.g., timmermans 18, lengwiler,
(forthcoming, ask Ronald)

We start by constructing a dummy variables with weekday and monthly
effects:

``` r
dums <-
  x %>%
  mutate(wday = lubridate::wday(time, label = TRUE)) %>%
  mutate(month = lubridate::month(time, label = TRUE)) %>%
  select(time, wday, month) %>%
  fastDummies::dummy_cols("wday", remove_selected_columns = TRUE) %>%
  fastDummies::dummy_cols("month", remove_selected_columns = TRUE) %>%
  select(-wday_Mon, -month_Jan)

dums
```

    ## # A tibble: 4,383 x 18
    ##    time       wday_Sun wday_Tue wday_Wed wday_Thu wday_Fri wday_Sat month_Feb
    ##    <date>        <int>    <int>    <int>    <int>    <int>    <int>     <int>
    ##  1 2005-01-01        0        0        0        0        0        1         0
    ##  2 2005-01-02        1        0        0        0        0        0         0
    ##  3 2005-01-03        0        0        0        0        0        0         0
    ##  4 2005-01-04        0        1        0        0        0        0         0
    ##  5 2005-01-05        0        0        1        0        0        0         0
    ##  6 2005-01-06        0        0        0        1        0        0         0
    ##  7 2005-01-07        0        0        0        0        1        0         0
    ##  8 2005-01-08        0        0        0        0        0        1         0
    ##  9 2005-01-09        1        0        0        0        0        0         0
    ## 10 2005-01-10        0        0        0        0        0        0         0
    ## # … with 4,373 more rows, and 10 more variables: month_Mar <int>,
    ## #   month_Apr <int>, month_May <int>, month_Jun <int>, month_Jul <int>,
    ## #   month_Aug <int>, month_Sep <int>, month_Oct <int>, month_Nov <int>,
    ## #   month_Dec <int>

These variables can be used as exogenous variables in a ARIMA model. We
use `forecat::auto.arima()` to determine the ARMA order. Note that we do
not want to use the seasonal part of the model, since we use dummies for
this purpose.

``` r
fit <- auto.arima(x$value, seasonal = FALSE, xreg = as.matrix(dums[, -1]))

adj <- x
adj$value <- as.numeric(fit$fitted)

ts_plot(x, adj)
```

![](overview_files/figure-gfm/arimax-1.png)<!-- -->

The nice think about the dummy model is that its seasonal effects are
very easy to interprete. By construction, they are constant over time,
and can be visualized as follows:

``` r
enframe(coef(fit)) %>%
  filter(grepl("wday", name)) %>%
  mutate(name = gsub("wday_", "", name)) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  ggplot(aes(x = name, y = value)) +
    geom_col() +
    ggtitle("Weekday effects", subtitle = "Baseline: Monday")
```

![](overview_files/figure-gfm/coeff-plots-1.png)<!-- -->

``` r
enframe(coef(fit)) %>%
  filter(grepl("month", name)) %>%
  mutate(name = gsub("month_", "", name)) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  ggplot(aes(x = name, y = value)) +
    geom_col() +
    ggtitle("Month effects", subtitle = "Baseline: January")
```

![](overview_files/figure-gfm/coeff-plots-2.png)<!-- -->

We see that, on average, transcations are lower on Sunday and peak on
Friday. We also see that, on average, transactions are sligthly lower in
early autumn.

### STL

This package contains a simple implementation of STL

``` r
seas_daily(x) %>%
  ts_pick("orig", "adj") %>%
  ts_plot()
```

![](overview_files/figure-gfm/stl-1.png)<!-- -->

### dsa

``` r
library(dsa)

z <- dsa::dsa(ts_xts(x))
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |===                                                                   |   5%  |                                                                              |=======                                                               |  10%  |                                                                              |====================                                                  |  29%  |                                                                              |===========================                                           |  38%  |                                                                              |===============================================                       |  67%  |                                                                              |==================================================                    |  71%  |                                                                              |=====================================================                 |  76%  |                                                                              |===============================================================       |  90%  |                                                                              |======================================================================| 100%

### prophet

``` r
library(prophet)
df <- rename(x, ds = time, y = value)
m <-
  prophet(daily.seasonality = FALSE) %>%
  # including swiss holidays, which seems to have no effect
  add_country_holidays(country_name = 'CH') %>%
  fit.prophet(df)

# not strictly needed, but will include forecast too
future <- make_future_dataframe(m, periods = 31)
forecast <- as_tibble(predict(m, future))

forecast %>%
  transmute(
    time = as.Date(ds),
    additive_terms,
    yhat
  ) %>%
  left_join(x, by = "time") %>%
  mutate(adj = value - additive_terms) %>%
  select(time, value, adj) %>%
  ts_long() %>%
  ts_plot()
```

![](overview_files/figure-gfm/prophet-1.png)<!-- -->

### stlf

Some models require the data to be equispaced. I.e., each low frequency
period must include the same number of high frequency periods. `ts_ts`
from the tsbox package offers an easy way to convert daily data into
regular `"ts"` objects with a frequency of 365.2425. Thus, days are
slightly offset in each year.

``` r
x_ts <- ts_ts(x)
head(x_ts)
```

    ## Time Series:
    ## Start = 2005 
    ## End = 2005.01368953503 
    ## Frequency = 365.2425 
    ## [1] 452 468 418 599 686 710

\[Probably don’t show everything. This one contributes nothing\]

``` r
m <- stlf(x_ts)
fct <- ts_tbl(m$mean)
adj <- m$fitted
ts_plot(x, adj)
```

![](overview_files/figure-gfm/stlf-1.png)<!-- -->

### TBATS

Other models require the data to be equispaced and without missing
values. Imputation offers an easy way out.

``` r
fit <- tbats(imputeTS::na_interpolation(x_ts))
adj <- fit$fitted
ts_plot(x, adj)
```

![](overview_files/figure-gfm/tbats-1.png)<!-- -->

## Evaluation

Based on the example above, compare according to the following criteria:

### Out-of-sample forecast

As in (timmermans 18).

The function `dailyseas::eval_oos()` performs an OOS forecast evaluation
for all models.

\[TODO this still needs to be implemented, but I have most of the code
needed for it.

dsa is probably the best, prophet 2nd, and then the rest. My stl
procedure is also decent.\]

\[Main Table like this:\]

    seas\_stlf     Mean       19917. 14809. 0.122
    seas\_prophet  Mean       19082. 13934. 0.117
    seas\_dsa      Mean       19082. 13934. 0.117
    ...

### Other Criteria??

  - Variance?

  - Tests for remaining seasonality?

  - ???

### Computation time

    stl (my one): 0.639
    seas_dummy:   2.319
    prophet:     10.181
    dsa:        145.553

## Conclusions
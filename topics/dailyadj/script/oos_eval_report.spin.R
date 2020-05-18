#' ---
#' title: OOS-Evaluation Report
#' output: html_document
#' params:
#'   x: !r transact
#'   x_name: "transact"
#' ---

#+ init, include=FALSE

library(knitr)
library(forecast)
library(tidyverse)
library(tsbox)
library(ggplot2)
library(cowplot)
opts_chunk$set(cache = FALSE) # does not work with parametrized reports

pkgload::load_all(here::here("."))



x <- params$x
x_name <- params$x_name

# x <- transact
# x_name <- "transact"



#+ calculaitons, include=FALSE

funs <- lst(
  seas_naive,
  seas_loess5,
  seas_prophet,
  seas_dsa
)

models_raw <-
  tibble(name = names(funs)) %>%
  mutate(ff = unname(funs))


models_raw_eval <-
  models_raw %>%
  mutate(result = map(ff, function(e) oos_evals(x, e))) %>%
  mutate(latest = map(ff, function(e) e(x)))

models <-
  models_raw_eval %>%
  mutate(smry = lapply(result, summary_oos_evals)) %>%
  mutate(plot_oos_evals = map2(result, name, ~ plot_oos_evals(.x) + ggtitle(.y))) %>%
  mutate(plot_components = map2(latest, name, ~ plot_components(.x) + ggtitle(.y))) %>%
  mutate(plot_final_series_dygraph = map2(latest, name, ~ plot_final_series_dygraph(.x, main = .y)))

overview <-
  models %>%
  select(name, smry) %>%
  unnest(cols = c(smry)) %>%
  select(name, period, mrse) %>%
  pivot_wider(values_from = "mrse", names_from = "name")

ref <- pull(overview, 2)
overview_normalized <-
  overview %>%
  mutate_at(vars(-period), ~ . / !! ref)



#' ## Overview


#+ echo = FALSE, warning = FALSE

kable(overview_normalized, digits = 2)

#' ## OOS Plots

#+ echo = FALSE, warning = FALSE

walk(models$plot_oos_evals, print)


#' ## Component Plots

#+ echo = FALSE, warning = FALSE

walk(models$plot_components, print)


#' ## Final Adjusted Series

#+ echo = FALSE, warning = FALSE

htmltools::tagList(models$plot_final_series_dygraph)








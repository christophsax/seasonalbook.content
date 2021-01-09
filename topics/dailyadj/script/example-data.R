library(tidyverse)
library(tsbox)

# transact, universal example series
# transact <- read_csv("data/data.csv", col_types = cols())
# save(transact, file = "data/transact.RData")


# trendecon --------------------------------------------------------------------

# More example series from trendecon
trendecon <-
  read_csv("https://raw.githubusercontent.com/trendecon/data/master/raw/ch/trendecon_sa.csv", col_types = cols())

usethis::use_data(trendecon)


# NZ immigration data ----------------------------------------------------------

link <- "https://github.com/tuckermcelroy/sigex/blob/master/data/imm.RData?raw=true"
tf <- tempfile(fileext = ".RData")
download.file(link, tf)
env <- new.env()
load(tf, env)
env$imm

# https://github.com/tuckermcelroy/sigex/blob/master/tests/NZ/NZ.R
# Start Date, according to L36:
# c(9,1,1997)
# Col Nmaes, according to L49:
# c("NZArr","NZDep","VisArr","VisDep","PLTArr","PLTDep")

library(tidyverse)
raw <-
  env$imm %>%
  as.data.frame() %>%
  as_tibble() %>%
  setNames(c("NZArr","NZDep","VisArr","VisDep","PLTArr","PLTDep")) %>%
  # what is PLT?
  transmute(
    arr = NZArr,
    dep = NZDep,
    arr_visit = VisArr,
    dep_visit = NZDep
  )


all_months <- seq(as.Date("1997-09-01"), length.out = 179, by = "month")

time_all <- function(time_first, nrow) seq(as.Date(time_first), length.out = nrow, by = "day")

nzimmigration_wide <-
  raw %>%
  mutate(is_total = arr > 30000) %>%
  mutate(month_id = cumsum(is_total)) %>%
  filter(!is_total) %>%
  select(-is_total) %>%
  filter(arr > 0) %>%
  nest_by(month_id) %>%
  ungroup() %>%
  mutate(time_first = all_months) %>%
  rowwise() %>%
  mutate(time = Map(time_all, time_first = time_first, nrow = nrow(data))) %>%
  unnest(cols = c(data, time)) %>%
  select(-month_id, -time_first) %>%
  relocate(time, 1)

stopifnot(identical(1, unique(diff(dta$time))))

library(tsbox)
nzimmigration <- ts_long(nzimmigration_wide)

usethis::use_data(nzimmigration)



library(tidyverse)
library(tsbox)

# transact, universal example series
# transact <- read_csv("data/data.csv", col_types = cols())
# save(transact, file = "data/transact.RData")

# More example series from trendecon
trendecon <-
  read_csv("https://raw.githubusercontent.com/trendecon/data/master/raw/ch/trendecon_sa.csv", col_types = cols())

usethis::use_data(trendecon)

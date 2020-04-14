library(tidyverse)
library(tsbox)

# transact, universal example series
# transact <- read_csv("data/data.csv", col_types = cols())
# save(transact, file = "data/transact.RData")

# More example series from trendecon
trendecon <-
  read_csv("https://raw.githubusercontent.com/trendecon/data/master/daily/trendecon_keywords.csv", col_types = cols()) %>%
  filter(id == "orig") %>%
  select(-id)



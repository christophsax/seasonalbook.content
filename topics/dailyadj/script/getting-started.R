library(tidyverse)
library(tsbox)

snb_main <- read_csv("data/data.csv", col_types = cols())

# More example series from trendecon
trendecon <-
  read_csv("https://raw.githubusercontent.com/trendecon/data/master/daily/trendecon_keywords.csv", col_types = cols()) %>%
  filter(id == "orig") %>%
  select(-id)



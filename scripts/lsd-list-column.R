# Create and manipulate list column of lsd

library(tidyverse)
source("scripts/functions/functions.R")

transactions <- read_csv("data/transactions.csv")

# Create list column
trans_list <- transactions %>% mutate(data = pmap(select(., l, s, d), c))
trans_nest <- transactions %>% nest(l, s, d)

# Group_by and summarise list column
trans_sum <- trans_list %>% 
  group_by(credit, debit) %>% 
  summarise(sum = list(reduce(data, `+`)))

# Take vector of length three with l, s, and d values and refactors to correct limit of 20s and 12d
deb_refactor_vector <- function(x) {
   temp <- c(
      x[1] + ((x[2] + x[3] %/% 12) %/% 20),
      (x[2] + x[3] %/% 12) %% 20,
      x[3] %% 12)
   names(temp) <- c("l", "s", "d")
   temp
}

# Group_by and summarise list column with refactor
trans_sum_refactor <- trans_list %>% 
  group_by(from, to) %>% 
  summarise(sum = list(deb_refactor_vector(reduce(data, `+`))))

# lsd vector to denarii
deb_lsd_d_vector <- function(x) {
  x[1] * 240 + x[2] * 12 + x[3]
}

trans_sum_refactor2 <- trans_sum_refactor %>% 
  mutate(denarii = map(sum, deb_lsd_d_vector)) %>% 
  unnest(denarii)

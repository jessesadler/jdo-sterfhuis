## Basic Analysis ##

library(tidyverse)

# Load data
transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d")))
accounts <- read_csv("data/accounts.csv")

# simplify
transactions <- transactions %>% select(from, to, gr:pounds)

### Decimal vlams ###

transactions <- mutate(transactions, vlams_dec = gr + sc / 20 + d / 240)


credit <- transactions %>% group_by(from) %>% summarise(credit = sum(vlams_dec))
debit <- transactions %>% group_by(to) %>% summarise(debit = sum(vlams_dec))

accounts_sum <- full_join(credit, debit, by = c("from" = "to")) %>% 
  replace_na(list(credit = 0, debit = 0)) %>% 
  rename(id = from)

accounts_sum <- accounts_sum %>% mutate(current = credit - debit) %>% 
  arrange(current)

open <- filter(accounts_sum, current >= 1/240 | current <= -1/240)
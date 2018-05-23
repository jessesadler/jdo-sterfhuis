## Clean data ##

library(tidyverse)

## Transactions ##

transactions <- read_csv("data-raw/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d"))) %>% 
  select(from:to, date:denarii) %>% 
  rename(l = librae, s = solidi, d = denarii)

write_csv(transactions, "data/transactions.csv")


## Accounts ##
accounts <- read_csv("data-raw/accounts.csv") %>% 
  select(id, account:location) %>% 
  rowid_to_column("id_int")

write_csv(accounts, "data/accounts.csv")

## Clean data ##

library(tidyverse)

## Transactions ##
transactions <- read_csv("data-raw/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d"))) %>% 
  select(credit:debit, date:denarii) %>% 
  rename(l = librae, s = solidi, d = denarii)

write_csv(transactions, "data/transactions.csv")

## Accounts ##
accounts <- read_csv("data-raw/accounts.csv") %>% 
  select(id, account:location) %>% 
  rowid_to_column("id_int")

write_csv(accounts, "data/accounts.csv")

accounts_id <- select(accounts, id, id_int)

## Transactions with integer ids ##
transactions_int <- transactions %>% 
  left_join(accounts_id, by = c("credit" = "id")) %>% 
  left_join(accounts_id, by = c("debit" = "id")) %>% 
  select(-credit, -debit) %>% 
  rename(credit = id_int.x, debit = id_int.y) %>% 
  select(credit, debit, everything())

write_csv(transactions_int, "data/transactions_int.csv")

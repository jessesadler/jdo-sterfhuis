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

## Transactions with integer ids ##
accounts_id <- select(accounts, id, id_int)

transactions_int <- transactions %>% 
  left_join(accounts_id, by = c("credit" = "id")) %>% 
  left_join(accounts_id, by = c("debit" = "id")) %>% 
  select(-credit, -debit) %>% 
  rename(credit = id_int.x, debit = id_int.y) %>% 
  select(credit, debit, everything())

write_csv(transactions_int, "data/transactions_int.csv")

## Transactions and accounts by group ##

# Accounts
accounts <- read_csv("data-raw/accounts.csv") %>% 
  select(account_id = id, account:location)

groups <- distinct(accounts, group) %>% 
  rowid_to_column("id")

accounts_group <- left_join(groups, accounts, by = "group") %>% 
  distinct(group, .keep_all = TRUE)

# Transactions
# Get the group id for each account
accounts_group_id <- left_join(accounts, groups, by = "group") %>% 
  select(id, account_id, group)

transactions_group <- transactions %>% 
  left_join(accounts_group_id, by = c("credit" = "account_id")) %>% 
  left_join(accounts_group_id, by = c("debit" = "account_id")) %>% 
  select(-credit, -debit) %>% 
  rename(credit = id.x, debit = id.y) %>% 
  select(credit, debit, date, l:d)

write_csv(accounts_group, "data/accounts_group.csv")
write_csv(transactions_group, "data/transactions_group.csv")

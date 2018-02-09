### Summarise transactions by group variable in accounts ###

library(tidyverse)
source("scripts/functions.R")

transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d"))) %>% 
  select(from:to, date:denari) %>% 
  rename(l = livre, s = solidi, d = denari)
accounts <- read_csv("data/accounts.csv") %>% 
  select(id, account:location)

account_groups <- accounts %>%
  select(id, group) %>% 
  distinct(group, .keep_all = TRUE) %>% 
  rowid_to_column("group_id")

trans_group <- transactions %>% 
  left_join(account_groups, by = c("from" = "id")) %>% 
  left_join(account_groups, by = c("to" = "id")) %>% 
  rename(group_cred = group.x, group_cred_id = group_id.x,
         group_deb = group.y, group_deb_id = group_id.y)

trans_group_sum <- deb_group_sum(trans_group, credit = group_cred_id, debit = group_deb_id)
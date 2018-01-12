### Summarise transactions by group variable in accounts ###

library(tidyverse)
source("scripts/functions.R")

account_groups <- accounts %>% select(id, group)
account_groups <- account_groups %>% 
  add_column(group_id = 1:nrow(account_groups))

distinct_groups <- distinct(account_groups, group, .keep_all = TRUE)

trans_group <- transactions %>% 
  left_join(account_groups, by = c("from" = "id")) %>% 
  left_join(account_groups, by = c("to" = "id"))

trans_group_sum <- deb_group_sum(trans_group, credit = group.x, debit = group.y)


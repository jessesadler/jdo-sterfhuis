### Subset of transactions for single and multiple accounts ###

# This script led to creation of ded_sub_credit and debit functions
# It uses the structure of the function and the function itself to get
# subsets of transactions for significant accounts. The function creates
# a percentage column to show percentages for each transaction.

# Get subset of transactions with single and multiple accounts

library(tidyverse)
library(stringr)

# Load data
transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d"))) %>% 
  select(from:to, date:denari) %>% 
  rename(l = livre, s = solidi, d = denari)
accounts <- read_csv("data/accounts.csv") %>% 
  select(id, account:location)

# Data frame with account ids and names to use for joins. Necessary for functions.
account_names <- select(accounts, id, account)

# Summary of transactions
# This can be used instead of transactions if looking at aggregate of transactions
transactions_sum <- deb_sum_df(transactions)

### Sum of credit and debit for the accounts ###
# This only uses pounds for percentage, because shilling and pence not significant
# Takes out dfl12_001, since this was not a real account
# It is a good way to show the most significant accounts
accounts_sum <- deb_current(transactions)
accounts_sum <- left_join(account_names, accounts_sum, by = "id") %>% 
  select(id:l_c, l_d, relation, l) %>% 
  filter(id != "dfl12_001") %>% 
  mutate(pct_c = round(l_c*100/sum(l_c), 4), 
         pct_d = round(l_d*100/sum(l_d), 4)) %>% 
  arrange(desc(l_c))

### Workflow for single account ###
book <- filter(transactions, from == "dfl12_289" | to == "dfl12_289")
book_cred <- transactions %>% 
  filter(from == "dfl12_289") %>% 
  left_join(account_names, by = c("to" = "id")) %>% 
  select(from:to, account, l:d, everything()) %>% 
  mutate(denari = deb_lsd_d(l, s, d), 
         pct = round(denari*100/sum(denari), 2)) %>% 
  arrange(desc(l))
book_deb <- transactions %>% 
  filter(to == "dfl12_289") %>% 
  left_join(account_names, by = c("from" = "id")) %>% 
  select(from:to, account, l:d, everything()) %>% 
  mutate(denari = deb_lsd_d(l, s, d), 
         pct = round(denari*100/sum(denari), 2)) %>% 
  arrange(desc(l))

### Bequests ###
# This uses accounts creditor to dfl12_151
bequests <- transactions %>% 
  filter(to == "dfl12_151") %>% 
  left_join(account_names, by = c("from" = "id")) %>% 
  select(from:to, account, l:d, everything()) %>% 
  mutate(denari = deb_lsd_d(l, s, d), 
         pct = round(denari*100/sum(denari), 2)) %>% 
  arrange(desc(denari))

### Open accounts at end of 1594 ###
open <- transactions %>% 
  deb_open() %>% 
  left_join(account_names, by = "id") %>% 
  select(id, account, everything()) %>% 
  arrange(desc(l))
open_cred <- filter(open, relation == "credit") %>% 
  mutate(denari = deb_lsd_d(l, s, d), 
         pct = round(denari*100/sum(denari), 2))
open_deb <- filter(open, relation == "debit") %>% 
  mutate(denari = deb_lsd_d(l, s, d), 
         pct = round(denari*100/sum(denari), 2))

### Accounts through functions that do same as above ###

### Balance on 8 November 1582 ###
balance_8_nov_sum <- deb_account(transactions, "dfl12_001")
balance_8_nov <- transactions %>% 
  filter(from == "dfl12_001" | to == "dfl12_001") %>% 
  arrange(desc(l))
balance_8_nov_cred <- deb_sub_credit(transactions, "dfl12_001")
balance_8_nov_deb <- deb_sub_debit(transactions, "dfl12_001")

### Winninge ende verlies ###
winninge_verlies <- filter(transactions, from == "dfl12_038" | to == "dfl12_038")
winninge_verlies_cred <- deb_sub_credit(transactions, "dfl12_038")
winninge_verlies_deb <- deb_sub_debit(transactions, "dfl12_038")

### Branches ###
verona_accounts <- c("dfl12_110", "dfl12_446")
venice_accounts <- c("dfl12_111", "dfl12_181")
london_accounts <- c("dfl12_112", "dfl12_446")

verona <- filter(transactions, from %in% verona_accounts | to %in% verona_accounts)
verona_cred <- deb_sub_credit(transactions, verona_accounts)
verona_deb <- deb_sub_debit(transactions, verona_accounts)  

venice <- filter(transactions, from %in% venice_accounts | to %in% venice_accounts)
venice_cred <- deb_sub_credit(transactions_sum, venice_accounts)
venice_deb <- deb_sub_debit(transactions_sum, venice_accounts)

london <- filter(transactions, from %in% london_accounts | to %in% london_accounts)
london_cred <- deb_sub_credit(transactions, london_accounts)
london_deb <- deb_sub_debit(transactions, london_accounts)

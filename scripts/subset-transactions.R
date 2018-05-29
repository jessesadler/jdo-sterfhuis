### Subset of transactions for single and multiple accounts ###

# This script led to creation of ded_account_credit and debit functions
# It uses the structure of the function and the function itself to get
# subsets of transactions for significant accounts. The function creates
# a percentage column to show percentages for each transaction.

# Get subset of transactions with single and multiple accounts

library(tidyverse)
library(stringr)
library(debkeepr)
source("scripts/credit-debit-functions.R")

# Load data
transactions <- read_csv("data/transactions.csv")
accounts <- read_csv("data/accounts.csv")

# Data frame with account ids and names to use for joins. Necessary for functions.
account_names <- select(accounts, id, account)

# Summary of transactions
# This can be used instead of transactions if looking at aggregate of transactions
transactions_sum <- transactions %>% 
  group_by(credit, debit) %>% 
  deb_sum(l, s, d) %>% 
  mutate(pounds = deb_lsd_l(l, s, d))

### Sum of credit and debit for the accounts ###
# This only uses pounds for percentage, because shilling and pence not significant
# Takes out dfl12_001, since this was not a real account
# It is a good way to show the most significant accounts

debit <- deb_debit(transactions) %>% 
  filter(account_id != "dfl12_001") %>% 
  mutate(denarii = deb_lsd_d(l, s, d),
         pct = denarii * 100 /sum(denarii)) %>% 
  add_column(relation = "debit", .after = 1) %>% 
  left_join(account_names, by = c("account_id" = "id")) %>% 
  select(account_id, account, everything()) %>% 
  arrange(desc(pct))

credit <- deb_credit(transactions) %>% 
  filter(account_id != "dfl12_001") %>% 
  mutate(denarii = deb_lsd_d(l, s, d),
         pct = denarii * 100 /sum(denarii)) %>% 
  add_column(relation = "credit", .after = 1) %>% 
  left_join(account_names, by = c("account_id" = "id")) %>% 
  select(account_id, account, everything()) %>% 
  arrange(desc(pct))

accounts_sum <- bind_rows(debit, credit) %>% 
  arrange(desc(pct))

### Workflow for single account ###
book <- filter(transactions, credit == "dfl12_289" | debit == "dfl12_289")
book_cred <- transactions %>% 
  filter(credit == "dfl12_289") %>% 
  left_join(account_names, by = c("debit" = "id")) %>% 
  select(credit:debit, account, l:d, everything()) %>% 
  mutate(denarii = deb_lsd_d(l, s, d), 
         pct = round(denari*100/sum(denarii), 2)) %>% 
  arrange(desc(l))
book_deb <- transactions %>% 
  filter(debit == "dfl12_289") %>% 
  left_join(account_names, by = c("credit" = "id")) %>% 
  select(credit:debit, account, l:d, everything()) %>% 
  mutate(denarii = deb_lsd_d(l, s, d), 
         pct = round(denari*100/sum(denarii), 2)) %>% 
  arrange(desc(l))

### Bequests ###
# This uses accounts creditor to dfl12_151
bequests <- transactions %>% 
  filter(debit == "dfl12_151") %>% 
  left_join(account_names, by = c("credit" = "id")) %>% 
  select(credit, account, date, l:d, everything()) %>% 
  mutate(denarii = deb_lsd_d(l, s, d), 
         pct = round(denarii*100/sum(denarii), 2)) %>% 
  arrange(desc(denarii))

### Open accounts at end of 1594 ###
open <- transactions %>% 
  deb_open() %>% 
  left_join(account_names, by = c("account_id" = "id")) %>% 
  select(account_id, account, everything())

open_credit <- open %>% 
  mutate(denarii = deb_lsd_d(l, s, d)) %>% 
  filter(denarii > 0) %>% 
  mutate(pct = round(denarii*100/sum(denarii), 2)) %>% 
  arrange(desc(denarii))
  
open_debit <- open %>% 
  mutate(denarii = deb_lsd_d(l, s, d)) %>% 
  filter(denarii < 0) %>% 
  mutate(pct = round(denarii*100/sum(denarii), 2)) %>% 
  arrange(denarii)

### Accounts through functions that do same as above ###

### Balance on 8 November 1582 ###
balance_8_nov_sum <- deb_account(transactions, "dfl12_001")
balance_8_nov <- transactions %>% 
  filter(credit == "dfl12_001" | credit == "dfl12_001") %>% 
  arrange(desc(l))
balance_8_nov_cred <- deb_account_credit(transactions, "dfl12_001") %>% 
  left_join(account_names, by = c("debit" = "id")) %>% 
  select(debit, date, account, everything())
balance_8_nov_deb <- deb_account_debit(transactions, "dfl12_001") %>% 
  left_join(account_names, by = c("credit" = "id")) %>% 
  select(credit, date, account, everything())

### Winninge ende verlies ###
winninge_verlies <- filter(transactions, credit == "dfl12_038" | credit == "dfl12_038")
winninge_verlies_cred <- deb_account_credit(transactions, "dfl12_038")
winninge_verlies_deb <- deb_account_debit(transactions, "dfl12_038")

### Branches ###
verona_accounts <- c("dfl12_110", "dfl12_446")
venice_accounts <- c("dfl12_111", "dfl12_181")
london_accounts <- c("dfl12_112", "dfl12_446")

verona <- filter(transactions, credit %in% verona_accounts | credit %in% verona_accounts)
verona_cred <- deb_account_credit(transactions, verona_accounts)
verona_deb <- deb_account_debit(transactions, verona_accounts)  

venice <- filter(transactions, credit %in% venice_accounts | credit %in% venice_accounts)
venice_cred <- deb_account_credit(transactions_sum, venice_accounts)
venice_deb <- deb_account_debit(transactions_sum, venice_accounts)

london <- filter(transactions, credit %in% london_accounts | credit %in% london_accounts)
london_cred <- deb_account_credit(transactions, london_accounts)
london_deb <- deb_account_debit(transactions, london_accounts)

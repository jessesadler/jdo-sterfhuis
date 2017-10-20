### Inheritance accounts ###

# Script to get the ids for different types of inheritance accounts

library(tidyverse)
library(stringr)

# Load data
transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d"))) %>% 
  select(from:to, date:denari) %>% 
  rename(l = livre, s = solidi, d = denari)
accounts <- read_csv("data/accounts.csv") %>% 
  select(id, account:location)

# Quickest way is to get ids for accounts under type inheritance
inheritance_accounts <- filter(accounts, type == "Inheritance") %>% 
  select(id) %>% flatten() %>% as_vector()

# The above does not distinguish between the different kinds of inheritance accounts
# Script to divide inheritance accounts between maternal, paternal, sororal, etc.

# Maternal and paternal
maternal_accounts <- c("dfl12_251", "dfl12_252", "dfl12_253", "dfl12_321", "dfl12_333", "dfl12_295")
paternal_accounts <- c("dfl12_340", "dfl12_341", "dfl12_342", "dfl12_343", "dfl12_344", "dfl12_345", 
                       "dfl12_346", "dfl12_347", "dfl12_348", "dfl12_352")

# Sororal
# This includes sororal accounts of the heirs and the account of the capital of Cornelia,
# which aggregates Cornelia's estate but is neither her maternal nor paternal account
accounts$sororal <- str_detect(accounts$account, "sororal")
sororal_accounts <- filter(accounts, sororal == TRUE | id == "dfl12_350") %>% 
  select(id) %>% flatten() %>% as_vector()

# London inheritance that is split between Jacques, Marten, and children of Maria
london_inheritance_accounts <- c("dfl12_478", "dfl12_479", "dfl12_489")

# Anna de Hane's estate
# This also includes dfl12_281, which deals with lawsuit against De Hane.
# dfl12_281 is not included in inheritance accounts
anna_de_hane_accounts <- filter(accounts, inheritance == "Anna de Hane") %>% 
  select(id) %>% flatten() %>% as_vector()

# Miscellaneous account of children of Maria's part in bequest to Goyvaerts
miscellaneous_account <- c("dfl12_286")

inheritance <- c(maternal_accounts, paternal_accounts, 
                 sororal_accounts, london_inheritance_accounts, 
                 anna_de_hane_accounts, miscellaneous_account)

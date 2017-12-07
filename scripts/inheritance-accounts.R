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
# These are not included in the inheritance accounts
anna_de_hane_accounts <- filter(accounts, group == "Anna de Hane") %>% 
  select(id) %>% flatten() %>% as_vector()

inheritance <- c(maternal_accounts, paternal_accounts, 
                 sororal_accounts)

### Individual inheritance accounts ###
# See aggregate-inheritance-accounts.R for similar scripts

inheritance_accounts_tbl <- filter(accounts, type == "Inheritance")

anna_accounts <- filter(inheritance_accounts_tbl, group == "Anna & Robert") %>% 
  select(id) %>% flatten() %>% as_vector()

jan_accounts <- filter(inheritance_accounts_tbl, group == "Jan") %>% 
  select(id) %>% flatten() %>% as_vector()

marten_accounts <- filter(inheritance_accounts_tbl, group == "Marten") %>% 
  select(id) %>% flatten() %>% as_vector()

maria_accounts <- filter(inheritance_accounts_tbl, group == "Maria") %>% 
  select(id) %>% flatten() %>% as_vector()

carlo_accounts <- filter(inheritance_accounts_tbl, group == "Carlo") %>% 
  select(id) %>% flatten() %>% as_vector()

jacques_accounts <- filter(inheritance_accounts_tbl, group == "Jacques") %>% 
  select(id) %>% flatten() %>% as_vector()

steven_accounts <- filter(inheritance_accounts_tbl, group == "Steven") %>% 
  select(id) %>% flatten() %>% as_vector()

hester_accounts <- filter(inheritance_accounts_tbl, group == "Hester") %>% 
  select(id) %>% flatten() %>% as_vector()

cornelia_accounts <- filter(inheritance_accounts_tbl, group == "Cornelia") %>% 
  select(id) %>% flatten() %>% as_vector()

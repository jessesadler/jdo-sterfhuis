## Aggregate Accounts of heirs and inheritance ##

# Can use this to aggregate either all accounts for each heir or only the inheritance accounts
# The difference is whether to filter accounts to only inheritance accounts at the beginning or not

library(stringr)

### Accounts of all the heirs ###
heir_groups <- c("Anna & Robert", "Jan", "Marten", "Maria","Carlo", "Jacques",
                 "Steven", "Hester", "Cornelia", "Hester and Cornelia")

# This way includes Robert van Eeckeren's accounts
heir_accounts <- filter(accounts, group %in% heir_groups) %>% 
  select(id) %>% flatten() %>% as_vector()

# Alternative way does not include Robert's accounts
heir_accounts <- filter(accounts, type == "Inheritance" | type == "Heir") %>% 
  select(id) %>% flatten() %>% as_vector()

### Inheritance Accounts ###
# Can run this and use it in place of accounts tbl
inheritance_accounts_tbl <- filter(accounts, type == "Inheritance")

### Individual Heirs ###

anna_accounts <- filter(accounts, group == "Anna & Robert") %>% 
  select(id) %>% flatten() %>% as_vector()
anna_replace <- set_names(replicate(length(anna_accounts), "dfl12_251"), anna_accounts)

jan_accounts <- filter(accounts, group == "Jan") %>% 
  select(id) %>% flatten() %>% as_vector()
jan_replace <- set_names(replicate(length(jan_accounts), "dfl12_287"), jan_accounts)

marten_accounts <- filter(accounts, group == "Marten") %>% 
  select(id) %>% flatten() %>% as_vector()
marten_replace <- set_names(replicate(length(marten_accounts), "dfl12_295"), marten_accounts)

maria_accounts <- filter(accounts, group == "Maria") %>% 
  select(id) %>% flatten() %>% as_vector()
maria_replace <- set_names(replicate(length(maria_accounts), "dfl12_351"), maria_accounts)

carlo_accounts <- filter(accounts, group == "Carlo") %>% 
  select(id) %>% flatten() %>% as_vector()
carlo_replace <- set_names(replicate(length(carlo_accounts), "dfl12_285"), carlo_accounts)

jacques_accounts <- filter(accounts, group == "Jacques") %>% 
  select(id) %>% flatten() %>% as_vector()
jacques_replace <- set_names(replicate(length(jacques_accounts), "dfl12_102"), jacques_accounts)

steven_accounts <- filter(accounts, group == "Steven") %>% 
  select(id) %>% flatten() %>% as_vector()
steven_replace <- set_names(replicate(length(steven_accounts), "dfl12_333"), steven_accounts)

hester_accounts <- filter(accounts, group == "Hester") %>% 
  select(id) %>% flatten() %>% as_vector()
hester_replace <- set_names(replicate(length(hester_accounts), "dfl12_252"), hester_accounts)

cornelia_accounts <- filter(accounts, group == "Cornelia") %>% 
  select(id) %>% flatten() %>% as_vector()
cornelia_replace <- set_names(replicate(length(cornelia_accounts), "dfl12_253"), cornelia_accounts)

anna_de_hane_accounts <- filter(accounts, group == "Anna de Hane") %>% 
  select(id) %>% flatten() %>% as_vector()
anna_de_hane_replace <- set_names(replicate(length(anna_de_hane_accounts), "dfl12_310"), anna_de_hane_accounts)

# Replace ids in transactions
transactions$credit <- str_replace_all(transactions$credit, anna_replace)
transactions$credit <- str_replace_all(transactions$credit, jan_replace)
transactions$credit <- str_replace_all(transactions$credit, marten_replace)
transactions$credit <- str_replace_all(transactions$credit, maria_replace)
transactions$credit <- str_replace_all(transactions$credit, carlo_replace)
transactions$credit <- str_replace_all(transactions$credit, jacques_replace)
transactions$credit <- str_replace_all(transactions$credit, steven_replace)
transactions$credit <- str_replace_all(transactions$credit, hester_replace)
transactions$credit <- str_replace_all(transactions$credit, cornelia_replace)
transactions$credit <- str_replace_all(transactions$credit, anna_de_hane_replace)
  
transactions$debit <- str_replace_all(transactions$debit, anna_replace)
transactions$debit <- str_replace_all(transactions$debit, jan_replace)
transactions$debit <- str_replace_all(transactions$debit, marten_replace)
transactions$debit <- str_replace_all(transactions$debit, maria_replace)
transactions$debit <- str_replace_all(transactions$debit, carlo_replace)
transactions$debit <- str_replace_all(transactions$debit, jacques_replace)
transactions$debit <- str_replace_all(transactions$debit, steven_replace)
transactions$debit <- str_replace_all(transactions$debit, hester_replace)
transactions$debit <- str_replace_all(transactions$debit, cornelia_replace)
transactions$debit <- str_replace_all(transactions$debit, anna_de_hane_replace)

## Explanation with one account ##

# Create a vector of the accounts for the heir
hester_accounts <- accounts %>% 
  filter(inheritance == "Hester") %>% 
  select(id) %>% flatten() %>% as_vector()

# Take vector and create named vector to use to replace account ids with one id
# Vector is of replacement id and names are ids to be replaced
# Two ways to do it: pick specific id to use as replacement or use first id in vector
hester_replace <- set_names(replicate(length(hester_accounts), "dfl12_252"), hester_accounts)
hester_replace <- set_names(replicate(length(hester_accounts), hester_accounts[1]), hester_accounts)

# Create list of transactions
hester_transactions <- transactions %>%
  filter(to %in% hester_accounts | credit %in% hester_accounts) %>% 
  ungroup()

hester_transactions$debit <- str_replace_all(hester_transactions$debit, hester_replace)
hester_transactions$credit <- str_replace_all(hester_transactions$credit, hester_replace)
hester_transactions <- deb_group_sum(hester_transactions)

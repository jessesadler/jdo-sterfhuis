## Aggregate Accounts of heirs and inheritance ##

# Can use this to aggregate either all accounts for each heir or only the inheritance accounts
# The difference is whether to filter accounts to only inheritance accounts at the beginning or not
# First part fge

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
transactions$from <- str_replace_all(transactions$from, anna_replace)
transactions$from <- str_replace_all(transactions$from, jan_replace)
transactions$from <- str_replace_all(transactions$from, marten_replace)
transactions$from <- str_replace_all(transactions$from, maria_replace)
transactions$from <- str_replace_all(transactions$from, carlo_replace)
transactions$from <- str_replace_all(transactions$from, jacques_replace)
transactions$from <- str_replace_all(transactions$from, steven_replace)
transactions$from <- str_replace_all(transactions$from, hester_replace)
transactions$from <- str_replace_all(transactions$from, cornelia_replace)
transactions$from <- str_replace_all(transactions$from, anna_de_hane_replace)
  
transactions$to <- str_replace_all(transactions$to, anna_replace)
transactions$to <- str_replace_all(transactions$to, jan_replace)
transactions$to <- str_replace_all(transactions$to, marten_replace)
transactions$to <- str_replace_all(transactions$to, maria_replace)
transactions$to <- str_replace_all(transactions$to, carlo_replace)
transactions$to <- str_replace_all(transactions$to, jacques_replace)
transactions$to <- str_replace_all(transactions$to, steven_replace)
transactions$to <- str_replace_all(transactions$to, hester_replace)
transactions$to <- str_replace_all(transactions$to, cornelia_replace)
transactions$to <- str_replace_all(transactions$to, anna_de_hane_replace)

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
  filter(to %in% hester_accounts | from %in% hester_accounts) %>% 
  ungroup()

hester_transactions$to <- str_replace_all(hester_transactions$to, hester_replace)
hester_transactions$from <- str_replace_all(hester_transactions$from, hester_replace)
hester_transactions <- deb_group_sum(hester_transactions)

### Creating subgraphs ###

## Create subgraph from type of account ##

# Simplify nodes and edges data frames
transactions_simple <- select(transactions, from, to, gr:d)
accounts_simple <- select(accounts, id, account_name_desc, ac_type)

wissel_accounts <- filter(accounts_simple, ac_type == "Wissel")
wissel_transactions <- transactions_simple %>%
  filter(to %in% wissel_accounts$id | from %in% wissel_accounts$id)

# Recreate node list, if desired
from <- wissel_transactions %>% 
  distinct(from) %>%
  rename(id = from)

to <- wissel_transactions %>% 
  distinct(to) %>%
  rename(id = to)

wissel_nodes <- full_join(from, to)

# Create igraph object
# Creates vertices from wissels_transactions data
wissels <- graph_from_data_frame(wissel_transactions)

### Creating subgraphs with example of inheritance accounts###

# Load data
transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d")))
accounts <- read_csv("data/accounts.csv")

## Create subgraph from type of account ##

# Sum of transactions
transactions <- transactions %>% 
  group_by(from, to) %>% 
  summarise(l = sum(livre) + ((sum(solidi) + (sum(denari) %/% 12)) %/% 20),
            s = (sum(solidi) + (sum(denari) %/% 12)) %% 20,
            d = sum(denari) %% 12)

# Select accounts and transactions desired
inheritance_accounts <- filter(accounts, type == "Inheritance")
inheritance_transactions <- transactions %>%
  filter(to %in% inheritance_accounts$id | from %in% inheritance_accounts$id) %>% 
  ungroup() # Need to ungroup to be able to get 

# Recreate node list and get subgroup of accounts
from <- inheritance_transactions %>% 
  distinct(from) %>%
  rename(id = from)

to <- inheritance_transactions %>% 
  distinct(to) %>%
  rename(id = to)

inheritance_nodes <- full_join(from, to)

inheritance_nodes <- filter(accounts, id %in% inheritance_nodes$id)

# Create igraph object
# Creates vertices from inheritances_transactions data
inheritance <- graph_from_data_frame(d = inheritance_transactions, vertices = inheritance_nodes, directed = TRUE)

ggraph(inheritance, layout = "kk") + 
  geom_edge_link(aes(alpha = l)) + 
  geom_node_point(aes(color = inheritance), alpha = 0.7) + 
  labs(title = "Accounts of the estate of a 16th-century merchant")


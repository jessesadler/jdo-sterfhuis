### Creating subgraphs with example of inheritance accounts###

# Load data
transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d")))
accounts <- read_csv("data/accounts.csv")

## Create subgraph from type of account ##

# Sum of transactions
transactions <- deb_sum_df(transactions)

# Select accounts and transactions desired
inheritance_accounts <- filter(accounts, type == "Inheritance" | type == "Heir")
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
  geom_node_point(aes(color = inheritance)) + 
  labs(title = "Accounts of the estate of a 16th-century merchant")


### Subgraph through a single account ###

# Sum of transactions
transactions_sum <- transactions %>% 
  group_by(from, to) %>% 
  summarise(l = sum(livre) + ((sum(solidi) + (sum(denari) %/% 12)) %/% 20),
            s = (sum(solidi) + (sum(denari) %/% 12)) %% 20,
            d = sum(denari) %% 12)

hester_accounts <- accounts %>% 
  filter(inheritance == "Hester") %>% 
  select(id) %>% flatten() %>% as_vector()

hester_transactions <- transactions_sum %>%
  filter(to %in% hester_accounts | from %in% hester_accounts) %>% 
  ungroup()

# Recreate node list and get subgroup of accounts
from <- hester_transactions %>% 
  distinct(from) %>%
  rename(id = from)

to <- hester_transactions %>% 
  distinct(to) %>%
  rename(id = to)

hester_nodes <- full_join(from, to)
hester_nodes <- filter(accounts, id %in% hester_nodes$id)

# Create igraph object
# Creates vertices from hester_transactions data
hester <- graph_from_data_frame(d = hester_transactions, vertices = hester_nodes, directed = TRUE)

ggraph(hester, layout = "kk") + 
  geom_edge_fan(aes(alpha = l), 
                 arrow = arrow(length = unit(3, 'mm')), 
                 end_cap = circle(2, 'mm')) + 
  geom_node_point(alpha = 0.7) + 
  geom_node_text(aes(label = account), repel = TRUE) +
  labs(title = "Accounts of the estate of a 16th-century merchant")

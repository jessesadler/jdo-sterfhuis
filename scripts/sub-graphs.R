### Creating subgraphs with example of inheritance accounts###

library(tidyverse)
library(igraph)
library(ggraph)

# Load data
transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d"))) %>% 
  select(from:denari, tr_type) %>% 
  rename(l = livre, s = solidi, d = denari)
accounts <- read_csv("data/accounts.csv") %>% 
  select(id, account:location)

### Subgraph through a single account ###

# Sum of transactions
transactions_sum <- deb_sum_df(transactions)

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
  geom_node_text(aes(label = account), repel = TRUE)

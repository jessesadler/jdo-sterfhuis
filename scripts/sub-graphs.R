### Creating subgraphs with example of inheritance accounts ###

library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(debkeepr)

# Load data
transactions <- read_csv("data/transactions.csv")
accounts <- read_csv("data/accounts.csv")

### Subgraph through a single account ###

# Sum of transactions
transactions_sum <- transactions %>% 
  group_by(credit, debit) %>% 
  deb_sum(l, s, d)

hester_accounts <- accounts %>% 
  filter(group == "Hester") %>% 
  select(id) %>% flatten() %>% as_vector()

hester_transactions <- transactions_sum %>%
  filter(debit %in% hester_accounts | credit %in% hester_accounts) %>% 
  ungroup() %>% 
  rename(from = credit, to = debit)

# Recreate node list and get subgroup of accounts
credit <- hester_transactions %>% 
  distinct(from) %>%
  rename(id = from)

debit <- hester_transactions %>% 
  distinct(to) %>%
  rename(id = to)

hester_nodes <- full_join(credit, debit, by = "id") %>% 
  left_join(accounts, by = c("id" = "id")) %>% 
  select(-id_int)

# Create igraph object: Need to go through igraph to have nodes and edges match up correctly
# Creates vertices from hester_transactions data
hester <- graph_from_data_frame(d = hester_transactions, vertices = hester_nodes, directed = TRUE)
hester_tbl <- as_tbl_graph(hester)

ggraph(hester, layout = "kk") + 
  geom_edge_fan(aes(alpha = l), 
                 arrow = arrow(length = unit(3, 'mm')), 
                 end_cap = circle(2, 'mm')) + 
  geom_node_point(alpha = 0.7) + 
  geom_node_text(aes(label = account), repel = TRUE) +
  theme_graph()


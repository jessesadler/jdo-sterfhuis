### Overview of the estate with ggraph ###

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

# Get dataframe of current value of accounts
# Select only pounds debit to be used for node size
nodes <- deb_current(transactions) %>% select(id, pounds = l_d)

# Sum of transactions between accounts
transactions <- deb_sum_df(transactions) %>% ungroup

# igraph object
sterfhuis <- graph_from_data_frame(d = transactions, vertices = nodes, directed = TRUE)

ggraph(sterfhuis, layout = "fr") + 
  geom_edge_fan(aes(edge_alpha = l)) + 
  scale_edge_alpha(labels = scales::dollar_format("£")) + 
  geom_node_point(aes(size = pounds), alpha = 0.7) + 
  scale_size_continuous(range = c(0.3, 6), labels = scales::dollar_format("£")) + 
  labs(size = "Accumulated Value", edge_alpha = "Transactions") + 
  theme_graph()

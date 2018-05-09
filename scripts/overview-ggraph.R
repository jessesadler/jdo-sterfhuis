### Overview of the estate with ggraph ###

library(tidyverse)
library(igraph)
library(ggraph)
source("scripts/functions.R")

# Load data
transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d"))) %>% 
  select(from:denari, tr_type) %>% 
  rename(l = librae, s = solidi, d = denarii)
accounts <- read_csv("data/accounts.csv") %>% 
  select(id, account:location)

# Get dataframe of current value of accounts
# Select only pounds debit to be used for node size
nodes <- deb_current(transactions) %>% 
  select(id, pounds = l_d) %>% 
  arrange(pounds)

# Take out balance on 8 November
# This leads to more accurate picture of relationships between accounts
nodes_alt <- deb_current(transactions) %>% 
  select(id, pounds = l_d) %>% 
  filter(id != "dfl12_001") %>% arrange(pounds)

# Sum of transactions between accounts
transactions_sum <- deb_group_sum(transactions) %>% ungroup()

# Take out transactions with balance
transactions_sum_alt <- transactions %>% 
  filter(from != "dfl12_001") %>% 
  filter(to != "dfl12_001") %>% 
  deb_group_sum() %>% ungroup()

# igraph object
sterfhuis <- graph_from_data_frame(d = transactions_sum, vertices = nodes, directed = TRUE)
sterfhuis_alt <- graph_from_data_frame(d = transactions_sum_alt, vertices = nodes_alt, directed = TRUE)

set_graph_style()

ggraph(sterfhuis, layout = "fr") + 
  geom_edge_fan(aes(edge_alpha = l)) + 
  scale_edge_alpha(labels = scales::dollar_format("£")) + 
  geom_node_point(aes(size = pounds), alpha = 0.7) + 
  scale_size_continuous(range = c(0.3, 6), labels = scales::dollar_format("£")) + 
  labs(size = "Accumulated Value", edge_alpha = "Transactions") + 
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))

ggsave("sterfhuis.png", width = 10, height = 8)
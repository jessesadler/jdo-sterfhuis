### Graph of accounts dealing with Heirs ###

library(tidyverse)
library(stringr)
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

## Create subset of transactions that deal directly with heirs
heirs_accounts <- filter(accounts, type == "Inheritance" | type == "Heir")
transactions <- transactions %>%
  filter(to %in% heirs_accounts$id | from %in% heirs_accounts$id)

## Aggregate accounts dealing with heirs
## Aggregate accounts: Estate, branches, winninge ende verlies

## Sum of transactions
transactions <- deb_group_sum(transactions) %>% ungroup

# Recreate accounts that are in transactions data frames
from <- transactions %>% 
  distinct(from) %>%
  rename(id = from)

to <- transactions %>% 
  distinct(to) %>%
  rename(id = to)

nodes <- full_join(from, to, by = "id")

nodes <- filter(accounts, id %in% nodes$id)


# Create igraph object
# Creates vertices from inheritances_transactions data
inheritance <- graph_from_data_frame(d = transactions, vertices = nodes, directed = TRUE)

ggraph(inheritance, layout = "dh") + 
  geom_edge_fan(aes(alpha = l)) + 
  geom_node_point(size = 2) + 
  theme_graph()

ggraph(inheritance, layout = "fr") + 
  geom_edge_fan(aes(alpha = l)) + 
  geom_node_point(aes(size = pounds)) + 
  scale_size_continuous(range = c(0.5, 5)) +
  theme_graph()

# Arc graph
ggraph(inheritance, layout = "linear") + 
  geom_edge_arc(aes(alpha = l)) + 
  geom_node_point(size = 2) +
  theme_graph()

# Circle graph
ggraph(inheritance, layout = "linear", circular = TRUE) + 
  geom_edge_arc(aes(alpha = l)) + 
  geom_node_point(size = 2) +
  theme_graph()

# Circle graph with alpha for direction
ggraph(inheritance, layout = "linear", circular = TRUE) + 
  geom_edge_arc(aes(color = l, alpha = ..index..)) + 
  scale_edge_alpha('Edge direction', guide = 'edge_direction') +
  geom_node_point(size = 2) +
  theme_graph()

## visNetwork
library(visNetwork)

nodes$group <- nodes$group
nodes$label <- nodes$label
nodes$title <- nodes$label

visNetwork(nodes, transactions) %>% 
  visIgraphLayout(layout = "layout_with_dh") %>% 
  visEdges(color = list(color = "grey", highlight = "purple")) %>% 
  visNodes(color = list(background = "grey", border = "grey", highlight = TRUE)) %>% 
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE)

visNetwork(nodes, transactions) %>% 
  visIgraphLayout(layout = "layout_in_circle") %>% 
  visEdges(arrows = "to", color = list(color = "grey", highlight = "purple")) %>% 
  visNodes(color = list(background = "grey", border = "grey", highlight = TRUE)) %>% 
  visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), nodesIdSelection = TRUE)

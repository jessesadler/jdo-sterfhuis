### Graph of accounts dealing with Heirs ###
# Use account and transactions groups

library(tidyverse)
library(stringr)
library(igraph)
library(ggraph)
library(debkeepr)

# Load data
transactions_group <- read_csv("data/transactions_group.csv")
accounts_group <- read_csv("data/accounts_group.csv")

## Create subset of transactions that deal directly with heirs
heirs_accounts <- filter(accounts_group, type == "Inheritance" | type == "Heir") %>% 
  select(id) %>% flatten() %>% as_vector()
transactions_inheritance <- transactions_group %>%
  filter(debit %in% heirs_accounts | credit %in% heirs_accounts)

## Aggregate accounts dealing with heirs
## Aggregate accounts: Estate, branches, winninge ende verlies

## Sum of transactions
transactions_sum <- transactions_inheritance %>% 
  group_by(credit, debit) %>% 
  deb_sum(l, s, d) %>% 
  mutate(pounds = round(deb_lsd_l(l, s, d), 3))

## Total debit of accounts
accounts_sum <- deb_debit(transactions_group) %>% 
  mutate(pounds = round(deb_lsd_l(l, s, d), 3)) %>% 
  full_join(accounts_group, by = c("account_id" = "id")) %>% 
  replace_na(list(pounds = 0)) %>% 
  arrange(pounds)

# Recreate accounts that are in transactions_inheritance data frames
credit <- transactions_inheritance %>% 
  distinct(credit) %>%
  rename(id = credit)

debit <- transactions_inheritance %>% 
  distinct(debit) %>%
  rename(id = debit)

accounts_inheritance <- full_join(credit, debit, by = "id") %>% 
  flatten() %>% as_vector()

nodes <- filter(accounts_sum, account_id %in% accounts_inheritance) %>% 
  mutate(label = if_else(account_id %in% heirs_accounts, paste(group), NA_character_))


# Create igraph object
# Creates vertices from inheritances_transactions data
inheritance <- graph_from_data_frame(d = transactions_sum,
                                     vertices = nodes, directed = TRUE)
set_graph_style()

ggraph(inheritance, layout = "fr") + 
  geom_edge_fan(aes(edge_alpha = l)) + 
  scale_edge_alpha(labels = scales::dollar_format("£")) + 
  geom_node_point(aes(size = pounds, color = label), alpha = 0.7) + 
  geom_node_text(aes(label = label), repel = TRUE) + 
  scale_size_continuous(range = c(0.5, 10), labels = scales::dollar_format("£")) + 
  labs(size = "Accumulated Value",
       edge_alpha = "Transactions",
       color = "Account groups",
       title = "Estate of Jan della Faille de Oude, 1582–1594") + 
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4))) + 
  theme(legend.title = element_text(face = "bold"))

# Circle graph with alpha for direction
ggraph(inheritance, layout = "linear", circular = TRUE) + 
  geom_edge_arc(aes(edge_alpha = l)) + 
  scale_edge_alpha(labels = scales::dollar_format("£")) + 
  geom_node_point(aes(size = pounds, color = label), alpha = 0.7) + 
  geom_node_text(aes(label = group), repel = TRUE) + 
  scale_size_continuous(range = c(0.5, 10), labels = scales::dollar_format("£")) + 
  labs(size = "Accumulated Value",
       edge_alpha = "Transactions",
       color = "Account groups",
       title = "Estate of Jan della Faille de Oude, 1582–1594") + 
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4))) + 
  theme(legend.title = element_text(face = "bold"))

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

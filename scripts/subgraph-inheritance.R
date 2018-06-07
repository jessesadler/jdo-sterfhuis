### Graph of accounts dealing with Heirs ###
# Uses account and transactions groups

library(tidyverse)
library(igraph)
library(ggraph)
library(debkeepr)

# Load data
transactions_group <- read_csv("data/transactions_group.csv")
accounts_group <- read_csv("data/accounts_group.csv") %>% 
  select(id, group, type) %>% 
  mutate(group = str_replace(group, "Balance on 8 November",
                             paste("Opening", "balance", sep = "\n")))
  
## Create subset of transactions that deal directly with heirs
heirs_accounts <- filter(accounts_group, type == "Inheritance") %>% 
  select(id) %>% flatten() %>% as_vector()
transactions_inheritance <- transactions_group %>%
  filter(debit %in% heirs_accounts | credit %in% heirs_accounts)

## Aggregate accounts dealing with heirs

## Sum of transactions
transactions_sum <- transactions_inheritance %>% 
  group_by(credit, debit) %>% 
  deb_sum(l, s, d) %>% 
  mutate(pounds = round(deb_lsd_l(l, s, d), 3))

# Recreate accounts that are in transactions_inheritance
inheritance_credit <- transactions_inheritance %>% 
  distinct(credit) %>%
  rename(id = credit)

inheritance_debit <- transactions_inheritance %>% 
  distinct(debit) %>%
  rename(id = debit)

accounts_inheritance <- full_join(inheritance_credit, inheritance_debit, by = "id")

## Total debit of accounts within inheritance transactions
inheritance_debit_l <- deb_debit(transactions_inheritance) %>% 
  mutate(debit_l = round(deb_lsd_l(l, s, d), 3)) %>% 
  select(account_id, debit_l)

inheritance_credit_l <- deb_credit(transactions_inheritance) %>% 
  mutate(credit_l = round(deb_lsd_l(l, s, d), 3)) %>% 
  select(account_id, credit_l)

nodes <- accounts_inheritance %>% 
  left_join(accounts_group, by = "id") %>% 
  left_join(inheritance_credit_l, by = c("id" = "account_id")) %>% 
  left_join(inheritance_debit_l, by = c("id" = "account_id")) %>% 
  replace_na(list(credit_l = 0, debit_l = 0)) %>% 
  # labels
  mutate(label = if_else(debit_l > 1000 & type != "Inheritance" | credit_l > 1000 & type != "Inheritance",
                         paste(group), NA_character_),
         label_arc = if_else(debit_l > 9000 & type != "Inheritance",
                             paste(group), NA_character_),
         color = if_else(type == "Inheritance", paste(group), NA_character_))

# Create igraph object
# Creates vertices from inheritances_transactions data
inheritance <- graph_from_data_frame(d = transactions_sum,
                                     vertices = nodes, directed = TRUE)
set_graph_style()

set.seed(240)
ggraph(inheritance, layout = "kk") + 
  geom_edge_fan(aes(edge_alpha = l),
                arrow = arrow(length = unit(3, 'mm')), 
                end_cap = circle(2, 'mm')) + 
  scale_edge_alpha(labels = scales::dollar_format("£")) + 
  geom_node_point(aes(size = debit_l, color = color), alpha = 0.9) + 
  geom_node_text(aes(label = label), repel = TRUE) + 
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format("£")) + 
  labs(size = "Total debit",
       edge_alpha = "Transactions",
       color = "Heirs") + 
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4), order = 1)) + 
  theme(legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle("Subgraph of the inheritance of the heirs of Jan de Oude",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594")

ggsave("plots-aans/inheritance-network.png", width = 10, height = 8)

# Arc graph

# Change arrangement of nodes to put heirs at the front
nodes2 <- arrange(nodes, color)

inheritance2 <- graph_from_data_frame(d = transactions_sum,
                                      vertices = nodes2, directed = TRUE)

ggraph(inheritance2, layout = "linear") + 
  geom_edge_arc(aes(edge_alpha = l)) + 
  scale_edge_alpha(labels = scales::dollar_format("£")) + 
  geom_node_point(aes(size = debit_l, color = color), alpha = 0.9) + 
  geom_node_text(aes(label = label_arc)) + 
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format("£")) + 
  labs(size = "Total debit",
       edge_alpha = "Transactions",
       color = "Heirs",
       title = "Estate of Jan della Faille de Oude, 1582–1594") + 
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4), order = 1)) + 
  theme(legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle("Subgraph of the inheritance of the heirs of Jan de Oude",
          subtitle = paste("Lines above the nodes move left (creditor) to right (debtor)",
                           "Lines below the nodes move right (creditor) to left (debtor)",
                           sep = "\n"))

ggsave("plots-aans/inheritance-arc-network.png", width = 12, height = 8)

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

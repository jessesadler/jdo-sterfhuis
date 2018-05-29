### Overview of the estate with ggraph ###

library(tidyverse)
library(igraph)
library(ggraph)
library(debkeepr)

# Load data
transactions <- read_csv("data/transactions.csv")
accounts_id <- read_csv("data/accounts.csv") %>% 
  select(id, type)

# Get dataframe of current value of accounts
# Select only pounds debit to be used for node size
nodes <- deb_debit(transactions) %>% 
  mutate(pounds = round(deb_lsd_l(l, s, d), 3)) %>% 
  full_join(accounts_id, by = c("account_id" = "id")) %>% 
  replace_na(list(pounds = 0)) %>% 
  arrange(pounds)

# Take out balance on 8 November
# This leads to more accurate picture of relationships between accounts
nodes_alt <- deb_debit(transactions) %>% 
  mutate(pounds = round(deb_lsd_l(l, s, d), 3)) %>% 
  full_join(accounts_id, by = c("account_id" = "id")) %>% 
  filter(account_id != "dfl12_001") %>% 
  replace_na(list(pounds = 0)) %>% 
  arrange(pounds)

# Sum of transactions between accounts
transactions_sum <- transactions %>% 
  group_by(credit, debit) %>% 
  deb_sum(l, s, d) %>% 
  mutate(pounds = round(deb_lsd_l(l, s, d), 3))

# Take out transactions with balance
transactions_sum_alt <- transactions %>% 
  filter(credit != "dfl12_001") %>% 
  filter(debit != "dfl12_001") %>% 
  group_by(credit, debit) %>% 
  deb_sum(l, s, d) %>% 
  mutate(pounds = round(deb_lsd_l(l, s, d), 3))

# igraph object
sterfhuis <- graph_from_data_frame(d = transactions_sum, vertices = nodes, directed = TRUE)
sterfhuis_alt <- graph_from_data_frame(d = transactions_sum_alt, vertices = nodes_alt, directed = TRUE)

set_graph_style()

# No color for type
set.seed(240)
ggraph(sterfhuis_alt, layout = "fr") + 
  geom_edge_fan(aes(edge_alpha = l)) + 
  scale_edge_alpha(labels = scales::dollar_format("£")) + 
  geom_node_point(aes(size = pounds), alpha = 0.7) + 
  scale_size_continuous(range = c(0.5, 8), labels = scales::dollar_format("£")) + 
  labs(size = "Accumulated Value",
       edge_alpha = "Transactions",
       title = "Estate of Jan della Faille de Oude, 1582–1594") + 
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4))) + 
  theme(legend.title = element_text(face = "bold"))

# Color for type
set.seed(240)
ggraph(sterfhuis_alt, layout = "fr") + 
  geom_edge_fan(aes(edge_alpha = l)) + 
  scale_edge_alpha(labels = scales::dollar_format("£")) + 
  geom_node_point(aes(size = pounds, color = type), alpha = 0.7) + 
  scale_size_continuous(range = c(0.5, 10), labels = scales::dollar_format("£")) + 
  labs(size = "Accumulated Value",
       edge_alpha = "Transactions",
       color = "Account groups",
       title = "Estate of Jan della Faille de Oude, 1582–1594") + 
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4))) + 
  theme(legend.title = element_text(face = "bold"))

ggsave("sterfhuis-network.png", width = 10, height = 8)

# Remove isolated nodes and subgraph
sterfhuis_tbl <- as_tbl_graph(sterfhuis_alt)

# Remove isolated
sterfhuis_tbl <- sterfhuis_tbl %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated())

# Remove subgraph of accounts written off
sterfhuis_tbl <- sterfhuis_tbl %>%
  activate(nodes) %>% 
  mutate(group = group_components()) %>% 
  filter(group == 1)

set.seed(240)
ggraph(sterfhuis_tbl, layout = "fr") + 
  geom_edge_fan(aes(edge_alpha = l), show.legend = c(edge_alpha = FALSE)) + 
  geom_node_point(aes(size = pounds, color = type), alpha = 0.7,
                  show.legend = c(size = FALSE)) + 
  scale_size_continuous(range = c(0.5, 8)) + 
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) + 
  labs(color = "Account groups",
       title = "Estate of Jan della Faille de Oude, 1582–1594") + 
  theme(legend.position = "bottom")

ggsave("sterfhuis-network.png", width = 10, height = 8)
## Groups network ##

library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(debkeepr)

transactions_group <- read_csv("data/transactions_group.csv")
accounts_group <- read_csv("data/accounts_group.csv")

# Create network from groups column in accounts #
nodes <- deb_debit(transactions_group) %>% 
  full_join(accounts_group, by = c("account_id" = "id")) %>% 
  rename(id = account_id, account_id = account_id.y) %>% 
  filter(account_id != "dfl12_001") %>% 
  mutate(pounds = round(deb_lsd_l(l, s, d), 3)) %>% 
  replace_na(list(pounds = 0)) %>% 
  arrange(pounds)

# Take out transactions with balance
transactions_sum <- transactions_group %>% 
  filter(credit != 1) %>% 
  filter(debit != 1) %>% 
  group_by(credit, debit) %>% 
  deb_sum(l, s, d) %>% 
  mutate(pounds = round(deb_lsd_l(l, s, d), 3))

sterfhuis_groups <- graph_from_data_frame(d = transactions_sum, vertices = nodes, directed = TRUE)
sterfhuis_groups_tbl <- as_tbl_graph(sterfhuis_groups)

# Remove isolated accounts
sterfhuis_groups_tbl <- sterfhuis_groups_tbl %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated())

set_graph_style()

set.seed(12)
ggraph(sterfhuis_groups_tbl, layout = "fr") + 
  geom_edge_fan(aes(edge_alpha = l)) + 
  scale_edge_alpha(labels = scales::dollar_format("£")) + 
  geom_node_point(aes(size = pounds, color = type), alpha = 0.9) + 
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format("£")) + 
  labs(size = "Accumulated Value",
       edge_alpha = "Transactions",
       color = "Account types") + 
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4), order = 1)) + 
  theme(legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle("Estate of Jan della Faille de Oude, 1582–1594",
          subtitle = "Groups of accounts")

ggsave("plots-aans/sterfhuis-network-groups.png", width = 10, height = 8)

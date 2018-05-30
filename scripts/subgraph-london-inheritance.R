### Graph of accounts dealing with a single account ###
# Uses account and transactions groups

library(tidyverse)
library(igraph)
library(ggraph)
library(debkeepr)

# Load data
transactions <- read_csv("data/transactions.csv")
accounts <- read_csv("data/accounts.csv") %>% 
  select(id, label, group, type) %>% 
  mutate(group = str_replace(group, "Balance on 8 November",
                             paste("Opening", "balance", sep = "\n")))

## Create subset of transactions that deal directly with heirs
london_inheritance_accounts <- c("dfl12_478", "dfl12_479", "dfl12_489")
transactions_london <- transactions %>%
  filter(debit %in% london_inheritance_accounts | credit %in% london_inheritance_accounts)

## Sum of transactions
transactions_sum <- transactions_london %>% 
  group_by(credit, debit) %>% 
  deb_sum(l, s, d) %>% 
  mutate(pounds = round(deb_lsd_l(l, s, d), 3))

# Recreate accounts that are in transactions_london
london_credit <- transactions_london %>% 
  distinct(credit) %>%
  rename(id = credit)

london_debit <- transactions_london %>% 
  distinct(debit) %>%
  rename(id = debit)

accounts_london <- full_join(london_credit, london_debit, by = "id")

## Total debit of accounts within london transactions
london_debit_l <- deb_debit(transactions_london) %>% 
  mutate(debit_l = round(deb_lsd_l(l, s, d), 3)) %>% 
  select(account_id, debit_l)

london_credit_l <- deb_credit(transactions_london) %>% 
  mutate(credit_l = round(deb_lsd_l(l, s, d), 3)) %>% 
  select(account_id, credit_l)

nodes <- accounts_london %>% 
  left_join(accounts, by = "id") %>% 
  left_join(london_credit_l, by = c("id" = "account_id")) %>% 
  left_join(london_debit_l, by = c("id" = "account_id")) %>% 
  mutate(debit_l = if_else(type == "Branch", credit_l, debit_l),
         color = if_else(type == "Branch", paste("Branch of London", "creditor", sep = "\n"), NA_character_))

# Create igraph object
# Creates vertices from inheritances_transactions data
london_inheritance <- graph_from_data_frame(d = transactions_sum,
                                            vertices = nodes, directed = TRUE)

set_graph_style()

set.seed(240)
ggraph(london_inheritance, layout = "kk") + 
  geom_edge_fan(aes(edge_alpha = l),
                arrow = arrow(length = unit(3, 'mm')), 
                end_cap = circle(2, 'mm')) + 
  scale_edge_alpha(labels = scales::dollar_format("£")) + 
  geom_node_point(aes(size = debit_l, color = color), alpha = 0.9) + 
  geom_node_text(aes(label = label), repel = TRUE) + 
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format("£")) + 
  labs(size = "Total debit",
       edge_alpha = "Transactions",
       color = NULL) + 
  guides(color = guide_legend(override.aes = list(size = 4), order = 1)) + 
  theme(legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle("Subgraph of the inheritance from London",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594")

ggsave("plots-aans/london-inheritance2.png", width = 10, height = 8)

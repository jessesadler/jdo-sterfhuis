## Branches subgraph ##

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
branch_accounts <- filter(accounts_group, type == "Branch") %>% 
  select(id) %>% flatten() %>% as_vector()
transactions_branches <- transactions_group %>%
  filter(debit %in% branch_accounts | credit %in% branch_accounts)

## Aggregate accounts dealing with branches

## Sum of transactions
transactions_sum <- transactions_branches %>% 
  group_by(credit, debit) %>% 
  deb_sum(l, s, d) %>% 
  mutate(pounds = round(deb_lsd_l(l, s, d), 3))

# Recreate accounts that are in transactions_branches
branches_credit <- transactions_branches %>% 
  distinct(credit) %>%
  rename(id = credit)

branches_debit <- transactions_branches %>% 
  distinct(debit) %>%
  rename(id = debit)

accounts_branches <- full_join(branches_credit, branches_debit, by = "id")

## Total debit of accounts within branches transactions
branches_debit_l <- deb_debit(transactions_branches) %>% 
  mutate(debit_l = round(deb_lsd_l(l, s, d), 3)) %>% 
  select(account_id, debit_l)

branches_credit_l <- deb_credit(transactions_branches) %>% 
  mutate(credit_l = round(deb_lsd_l(l, s, d), 3)) %>% 
  select(account_id, credit_l)

# Build nodes from parts
nodes <- accounts_branches %>% 
  left_join(accounts_group, by = "id") %>% 
  left_join(branches_credit_l, by = c("id" = "account_id")) %>% 
  left_join(branches_debit_l, by = c("id" = "account_id")) %>% 
  replace_na(list(credit_l = 0, debit_l = 0)) %>% 
  # Labels
  mutate(label = if_else(credit_l > 2500 & type != "Inheritance" | debit_l > 2500 & type != "Inheritance",
                         paste(group), NA_character_)) %>% 
  mutate(color = if_else(type == "Inheritance", paste(group), NA_character_))

# Create igraph object
# Creates vertices from branches data
branches <- graph_from_data_frame(d = transactions_sum,
                                     vertices = nodes, directed = TRUE)
set_graph_style()

# Size is total credit
set.seed(240)
ggraph(branches, layout = "kk") + 
  geom_edge_fan(aes(edge_alpha = l),
                arrow = arrow(length = unit(3, 'mm')), 
                end_cap = circle(2, 'mm')) + 
  scale_edge_alpha(labels = scales::dollar_format("£")) + 
  geom_node_point(aes(size = credit_l, color = color), alpha = 0.9) + 
  geom_node_text(aes(label = label), repel = TRUE) + 
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format("£")) + 
  labs(size = "Total credit",
       edge_alpha = "Transactions",
       color = "Heirs") + 
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4), order = 1)) + 
  theme(legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle("Subgraph of the branches in the trade of Jan de Oude",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594")

ggsave("plots-aans/branches-credit.png", width = 10, height = 8)

# Size is total debit
set.seed(240)
ggraph(branches, layout = "kk") + 
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
  ggtitle("Subgraph of the branches in the trade of Jan de Oude",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594")

ggsave("plots-aans/branches-debit.png", width = 10, height = 8)

## Generic subgraph ##

library(tidyverse)
library(igraph)
library(ggraph)
library(debkeepr)

transactions <- read_rds("data/transactions-lsd.rds")
accounts <- read_csv("data/accounts.csv") %>% 
  select(id, label)


transactions_subset <- transactions %>% 
  filter(debit == "dfl12_038" | credit == "dfl12_038")

accounts_vector <- c("dfl12_038", "dfl12_289")

transactions_subset <- transactions %>% 
  filter(debit %in% accounts_vector | credit %in% accounts_vector)

transactions_sum <- transactions_subset %>% 
  group_by(credit, debit) %>% 
  deb_summarise(lsd) %>% 
  mutate(pounds = deb_lsd_l(lsd))

# Recreate accounts that are in transactions_subset
sub_credit_accounts <- transactions_subset %>% 
  distinct(credit) %>%
  rename(id = credit)

sub_debit_accounts <- transactions_subset %>% 
  distinct(debit) %>%
  rename(id = debit)

sub_accounts <- full_join(sub_credit_accounts, sub_debit_accounts, by = "id")

## Total debit of accounts within branches transactions
sub_debit <- deb_debit(transactions_subset) %>% 
  mutate(debit = deb_lsd_l(lsd)) %>% 
  select(-lsd)

sub_credit <- deb_credit(transactions_subset) %>% 
  mutate(credit = deb_lsd_l(lsd)) %>% 
  select(-lsd)

# Build nodes from parts
nodes <- sub_accounts %>% 
  left_join(accounts, by = "id") %>% 
  left_join(sub_credit, by = c("id" = "account_id")) %>% 
  left_join(sub_debit, by = c("id" = "account_id")) %>% 
  mutate(label = if_else(credit > 2500 | debit > 2500,
                         paste(label), NA_character_))

sub_graph <- graph_from_data_frame(d = transactions_sum,
                                   vertices = nodes, directed = TRUE)

set.seed(240)
ggraph(sub_graph, layout = "kk") + 
  geom_edge_fan(aes(edge_alpha = pounds),
                arrow = arrow(length = unit(4, 'mm')), 
                end_cap = circle(2, 'mm')) + 
  scale_edge_alpha(labels = scales::dollar_format(prefix = "£")) + 
  geom_node_point(aes(size = credit), alpha = 0.9) + 
  geom_node_text(aes(label = label), repel = TRUE) + 
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format(prefix = "£")) + 
  labs(size = "Total credit",
       edge_alpha = "Transactions") + 
  theme_graph()

# Arc graph
ggraph(sub_graph, layout = "linear") + 
  geom_edge_arc(aes(edge_alpha = pounds)) + 
  scale_edge_alpha(labels = scales::dollar_format(prefix = "£")) + 
  geom_node_point(aes(size = debit), alpha = 0.9) + 
  geom_node_text(aes(label = label),
                 nudge_y = 1.75, nudge_x = 1.75, angle = 45) + 
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format(prefix = "£")) + 
  labs(size = "Total debit",
       edge_alpha = "Transactions") + 
  theme_graph()

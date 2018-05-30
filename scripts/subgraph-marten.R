### Graph of accounts dealing with a single heir ###
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
marten_accounts <- filter(accounts, group == "Marten") %>% 
  select(id) %>% flatten() %>% as_vector()
transactions_marten <- transactions %>%
  filter(debit %in% marten_accounts | credit %in% marten_accounts)

## Sum of transactions
transactions_sum <- transactions_marten %>% 
  group_by(credit, debit) %>% 
  deb_sum(l, s, d) %>% 
  mutate(pounds = round(deb_lsd_l(l, s, d), 3))

# Recreate accounts that are in transactions_marten
marten_credit <- transactions_marten %>% 
  distinct(credit) %>%
  rename(id = credit)

marten_debit <- transactions_marten %>% 
  distinct(debit) %>%
  rename(id = debit)

accounts_marten <- full_join(marten_credit, marten_debit, by = "id")

## Total debit of accounts within marten transactions
marten_debit_l <- deb_debit(transactions_marten) %>% 
  mutate(debit_l = round(deb_lsd_l(l, s, d), 3)) %>% 
  select(account_id, debit_l)

marten_credit_l <- deb_credit(transactions_marten) %>% 
  mutate(credit_l = round(deb_lsd_l(l, s, d), 3)) %>% 
  select(account_id, credit_l)

nodes <- accounts_marten %>% 
  left_join(accounts, by = "id") %>% 
  left_join(marten_credit_l, by = c("id" = "account_id")) %>% 
  left_join(marten_debit_l, by = c("id" = "account_id")) %>% 
  replace_na(list(credit_l = 0, debit_l = 0)) %>% 
  # labels: Fix labels to use
  mutate(label = if_else(debit_l > 500 & type != "Inheritance" | credit_l > 500 & type != "Inheritance",
                         paste(group), NA_character_),
         label = str_remove(label, "Marten"),
         color = if_else(group == "Marten", "Marten's accounts", NA_character_))

# Create igraph object
# Creates vertices from inheritances_transactions data
marten <- graph_from_data_frame(d = transactions_sum,
                                     vertices = nodes, directed = TRUE)
set_graph_style()

set.seed(240)
ggraph(marten, layout = "kk") + 
  geom_edge_fan(aes(edge_alpha = l),
                arrow = arrow(length = unit(3, 'mm')), 
                end_cap = circle(2, 'mm')) + 
  scale_edge_alpha(labels = scales::dollar_format("£")) + 
  geom_node_point(aes(size = credit_l, color = color), alpha = 0.9) + 
  geom_node_text(aes(label = label), repel = TRUE) + 
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format("£")) + 
  labs(size = "Total debit",
       edge_alpha = "Transactions",
       color = NULL) + 
  guides(color = guide_legend(override.aes = list(size = 4), order = 1)) + 
  theme(legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle("Subgraph of the inheritance of Marten della Faille",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594")

ggsave("plots-aans/inheritance-marten.png", width = 10, height = 8)

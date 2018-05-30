## Subgraph of bills of exchange ##

# Create separate graphs for total credit and debit as size of node
# Color of nodes is balance: balanced, more credit, or more debit

library(tidyverse)
library(igraph)
library(ggraph)
library(debkeepr)

# Load data
transactions <- read_csv("data/transactions.csv")
accounts <- read_csv("data/accounts.csv") %>% 
  select(id, label, type)

wissel_accounts <- filter(accounts, type == "Wissel") %>% 
  select(id) %>% flatten() %>% as_vector()

transactions_wissel <- transactions %>%
  filter(debit %in% wissel_accounts | credit %in% wissel_accounts)

## Sum of transactions
transactions_wissel_sum <- transactions_wissel %>% 
  group_by(credit, debit) %>% 
  deb_sum(l, s, d) %>% 
  mutate(pounds = round(deb_lsd_l(l, s, d), 3))

## Total credit and debit of accounts within inheritance transactions ##

# Whether account is more debtor or creditor
accounts_wissel_debit <- deb_debit(transactions_wissel) %>% 
  mutate(denarii = -round(deb_lsd_d(l, s, d), 3))

accounts_wissel_credit <- deb_credit(transactions_wissel) %>% 
  mutate(denarii = round(deb_lsd_d(l, s, d), 3))

accounts_wissel_sum <- bind_rows(accounts_wissel_debit, accounts_wissel_credit) %>% 
  group_by(account_id) %>% 
  summarise(denarii = sum(denarii)) %>% 
  mutate(relation = case_when(denarii == 0 ~ "Balanced",
                              denarii > 0 ~ "Creditor",
                              denarii < 0 ~ "Debtor"),
         denarii = if_else(denarii < 0, -denarii, denarii)) %>% 
  deb_d_mutate(denarii) %>% 
  left_join(accounts, by = c("account_id" = "id"))

# Find total credit and debit for each account
wissel_debit_l <- deb_debit(transactions_wissel) %>% 
  mutate(debit_l = round(deb_lsd_l(l, s, d), 3)) %>% 
  select(account_id, debit_l)

wissel_credit_l <- deb_credit(transactions_wissel) %>% 
  mutate(credit_l = round(deb_lsd_l(l, s, d), 3)) %>% 
  select(account_id, credit_l)

nodes <- accounts_wissel_sum %>% 
  left_join(wissel_credit_l, by = "account_id") %>% 
  left_join(wissel_debit_l, by = "account_id") %>% 
  replace_na(list(credit_l = 0, debit_l = 0)) %>% 
  mutate(text = if_else(credit_l > 2000 | debit_l > 1500, paste(label), ""))

wissels <- graph_from_data_frame(d = transactions_wissel_sum,
                                 vertices = nodes, directed = TRUE)

set_graph_style()

# Size is total credit
set.seed(240)
ggraph(wissels, layout = "fr") + 
  geom_edge_fan(aes(edge_alpha = pounds)) + 
  scale_edge_alpha(labels = scales::dollar_format("£")) + 
  geom_node_point(aes(size = credit_l, color = relation), alpha = 0.9) + 
  geom_node_text(aes(label = text)) + 
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format("£")) + 
  labs(size = "Total credit",
       edge_alpha = "Transactions",
       color = "Balance") + 
  guides(color = guide_legend(override.aes = list(size = 4), order = 1)) + 
  theme(legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle("Subgraph of bills of exchange: Total credit",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594")

ggsave("plots-aans/wissels-credit.png", width = 10, height = 8)

# Size is total debit
set.seed(240)
ggraph(wissels, layout = "fr") + 
  geom_edge_fan(aes(edge_alpha = pounds)) + 
  scale_edge_alpha(labels = scales::dollar_format("£")) + 
  geom_node_point(aes(size = debit_l, color = relation), alpha = 0.9) + 
  geom_node_text(aes(label = text)) + 
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format("£")) + 
  labs(size = "Total debit",
       edge_alpha = "Transactions",
       color = "Balance") + 
  guides(color = guide_legend(override.aes = list(size = 4), order = 1)) + 
  theme(legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle("Subgraph of bills of exchange: Total debit",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594")

ggsave("plots-aans/wissels-debit.png", width = 10, height = 8)

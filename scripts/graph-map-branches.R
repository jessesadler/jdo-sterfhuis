### ggraph with ggmap for transactions between branches ###

library(tidyverse)
library(stringr)
library(ggmap)
library(tidygraph)
library(ggraph)
library(debkeepr)

# Load data
transactions <- read_csv("data/transactions.csv")
accounts <- read_csv("data/accounts.csv")

transactions_d <- transactions %>% 
  deb_lsd_d_mutate(column_name = denarii) %>% 
  select(credit, debit, date, denarii)

### Filter transactions to only those between branches ###
branch_accounts <- filter(accounts, type == "Branch") %>% 
  select(id) %>% flatten() %>% as_vector()

branch_transactions <- transactions_d %>% 
  filter(credit %in% branch_accounts & debit %in% branch_accounts)

# Aggregate accounts by branch
# Verona
branch_transactions$credit <- str_replace_all(branch_transactions$credit, "dfl12_446", "dfl12_110")
branch_transactions$debit <- str_replace_all(branch_transactions$debit, "dfl12_446", "dfl12_110")

# Venice
branch_transactions$credit <- str_replace_all(branch_transactions$credit, "dfl12_181", "dfl12_111")
branch_transactions$debit <- str_replace_all(branch_transactions$debit, "dfl12_181", "dfl12_111")

# London
branch_transactions$credit <- str_replace_all(branch_transactions$credit, "dfl12_477", "dfl12_112")
branch_transactions$debit <- str_replace_all(branch_transactions$debit, "dfl12_477", "dfl12_112")

### Summarise transactions between accounts and take out transactions within a branch ###
branch_edges <- branch_transactions %>% 
  group_by(credit, debit) %>% 
  summarise(denarii = sum(denarii)) %>% 
  mutate(pounds = floor(denarii/240)) %>% 
  filter(credit != debit)

### Create branch nodes ###
branch_accounts_tbl <- accounts %>% 
  filter(id %in% c("dfl12_110", "dfl12_111", "dfl12_112")) %>% 
  select(id, group, location)

branch_accounts_df <- branch_accounts_tbl %>% 
  as.data.frame(stringsAsFactors = FALSE)

branch_accounts_geo <- mutate_geocode(branch_accounts_df, location)

branch_nodes <- branch_accounts_geo %>% 
  as_tibble() %>% 
  rename(x = lon, y = lat)

### Make into tidygraph object ###
branches <- tbl_graph(branch_nodes, branch_edges)

# Get background map

map <- get_googlemap(center = c(3.5, 46.5), zoom = 5,
                     color = "bw",
                     style = "feature:road|visibility:off&style=element:labels|visibility:off&
                     style=feature:administrative|visibility:off")

gg <- ggraph(branches, layout = "nicely")

ggmap(map, base_layer = gg) +
  geom_node_text(aes(label = location), repel = TRUE) +
  geom_edge_arc(aes(width = pounds),
                arrow = arrow(length = unit(4, 'mm')), 
                end_cap = circle(2, 'mm')) + 
  scale_edge_width(range = c(1, 2)) +
  theme_graph()

ggraph(branches, layout = "linear") +
  geom_node_text(aes(label = location)) +
  geom_edge_fan(aes(width = pounds), arrow = arrow(length = unit(2, 'mm')), 
                end_cap = circle(4, 'mm')) + 
  scale_edge_width(range = c(0.2, 3)) +
  theme_graph()

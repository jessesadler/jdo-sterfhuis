### Sterfhuis graph with node size by pounds received ###

library(tidyverse)
library(stringr)
library(igraph)
library(ggraph)
source("scripts/functions.R")

# Load data
transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d"))) %>% 
  select(from:denari, tr_type) %>% 
  rename(l = librae, s = solidi, d = denarii)
accounts <- read_csv("data/accounts.csv") %>% 
  select(id, account:location)

### Aggregate accounts ###

## Inheritance accounts ##
anna_accounts <- filter(accounts, group == "Anna & Robert") %>% 
  select(id) %>% flatten() %>% as_vector()
anna_replace <- set_names(replicate(length(anna_accounts), "dfl12_251"), anna_accounts)

jan_accounts <- filter(accounts, group == "Jan") %>% 
  select(id) %>% flatten() %>% as_vector()
jan_replace <- set_names(replicate(length(jan_accounts), "dfl12_287"), jan_accounts)

marten_accounts <- filter(accounts, group == "Marten") %>% 
  select(id) %>% flatten() %>% as_vector()
marten_replace <- set_names(replicate(length(marten_accounts), "dfl12_295"), marten_accounts)

maria_accounts <- filter(accounts, group == "Maria") %>% 
  select(id) %>% flatten() %>% as_vector()
maria_replace <- set_names(replicate(length(maria_accounts), "dfl12_351"), maria_accounts)

carlo_accounts <- filter(accounts, group == "Carlo") %>% 
  select(id) %>% flatten() %>% as_vector()
carlo_replace <- set_names(replicate(length(carlo_accounts), "dfl12_285"), carlo_accounts)

jacques_accounts <- filter(accounts, group == "Jacques") %>% 
  select(id) %>% flatten() %>% as_vector()
jacques_replace <- set_names(replicate(length(jacques_accounts), "dfl12_102"), jacques_accounts)

steven_accounts <- filter(accounts, group == "Steven") %>% 
  select(id) %>% flatten() %>% as_vector()
steven_replace <- set_names(replicate(length(steven_accounts), "dfl12_333"), steven_accounts)

hester_accounts <- filter(accounts, group == "Hester") %>% 
  select(id) %>% flatten() %>% as_vector()
hester_replace <- set_names(replicate(length(hester_accounts), "dfl12_252"), hester_accounts)

cornelia_accounts <- filter(accounts, group == "Cornelia") %>% 
  select(id) %>% flatten() %>% as_vector()
cornelia_replace <- set_names(replicate(length(cornelia_accounts), "dfl12_253"), cornelia_accounts)

anna_de_hane_accounts <- filter(accounts, group == "Anna de Hane") %>% 
  select(id) %>% flatten() %>% as_vector()
anna_de_hane_replace <- set_names(replicate(length(anna_de_hane_accounts), "dfl12_310"), anna_de_hane_accounts)

# Replace ids in transactions
transactions$from <- str_replace_all(transactions$from, anna_replace)
transactions$from <- str_replace_all(transactions$from, jan_replace)
transactions$from <- str_replace_all(transactions$from, marten_replace)
transactions$from <- str_replace_all(transactions$from, maria_replace)
transactions$from <- str_replace_all(transactions$from, carlo_replace)
transactions$from <- str_replace_all(transactions$from, jacques_replace)
transactions$from <- str_replace_all(transactions$from, steven_replace)
transactions$from <- str_replace_all(transactions$from, hester_replace)
transactions$from <- str_replace_all(transactions$from, cornelia_replace)
transactions$from <- str_replace_all(transactions$from, anna_de_hane_replace)

transactions$to <- str_replace_all(transactions$to, anna_replace)
transactions$to <- str_replace_all(transactions$to, jan_replace)
transactions$to <- str_replace_all(transactions$to, marten_replace)
transactions$to <- str_replace_all(transactions$to, maria_replace)
transactions$to <- str_replace_all(transactions$to, carlo_replace)
transactions$to <- str_replace_all(transactions$to, jacques_replace)
transactions$to <- str_replace_all(transactions$to, steven_replace)
transactions$to <- str_replace_all(transactions$to, hester_replace)
transactions$to <- str_replace_all(transactions$to, cornelia_replace)
transactions$to <- str_replace_all(transactions$to, anna_de_hane_replace)

## Estate
estate_accounts <- filter(accounts, type == "Estate") %>% 
  select(id) %>% flatten() %>% as_vector()
estate_replace <- set_names(replicate(length(estate_accounts), "dfl12_151"), estate_accounts)

transactions$from <- str_replace_all(transactions$from, estate_replace)
transactions$to <- str_replace_all(transactions$to, estate_replace)

## Profits and losses
transactions$from <- str_replace_all(transactions$from, "dfl12_445", "dfl12_038")
transactions$to <- str_replace_all(transactions$to, "dfl12_445", "dfl12_038")

## Wissels
wissel_accounts <- filter(accounts, type == "Wissel") %>% 
  select(id) %>% flatten() %>% as_vector()
wissel_replace <- set_names(replicate(length(wissel_accounts), "dfl12_117"), wissel_accounts)

transactions$from <- str_replace_all(transactions$from, wissel_replace)
transactions$to <- str_replace_all(transactions$to, wissel_replace)

## Branches

# Verona
transactions$from <- str_replace_all(transactions$from, "dfl12_446", "dfl12_110")
transactions$to <- str_replace_all(transactions$to, "dfl12_446", "dfl12_110")

# Venice
transactions$from <- str_replace_all(transactions$from, "dfl12_181", "dfl12_111")
transactions$to <- str_replace_all(transactions$to, "dfl12_181", "dfl12_111")

# London
transactions$from <- str_replace_all(transactions$from, "dfl12_477", "dfl12_112")
transactions$to <- str_replace_all(transactions$to, "dfl12_477", "dfl12_112")

## Get dataframe of current value of accounts
# Select only pounds debit to be used for node size
current <- deb_current(transactions) %>% select(id, pounds = l_d)

## Sum of transactions
transactions <- deb_group_sum(transactions) %>% ungroup

# Recreate accounts that are in transactions data frames
from <- transactions %>% 
  distinct(from) %>%
  rename(id = from)

to <- transactions %>% 
  distinct(to) %>%
  rename(id = to)

nodes <- full_join(from, to, by = "id") # create nodes data frame
nodes <- filter(accounts, id %in% nodes$id) # Use nodes to get subset of accounts data
nodes <- left_join(nodes, current, by = "id") # add total pounds debit from current

# Create igraph object
# Creates vertices from inheritances_transactions data
sterfhuis <- graph_from_data_frame(d = transactions, vertices = nodes, directed = TRUE)

ggraph(sterfhuis, layout = "kk") + 
  geom_edge_link(aes(alpha = l)) + 
  geom_node_point(aes(size = pounds)) + 
  scale_size_continuous(range = c(0.3, 6)) +
  theme_graph()

# Arc graph
ggraph(sterfhuis, layout = "linear") + 
  geom_edge_arc(aes(alpha = l)) + 
  geom_node_point(aes(size = pounds)) +
  scale_size_continuous(range = c(0.5, 6)) +
  theme_graph()

# Circle graph
ggraph(sterfhuis, layout = "linear", circular = TRUE) + 
  geom_edge_arc(aes(alpha = l)) + 
  geom_node_point(aes(size = pounds)) +
  scale_size_continuous(range = c(0.5, 6)) +
  theme_graph()


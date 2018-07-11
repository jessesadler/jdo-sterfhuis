### Sterfhuis graph with node size by pounds received ###

library(tidyverse)
library(stringr)
library(igraph)
library(ggraph)
library(debkeepr)

# Load data
transactions <- read_csv("data/transactions.csv")
accounts <- read_csv("data/accounts.csv")

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
transactions$credit <- str_replace_all(transactions$credit, anna_replace)
transactions$credit <- str_replace_all(transactions$credit, jan_replace)
transactions$credit <- str_replace_all(transactions$credit, marten_replace)
transactions$credit <- str_replace_all(transactions$credit, maria_replace)
transactions$credit <- str_replace_all(transactions$credit, carlo_replace)
transactions$credit <- str_replace_all(transactions$credit, jacques_replace)
transactions$credit <- str_replace_all(transactions$credit, steven_replace)
transactions$credit <- str_replace_all(transactions$credit, hester_replace)
transactions$credit <- str_replace_all(transactions$credit, cornelia_replace)
transactions$credit <- str_replace_all(transactions$credit, anna_de_hane_replace)

transactions$debit <- str_replace_all(transactions$debit, anna_replace)
transactions$debit <- str_replace_all(transactions$debit, jan_replace)
transactions$debit <- str_replace_all(transactions$debit, marten_replace)
transactions$debit <- str_replace_all(transactions$debit, maria_replace)
transactions$debit <- str_replace_all(transactions$debit, carlo_replace)
transactions$debit <- str_replace_all(transactions$debit, jacques_replace)
transactions$debit <- str_replace_all(transactions$debit, steven_replace)
transactions$debit <- str_replace_all(transactions$debit, hester_replace)
transactions$debit <- str_replace_all(transactions$debit, cornelia_replace)
transactions$debit <- str_replace_all(transactions$debit, anna_de_hane_replace)

## Estate
estate_accounts <- filter(accounts, type == "Estate") %>% 
  select(id) %>% flatten() %>% as_vector()
estate_replace <- set_names(replicate(length(estate_accounts), "dfl12_151"), estate_accounts)

transactions$credit <- str_replace_all(transactions$credit, estate_replace)
transactions$debit <- str_replace_all(transactions$debit, estate_replace)

## Profits and losses
transactions$credit <- str_replace_all(transactions$credit, "dfl12_445", "dfl12_038")
transactions$debit <- str_replace_all(transactions$debit, "dfl12_445", "dfl12_038")

## Wissels
wissel_accounts <- filter(accounts, type == "Wissel") %>% 
  select(id) %>% flatten() %>% as_vector()
wissel_replace <- set_names(replicate(length(wissel_accounts), "dfl12_117"), wissel_accounts)

transactions$credit <- str_replace_all(transactions$credit, wissel_replace)
transactions$debit <- str_replace_all(transactions$debit, wissel_replace)

## Branches

# Verona
transactions$credit <- str_replace_all(transactions$credit, "dfl12_446", "dfl12_110")
transactions$debit <- str_replace_all(transactions$debit, "dfl12_446", "dfl12_110")

# Venice
transactions$credit <- str_replace_all(transactions$credit, "dfl12_181", "dfl12_111")
transactions$debit <- str_replace_all(transactions$debit, "dfl12_181", "dfl12_111")

# London
transactions$credit <- str_replace_all(transactions$credit, "dfl12_477", "dfl12_112")
transactions$debit <- str_replace_all(transactions$debit, "dfl12_477", "dfl12_112")

## Get dataframe of current value of accounts
# Select only pounds debit to be used for node size
total_debit <- deb_debit(transactions) %>% 
  deb_lsd_l_mutate(column_name = total_debit)

## Sum of transactions
transactions_sum <- transactions %>% 
  group_by(credit, debit) %>% 
  deb_sum(l, s, d) %>% 
  deb_lsd_l_mutate(column_name = pounds)

# If not aggregating accounts
nodes <- left_join(accounts, total_debit, by = c("id" = "account_id")) %>% 
  replace_na(list(total_debit = 0)) %>% # Make total_debit 0 for accounts that had no debit
  select(-id_int)

# Recreate accounts that are in transactions data frames
credit <- transactions %>% 
  distinct(credit) %>%
  rename(id = credit)

debit <- transactions %>% 
  distinct(debit) %>%
  rename(id = debit)

nodes <- full_join(credit, debit, by = "id") # create nodes data frame
nodes <- filter(accounts, id %in% nodes$id) %>% # Use nodes to get subset of accounts data
  select(-id_int)
nodes <- left_join(nodes, total_debit, by = c("id" = "account_id")) %>% 
  replace_na(list(total_debit = 0))

# Create igraph object
# Creates vertices credit inheritances_transactions data
sterfhuis <- graph_from_data_frame(d = transactions_sum,
                                   vertices = nodes,
                                   directed = TRUE)

ggraph(sterfhuis, layout = "kk") + 
  geom_edge_link(aes(alpha = l)) + 
  geom_node_point(aes(size = total_debit)) + 
  scale_size_continuous(range = c(0.3, 6)) +
  theme_graph()

# Arc graph
ggraph(sterfhuis, layout = "linear") + 
  geom_edge_arc(aes(alpha = l)) + 
  geom_node_point(aes(size = total_debit)) +
  scale_size_continuous(range = c(0.5, 6)) +
  theme_graph()

# Circle graph
ggraph(sterfhuis, layout = "linear", circular = TRUE) + 
  geom_edge_arc(aes(alpha = l)) + 
  geom_node_point(aes(size = total_debit)) +
  scale_size_continuous(range = c(0.5, 6)) +
  theme_graph()

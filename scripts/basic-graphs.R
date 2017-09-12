library(tidyverse)
library(igraph)
library(ggraph)

# Load data
transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d")))
accounts <- read_csv("data/accounts.csv")

transactions <- group_by(transactions, from, to) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  ungroup()

accounts <- select(accounts, id, label = account)

sterfhuis <- graph_from_data_frame(d = transactions, vertices = accounts, directed = TRUE)

set_graph_style()

ggraph(sterfhuis) + 
  geom_edge_link(aes(alpha = count)) + 
  geom_node_point(alpha = 0.7) + 
  theme(legend.position = "none")

ggraph(sterfhuis, layout = "kk") + 
  geom_edge_link(aes(alpha = count)) + 
  geom_node_point(alpha = 0.7) + 
  theme(legend.position = "none") + 
  labs(title = "Accounts of the estate of a 16th-century merchant")

ggraph(sterfhuis, layout = "linear") + geom_edge_arc(aes(alpha = count)) + geom_node_point(aes(color = ac_type), alpha = 0.7)
### Running value of accounts ###

library(tidyverse)

# Load data
transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d"))) %>% 
  select(from:denari, tr_type) %>% 
  rename(l = livre, s = solidi, d = denari)
accounts <- read_csv("data/accounts.csv") %>% 
  select(id, account:location)

# Change transactions to denari and simplify
transactions_d <- transactions %>% 
  mutate(denari = deb_lsd_d(l, s, d)) %>% 
  select(from, to, date, denari)

# Get total credit and debit for each accout by date
credit <- transactions_d %>% group_by(from, date) %>% 
  summarise(denari = sum(denari)) %>% 
  rename(id = from)
debit <- transactions_d %>% group_by(to, date) %>% 
  summarise(denari = -sum(denari)) %>% 
  rename(id = to)

# Row bind the two data frame together
accounts_running <- bind_rows(credit, debit) %>% 
  group_by(id, date) %>% 
  summarise(denari = sum(denari))

# Create data frame with running value for each account by date
accounts_current <- accounts_running %>% 
  group_by(id) %>% 
  mutate(current = cumsum(denari)) %>% 
  mutate(vlaams = deb_d_lsd_print(current))


### Create line graph from branches ###
library(ggplot2)
library(ggrepel)

# Filter branches and add in names of accounts
branch_accounts <- filter(accounts, type == "Branch") %>% 
  select(id, label)
branches <- accounts_current %>% filter(id %in% branch_accounts$id)
branches <- left_join(branches, branch_accounts, by = "id")

### Plots ###
# y = current/240 to turn denari to pounds
ggplot(branches) + 
  geom_line(aes(x = date, y = current/240, group = label, color = label))

# Split the branches between before and after the break in accounting
# which began on 16 March 1585
library(lubridate)
date_break <- ymd(15850316)

branches_1 <- filter(branches, date < date_break)
branches_2 <- filter(branches, date > date_break)

ggplot(branches_1) +
  geom_line(aes(x = date, y = current/240, group = label, color = label)) +
  geom_hline(yintercept = 0) + # Draw line along y = 0 to highlight credit vs debit
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Credit and debit of the mercantile branches to the estate of Jan de Oude", 
       x = "Date", y = "Pounds Flemish", color = "Branches")

ggplot(branches_2) +
  geom_line(aes(x = date, y = current/240, group = label, color = label))


### Line graph from single account ###
willem_vd_mey <- accounts_current %>% filter(id == "dfl12_048")

# Can add text to points because there are few
# Repel text a bit more so that it is mostly away from lines
ggplot(willem_vd_mey) +
  geom_line(aes(x = date, y = current/240)) +
  geom_text_repel(aes(x = date, y = current/240, label = vlaams), point.padding = unit(0.5, "lines"))


### Lines for different accounts of an heir plus cumulative

hester_accounts <- accounts %>% 
  filter(inheritance == "Hester") %>% 
  select(id, account)

hester_current <- accounts_current %>% filter(id %in% hester_accounts$id)

hester_cumulative <- hester_current %>% 
  group_by(date) %>% 
  summarise(id = "cumulative", day = sum(denari)) %>% 
  mutate(cumulative = cumsum(day))

## Plot with cumulative
ggplot() + 
  geom_line(data = hester_current, aes(x = date, y = current/240, group = id, color = id)) +
  geom_line(data = hester_cumulative, aes(x = date, y = cumulative/240,  group = id, color = id))


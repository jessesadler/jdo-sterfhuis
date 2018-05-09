### Time series with grouped branches ###

library(tidyverse)
library(xts)
library(timetk)
library(dygraphs)
source("scripts/functions.R")

# Load data
transactions <- read_csv("data/transactions.csv")
accounts <- read_csv("data/accounts.csv")

# Change transactions to denarii and simplify
transactions_d <- transactions %>% 
  mutate(denarii = deb_lsd_d(l, s, d)) %>% 
  select(from, to, date, denarii)

# Get total credit and debit for each accout by date
# Make denarii negative for debit transactions
credit <- transactions_d %>% 
  group_by(from, date) %>% 
  summarise(denarii = sum(denarii)) %>% 
  rename(id = from)
debit <- transactions_d %>% 
  group_by(to, date) %>% 
  summarise(denarii = -sum(denarii)) %>% 
  rename(id = to)

# Get groups from accounts tibble
account_groups <- accounts %>% select(id, group)

branch_accounts <- filter(accounts, type == "Branch") %>% 
  select(id) %>% flatten() %>% as_vector()

# Create running values
# Filter to only include branch accounts
# Left join with account_groups to get grouping information
# Summarize by group rather than id to join together
# the two accounts for each branch
branches_running <- bind_rows(credit, debit) %>% 
  filter(id %in% branch_accounts) %>% 
  left_join(account_groups, by = "id") %>% 
  group_by(group, date) %>% 
  summarise(denarii = sum(denarii)) %>% 
  mutate(current = cumsum(denarii)) %>% 
  select(-denarii) %>% 
  ungroup()

# Plot of branches data
before <- ggplot(branches_running) + 
  geom_line(aes(x = date, y = current/240, group = group, color = group))

### Convert to xts ###

# Could use to_fill_xts and from_fill xts with group

# Widen tbl then convert
branches_wide <- branches_running %>% spread(group, current)

# Convert to xts
branches_xts <- tk_xts(branches_wide, silent = TRUE)

# Fill in values for every day
branches_xts <- na.locf(merge(branches_xts, seq(min(branches_running$date),
                                                max(branches_running$date), by = 1)))

# Convert denarii to pounds rounded down for plotting
branches_xts_l <- floor(branches_xts/240)

# Plot
plot(branches_xts_l)
dygraph(branches_xts_l) %>% dyRangeSelector()

### Return to tibble ###
branches_tbl <- tk_tbl(branches_xts, rename_index = "date")

branches_gather <- branches_tbl %>% gather(id, current, -date)

# Plot
after <- ggplot(branches_gather) + 
  geom_line(aes(x = date, y = current/240, group = id, color = id))

## Comparison of plots
library(patchwork)

before + after

# Plot with only before 16 March 1585
library(lubridate)
date_break <- ymd(15850316)

branches_running %>% 
  filter(date < date_break) %>% 
  ggplot() + 
    geom_line(aes(x = date, y = current/240, group = group, color = group), show.legend = FALSE) + 
    theme_minimal() +
  branches_gather %>% 
    filter(date < date_break) %>% 
  ggplot + 
    geom_line(aes(x = date, y = current/240, group = id, color = id)) +
    theme_minimal()
  
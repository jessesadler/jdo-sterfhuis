## Time series inheritance ##

# Get xts object of running value of cumulative credit of all nine heirs

library(tidyverse)
library(xts)
library(timetk)
library(lubridate)
library(dygraphs)
source("scripts/functions.R")

# This script uses tbl-xts functions

# Load data
transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d"))) %>% 
  select(from:denari, tr_type) %>% 
  rename(l = livre, s = solidi, d = denari)
accounts <- read_csv("data/accounts.csv") %>% 
  select(id, account:location)

inheritance_accounts <- filter(accounts, type == "Inheritance") %>% 
  select(id) %>% flatten() %>% as_vector()

# Create inheritance running with groups from inheritance accounts
inheritance_running <- deb_running_group(transactions, inheritance_accounts)

# To xts
inheritance_xts <- to_fill_xts(inheritance_running, group = group)

# xts in pounds
inheritance_xts_l <- floor(inheritance_xts/240)

# To tibble
inheritance_tbl <- from_fill_xts(inheritance_xts)

# Plot
ggplot(inheritance_tbl) + 
  geom_line(aes(x = date, y = current/240, group = id, color = id))

date_break <- ymd(15850316)

inheritance_tbl %>% 
  filter(date < date_break) %>% 
  ggplot() + 
  geom_line(aes(x = date, y = current/240, group = id, color = id)) +
  theme_minimal()

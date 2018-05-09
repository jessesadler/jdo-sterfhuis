## Time series data ##

# This script shows how to go from tbl with running values to time series object.
# Time series makes it possible to do a number of economic transformations on the data.
# The primary reason to get a time series object is that in plotting normal running accounts objects
# the lines are angled between dates with values, because there is not a value for every day.
# This is problematic, because the amounts are not going down over this time.
# Creating an xts object makes it possible to fill in values for each date.
# This script goes from running accounts to xts, fills in dates, graphs, and changes back to tibble.
# Gives examples with both single and multiple accounts

library(tidyverse)
library(xts)
library(tibbletime)
library(timetk)
library(dygraphs)
source("scripts/functions.R")

# Get running accoungts from running accounts script
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

# Accounts running with date, id, and running sum of denari
# Get rid of denarii variable, which is daily total, because
# Can only have one type of value in time series matrix.
# Keep values in denarii to maintain as much data as possible
accounts_running <- bind_rows(credit, debit) %>% 
  group_by(id, date) %>% 
  summarise(denarii = sum(denarii)) %>% 
  mutate(current = cumsum(denarii)) %>% 
  select(-denarii) %>% 
  ungroup()

### tibbletime ###
# Can turn this into time tibble with tibbletime package
running_tbl <- as_tbl_time(accounts_running, index = date)

# Example with data from cassa

# Prepare data
# Get it so only columns are dates and current on that date
# Do not need id column, since it is all on one day
cassa <- accounts_running %>% 
  filter(id == "dfl12_002") %>% 
  select(date, current)

# Change to class xts
# Need order.by because xts wants dates as rownames
cassa_xts <- as.xts(cassa, order.by = cassa$date)

# Can also use timetk package to change to xts
# This is a nicer way to do it

cassa_xts <- tk_xts(cassa)

# Fill in dates
cassa_xts <- na.locf(merge(cassa_xts, seq(min(cassa$date), max(cassa$date), by = 1)))

# Remove now empty date column if it was present
cassa_xts <- cassa_xts[,-1]

# Can convert denarii to pounds for plotting
# Use division and round to floow to have same pounds
# as would appear if l, s, and d were present
# Want to keep original cassa_xts if you want to transform back to tibble
cassa_xts_l <- floor(cassa_xts/240)

plot(cassa_xts_l)

# dygraph
dygraph(cassa_xts) %>% dyRangeSelector() %>% dyOptions(stackedGraph = TRUE)

### Example of maternal accounts ###
maternal_accounts <- c("dfl12_251", "dfl12_252", "dfl12_253", "dfl12_321", "dfl12_333", "dfl12_295")

maternal_running <- accounts_running %>% 
  filter(id %in% maternal_accounts)

# Widen, turn into xts, fill in dates
maternal_wide <- maternal_running %>% spread(id, current)

maternal_xts <- tk_xts(maternal_wide)

# Fill in values for every day
maternal_xts <- na.locf(merge(maternal_xts, seq(min(maternal_running$date),
                                                max(maternal_running$date), by = 1)))

# Convert denarii to pounds rounded down
maternal_xts_l <- floor(maternal_xts/240)

dygraph(maternal_xts_l) %>% dyRangeSelector()

### Return to tibble and gather ###
maternal_tbl <- tk_tbl(maternal_xts, rename_index = "date")

maternal_gather <- maternal_tbl %>% gather(id, current, -date)

## ggplot ##
library(lubridate)

maternal_gather %>% 
  filter(date < ymd(15850316)) %>% 
  ggplot() + 
  geom_line(aes(x = date, y = current, group = id, color = id)) + 
  theme_minimal()

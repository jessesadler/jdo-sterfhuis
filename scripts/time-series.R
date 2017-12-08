## Example for time series ##

library(xts)

# Get running accoungts from running accounts script
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
# Make denari negative for debit transactions
credit <- transactions_d %>% 
  group_by(from, date) %>% 
  summarise(denari = sum(denari)) %>% 
  rename(id = from)
debit <- transactions_d %>% 
  group_by(to, date) %>% 
  summarise(denari = -sum(denari)) %>% 
  rename(id = to)

# Accounts running with date, id, and current pounds rounded down
# Round down to keep the same number as would appear if s and d were included
accounts_running <- bind_rows(credit, debit) %>% 
  group_by(id, date) %>% 
  summarise(denari = sum(denari)) %>% 
  mutate(current = floor(cumsum(denari)/240)) %>% 
  select(-denari) %>% 
  ungroup()

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

# Can also use timetk package
library(timetk)

cassa_xts <- tk_xts(cassa)

# Fill in dates
cassa_xts <- na.locf(merge(cassa_xts, seq(min(cassa$date), max(cassa$date), by = 1)))

# Remove now empty date column if it was present
cassa_xts <- cassa_xts[,-1]

plot(cassa_xts)

library(dygraphs)
dygraph(cassa_xts) %>% dyRangeSelector() %>% dyOptions(stackedGraph = TRUE)

### Example of maternal accounts ###
maternal_accounts <- c("dfl12_251", "dfl12_252", "dfl12_253", "dfl12_321", "dfl12_333", "dfl12_295")

maternal_running <- accounts_running %>% 
  filter(id %in% maternal_accounts)

# Widen, turn into xts, fill in dates
maternal_wide <- maternal_running %>% spread(id, current)

maternal_xts <- tk_xts(maternal_wide)

maternal_xts <- na.locf(merge(maternal_xts, seq(min(maternal_running$date),
                                                max(maternal_running$date), by = 1)))

dygraph(maternal_xts) %>% dyRangeSelector()

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

## Basic Analysis ##

library(tidyverse)

# Load data
transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d")))
accounts <- read_csv("data/accounts.csv")

# Separate data frames for DFL12 and DFL12bis
transactions12 <- filter(transactions, date <= as.Date("1583-12-26", "%Y-%m-%d"))
transactions12b <- filter(transactions, date > as.Date("1583-12-26", "%Y-%m-%d"))

# Sum for each connection
transactions_sum <- transactions %>% 
  group_by(from, to) %>% 
  summarise(l = sum(livre) + ((sum(solidi) + (sum(denari) %/% 12)) %/% 20),
            s = (sum(solidi) + (sum(denari) %/% 12)) %% 20,
            d = sum(denari) %% 12)


transactions_sum <- deb_sum(transactions)

### Decimal vlams ###

transactions_dec <- mutate(transactions, vlams_dec = livre + solidi / 20 + denari / 240)

credit_dec <- transactions_dec %>% group_by(from) %>% summarise(credit = sum(vlams_dec))
debit_dec <- transactions_dec %>% group_by(to) %>% summarise(debit = sum(vlams_dec))

accounts_sum <- full_join(credit_dec, debit_dec, by = c("from" = "to")) %>% 
  replace_na(list(credit = 0, debit = 0)) %>% 
  rename(id = from)

current <- accounts_sum %>% mutate(current = credit - debit)

open <- filter(current, current >= 0.001 | current <= -0.001) %>% arrange(id)
closed <- filter(current, near(current, 0))

### Vlams ###

## With two steps ##
credit_vlams <- transactions %>% group_by(from) %>% 
  summarise(l = sum(livre), s = sum(solidi), d = sum(denari))
credit_vlams <- mutate(credit_vlams,
                l_c = l + ((s + d %/% 12) %/% 20),
                s_c = (s + d %/% 12) %% 20,
                d_c = d %% 12) %>% 
  select(-(l:d))

## More efficient way ##

credit_vlams <- transactions %>% 
  group_by(from) %>% 
  summarise(
    l_c = sum(livre) + ((sum(solidi) + (sum(denari) %/% 12)) %/% 20),
    s_c = (sum(solidi) + (sum(denari) %/% 12)) %% 20,
    d_c = sum(denari) %% 12)

debit_vlams <- transactions %>% group_by(to) %>% summarise(
  l_d = sum(livre) + ((sum(solidi) + (sum(denari) %/% 12)) %/% 20),
  s_d = (sum(solidi) + (sum(denari) %/% 12)) %% 20,
  d_d = sum(denari) %% 12)

accounts_sum <- full_join(credit_vlams, debit_vlams, by = c("from" = "to")) %>% 
  replace_na(list(l_c = 0, s_c = 0, d_c = 0, l_d = 0, s_d = 0, d_d = 0)) %>% 
  rename(id = from)

current <- mutate(accounts_sum, l = l_c - l_d, s = s_c - s_d, d = d_c - d_d)

open <- select(current, id, l:d) %>% filter(l + s + d != 0) %>% arrange(id)

# Still need to find way to deal with negative numbers in sc and d

# Single account
credit <- filter(transactions, from == "dfl12_112") %>% summarise(
  relation = "credit",
  l = sum(livre) + ((sum(solidi) + (sum(denari) %/% 12)) %/% 20),
  s = (sum(solidi) + (sum(denari) %/% 12)) %% 20,
  d = sum(denari) %% 12)

debit <- filter(transactions, to == "dfl12_112") %>% summarise(
  relation = "debit",
  l = sum(livre) + ((sum(solidi) + (sum(denari) %/% 12)) %/% 20),
  s = (sum(solidi) + (sum(denari) %/% 12)) %% 20,
  d = sum(denari) %% 12)

dfl_080 <- bind_rows(credit, debit)

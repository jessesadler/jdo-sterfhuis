## Basic Analysis ##

library(tidyverse)

# Load data
transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d")))
accounts <- read_csv("data/accounts.csv")

# Separate data frames for DFL12 and DFL12bis
transactions12 <- filter(transactions, date <= as.Date("1583-12-26", "%Y-%m-%d"))
transactions12b <- filter(transactions, date > as.Date("1583-12-26", "%Y-%m-%d"))

### Decimal vlams ###

transactions_dec <- mutate(transactions, vlams_dec = gr + sc / 20 + d / 240)

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
credit_vlams <- transactions %>% group_by(from) %>% summarise(gr = sum(gr), sc = sum(sc), d = sum(d))
credit_vlams <- mutate(credit_vlams,
                gr_c = gr + ((sc + d %/% 12) %/% 20),
                sc_c = (sc + d %/% 12) %% 20,
                d_c = d %% 12) %>% 
  select(-(gr:d))

## More efficient way ##

credit_vlams <- transactions %>% group_by(from) %>% summarise(
  gr_c = sum(gr) + ((sum(sc) + (sum(d) %/% 12)) %/% 20),
  sc_c = (sum(sc) + (sum(d) %/% 12)) %% 20,
  d_c = sum(d) %% 12)

debit_vlams <- transactions %>% group_by(to) %>% summarise(
  gr_d = sum(gr) + ((sum(sc) + (sum(d) %/% 12)) %/% 20),
  sc_d = (sum(sc) + (sum(d) %/% 12)) %% 20,
  d_d = sum(d) %% 12)

accounts_sum <- full_join(credit_vlams, debit_vlams, by = c("from" = "to")) %>% 
  replace_na(list(gr_c = 0, sc_c = 0, d_c = 0, gr_d = 0, sc_d = 0, d_d = 0)) %>% 
  rename(id = from)

current <- mutate(accounts_sum, gr = gr_c - gr_d, sc = sc_c - sc_d, d = d_c - d_d)

open <- select(current, id, gr:d) %>% filter(gr + sc + d != 0) %>% arrange(id)

# Still need to find way to deal with negative numbers in sc and d

# Single account
credit <- filter(transactions, from == "dfl12_080") %>% summarise(
  relation = "credit",
  gr = sum(gr) + ((sum(sc) + (sum(d) %/% 12)) %/% 20),
  sc = (sum(sc) + (sum(d) %/% 12)) %% 20,
  d = sum(d) %% 12)

debit <- filter(transactions, to == "dfl12_080") %>% summarise(
  relation = "debit",
  gr = sum(gr) + ((sum(sc) + (sum(d) %/% 12)) %/% 20),
  sc = (sum(sc) + (sum(d) %/% 12)) %% 20,
  d = sum(d) %% 12)

dfl_080 <- bind_rows(credit, debit)

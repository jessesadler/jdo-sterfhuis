## Basic Analysis ##

library(tidyverse)

# Load data
transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d")))
accounts <- read_csv("data/accounts.csv")

# simplify
transactions <- transactions %>% select(from, to, gr:pounds)

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

credit_vlams <- transactions %>% group_by(from) %>% summarise(gr = sum(gr), sc = sum(sc), d = sum(d))
credit_vlams <- mutate(credit_vlams,
                gr_c = gr + ((sc + d %/% 12) %/% 20),
                sc_c = (sc + d %/% 12) %% 20,
                d_c = d %% 12) %>% 
  select(-(gr:d))

debit_vlams <- transactions %>% group_by(to) %>% summarise(gr = sum(gr), sc = sum(sc), d = sum(d))
debit_vlams <- mutate(debit_vlams,
                       gr_d = gr + ((sc + d %/% 12) %/% 20),
                       sc_d = (sc + d %/% 12) %% 20,
                       d_d = d %% 12) %>% 
  select(-(gr:d))

accounts_sum <- full_join(credit_vlams, debit_vlams, by = c("from" = "to")) %>% 
  replace_na(list(gr_c = 0, sc_c = 0, d_c = 0, gr_d = 0, sc_d = 0, d_d = 0)) %>% 
  rename(id = from)

current <- mutate(accounts_sum, gr = gr_c - gr_d, sc = sc_c - sc_d, d = d_c - d_d)

open <- select(current, id, gr:d) %>% filter(gr + sc + d != 0)

# Still need to find way to deal with negative numbers in sc and d

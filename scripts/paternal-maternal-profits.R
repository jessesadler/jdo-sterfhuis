### Paternal and maternal profits ###

library(tidyverse)
library(lubridate)
source("scripts/functions.R")

transactions <- read_csv("data/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d"))) %>% 
  select(from:to, date:denari) %>% 
  rename(l = livre, s = solidi, d = denari)
accounts <- read_csv("data/accounts.csv") %>% 
  select(id, account:location)

# Data frame with account ids and names to use for joins
account_names <- select(accounts, id, account)

maternal_accounts <- c("dfl12_251", "dfl12_252", "dfl12_253", "dfl12_321", "dfl12_333", "dfl12_295")

### Capital invested in trade in 1578 ###
paternal_capital <- tibble(id = "dfl12_289",
                           account = "Jan de Oude",
                           type = "paternal",
                           l = 34000, s = 0, d = 0,
                           date = ymd("1578-12-31"),
                           denari = deb_lsd_d(34000, 0, 0))
maternal_capital <- tibble(id = maternal_accounts,
                           account = c("Hester", "Cornelia", "Marten", "Steven", "Anna", "Jacques"),
                           type = "maternal",
                           l = c(5333, 5333, 4945, 4335, 3050, 2200),
                           s = c(6, 6, 10, 5, 0, 0), 
                           d = c(8, 8, 7, 6, 0, 0),
                           date = ymd("1578-12-31")) %>% 
  mutate(denari = deb_lsd_d(l, s, d),
         pct = round(denari*100/sum(denari), 4))

movable_capital_1578 <- bind_rows(paternal_capital, maternal_capital) %>% 
  mutate(pct = round(denari*100/sum(denari), 4))

total_denari_1578 <- sum(movable_capital_1578$denari)
movable_capital_1578_total <- tibble(account = "Total 1578",
                                     l = deb_d_livre(total_denari_1578),
                                     s = deb_d_solidi(total_denari_1578),
                                     d = deb_d_denari(total_denari_1578),
                                     date = ymd("1578-12-31"),
                                     denari = total_denari_1578)

### Profits up to 26 December 1583 ###
winninge_verlies_cred1 <- deb_account_credit(transactions, "dfl12_038")
winninge_verlies_deb1 <- deb_account_debit(transactions, "dfl12_038")

paternal_profits1 <- filter(winninge_verlies_deb1, from == "dfl12_289") %>% 
  select(id = from, account:pct) %>% 
  add_column(type = "paternal")

# Paternal profits without household costs of £3644.0.0
paternal_profits1_housecost <- paternal_profits1 %>% 
  mutate(l = l - 3644, denari = denari - deb_lsd_d(3644, 0, 0))

maternal_profits1 <- filter(winninge_verlies_deb1, from %in% maternal_accounts) %>% 
  select(id = from, account:denari) %>% 
  mutate(pct = round(denari*100/sum(denari), 4)) %>% 
  add_column(type = "maternal")

profits_1583 <- bind_rows(paternal_profits1_housecost, maternal_profits1) %>% 
  mutate(pct = round(denari*100/sum(denari), 4))

### Profits up to end of 1594 ###
winninge_verlies_cred2 <- deb_account_credit(transactions, "dfl12_445")
winninge_verlies_deb2 <- deb_account_debit(transactions, "dfl12_445")

# Necessary because accounts not clearly divided between maternal and paternal in dfl12bis
inheritance_accounts <- c("dfl12_251", "dfl12_295", "dfl12_340", "dfl12_343", 
                          "dfl12_344", "dfl12_345", "dfl12_346", "dfl12_347")

profits_1594 <- filter(winninge_verlies_deb2, from %in% inheritance_accounts) %>% 
  mutate(pct = round(denari*100/sum(denari), 4))

maternal_profits2 <- filter(profits_1594, l != 128) %>% 
  select(id = from, account:pct) %>% 
  add_column(type = "maternal") %>% 
  mutate(pct = round(denari*100/sum(denari), 4))
paternal_profits2 <- filter(profits_1594, l == 128) %>% 
  select(id = from, account:pct) %>% 
  add_column(type = "paternal") %>% 
  mutate(pct = round(denari*100/sum(denari), 4))

### Bring together the above to show all capital and profits ###
movable_capital <- bind_rows(movable_capital_1578, profits_1583, profits_1594)

### Summary of maternal and paternal capital and profits ###

### Paternal and maternal capital invested in 1578 ###
paternal_1578 <- select(paternal_capital, -(id:account))
maternal_1578 <- deb_refactor_sum(maternal_capital) %>% 
  mutate(denari = deb_lsd_d(l, s, d)) %>% 
  add_column(date = ymd("1578-12-31"), type = "maternal")

capital_mp_1578 <- bind_rows(paternal_1578, maternal_1578) %>% 
  mutate(pct = round(denari*100/sum(denari), 4))

### Paternal and maternal capital profits in 1583 ###
paternal_profits_sum1 <- paternal_profits1 %>% select(l:denari, type)
maternal_profits_sum1 <- deb_refactor_sum(maternal_profits1) %>% 
  mutate(denari = deb_lsd_d(l, s, d)) %>% 
  add_column(date = ymd("1583-12-15"), type = "maternal")

profits_mp_1583 <- bind_rows(paternal_profits_sum1, maternal_profits_sum1) %>% 
  mutate(pct = round(denari*100/sum(denari), 4))

### Paternal and maternal capital profits in 1594 ###
paternal_profits_sum2 <- deb_refactor_sum(paternal_profits2) %>% 
  mutate(denari = deb_lsd_d(l, s, d)) %>% 
  add_column(date = ymd("1594-10-01"), type = "paternal")
maternal_profits_sum2 <- deb_refactor_sum(maternal_profits2) %>% 
  mutate(denari = deb_lsd_d(l, s, d)) %>% 
  add_column(date = ymd("1594-10-01"), type = "maternal")

profits_mp_1594 <- bind_rows(paternal_profits_sum2, maternal_profits_sum2) %>% 
  select(date, type, l:denari) %>% 
  mutate(pct = round(denari*100/sum(denari), 4))

# Summary of paternal profits to get percentages
profits_1594_pct <- bind_rows(maternal_profits2, paternal_profits_sum2) %>% 
  mutate(pct = round(denari*100/sum(denari), 4))

### Overview of paternal and maternal capital and profits ###
profits_overview <- bind_rows(capital_mp_1578, profits_mp_1583, profits_mp_1594)

### Total capital and profit ###

# 1578
movable_sum_1578 <- deb_refactor_sum(movable_capital_1578) %>% 
  mutate(denari = deb_lsd_d(l, s, d)) %>% 
  add_column(date = ymd("1578-12-31"))

# 1583
movable_sum_1583 <- deb_refactor_sum(profits_1583) %>% 
  mutate(denari = deb_lsd_d(l, s, d)) %>% 
  add_column(date = ymd("1583-12-15"))

# 1594
movable_sum_1594 <- deb_refactor_sum(profits_1594) %>% 
  mutate(denari = deb_lsd_d(l, s, d)) %>% 
  add_column(date = ymd("1594-10-01"))


movable_sums <- bind_rows(movable_sum_1578, movable_sum_1583, movable_sum_1594)

### Capital and profits in denari ###
maternal_1578d <- deb_lsd_d(maternal_sum[1], maternal_sum[2], maternal_sum[3]) %>% 
  as.numeric()
paternal_1578d <- deb_lsd_d(34000, 0, 0)
maternal_1583d <- deb_lsd_d(maternal_profits_sum1[1], maternal_profits_sum1[2],
                            maternal_profits_sum1[3]) %>% 
  as.numeric()
paternal_1583d <- deb_lsd_d(paternal_profits_sum1[1], paternal_profits_sum1[2], 
                            paternal_profits_sum1[3]) %>% 
  as.numeric()
maternal_1594d <- deb_lsd_d(maternal_profits_sum2[1], maternal_profits_sum2[2], 
                            maternal_profits_sum2[3]) %>% 
  as.numeric()
paternal_1594d <- deb_lsd_d(paternal_profits_sum2[1], paternal_profits_sum2[2], 
                            paternal_profits_sum2[3]) %>% 
  as.numeric()

### Take out household costs of £3644 from 1583 paternal profits ###
household_costs <- tibble(l = 3644, s = 0, d = 0, date = ymd("1583-12-15")) %>% 
  mutate(denari = deb_lsd_d(l, s, d))

paternal_1583_profitsd <- paternal_1583d - deb_lsd_d(l = 3644, s = 0, d = 0)
paternal_1583_profits <- deb_d_lsd(paternal_1583_profitsd) %>% 
  add_column(denari = paternal_1583_profitsd,
             date = ymd("1583-12-15"),
             type = "paternal")

profits_mp_1583_h <- bind_rows(paternal_1583_profits, maternal_profits_sum1) %>% 
  mutate(pct = round(denari*100/sum(denari), 4))

### Take out sororal profits from paternal profits and add to maternal profits in 1594 ###
# Cornelia's part in the profits same as Hester's maternal profits
sororal_profits_1594d <- maternal_profits2 %>% 
  filter(id == "dfl12_347") %>% 
  select(denari) %>% 
  as.numeric()
paternal_1594zsd <- paternal_1594d - sororal_profits_1594d
maternal_1594zsd <- maternal_1594d + sororal_profits_1594d

paternal_1594zsd / (paternal_1594zsd + maternal_1594zsd)
maternal_1594zsd / (paternal_1594zsd + maternal_1594zsd)

paternal_1594_profitszsd <- deb_d_lsd(paternal_1594zsd) %>% 
  add_column(denari = paternal_1594zsd,
             date = ymd("1594-10-01"),
             type = "paternal")
maternal_1594_profitszsd <- deb_d_lsd(maternal_1594zsd) %>% 
  add_column(denari = maternal_1594zsd,
             date = ymd("1594-10-01"),
             type = "maternal")

profits_mp_1594zsd <- bind_rows(paternal_1594_profitszsd, maternal_1594_profitszsd) %>% 
  mutate(pct = round(denari*100/sum(denari), 4))

### Paternal and maternal profits without household costs and sororal with maternal ####
profits_overview_alternate <- bind_rows(capital_mp_1578, profits_mp_1583_h, profits_mp_1594zsd)

## Double-Entry Bookkeeping Functions ##

# Requires
library(tidyverse)

# Functions to refactor l, s, and d
deb_livre <- function(l, s, d) {l + ((s + d %/% 12) %/% 20)}
deb_solidi <- function(s, d) {(s + d %/% 12) %% 20}
deb_denari <- function(d) {d %% 12}

# Functions that take sum and refactor l, s, and d
# These are helper functions
deb_l_sum <- function(l, s, d) {sum(l) + ((sum(s) + (sum(d) %/% 12)) %/% 20)}
deb_s_sum <- function(s, d) {(sum(s) + (sum(d) %/% 12)) %% 20}
deb_d_sum <- function(d) {round(sum(d) %% 12, 3)}

# Take pounds, shillings, and pennies
# and refactor to correct limit
deb_refactor <- function(l, s, d) {
  c(
    l = deb_livre(l, s, d),
    s = deb_solidi(s, d),
    d = deb_denari(d))
}

# Same as above but create sum of l, s, d first
# meant to be used with df$l, df$s, df$d
deb_refactor_sum <- function(l, s, d) {
  l = sum(l)
  s = sum(s)
  d = sum(d)
  tibble(
    l = deb_livre(l, s, d),
    s = deb_solidi(s, d),
    d = deb_denari(d))
}

# Refactor with only denari
# Can take postive or negative value
# If negative, returns negative l, s, and d in case one is 0
deb_d_lsd <- function(d) {
  if (d < 0) {
    tibble(
      l = -((-d %/% 12) %/% 20),
      s = -((-d %/% 12) %% 20),
      d = -(-d %% 12)) 
  } else {
  tibble(
    l = (d %/% 12) %/% 20,
    s = (d %/% 12) %% 20,
    d = d %% 12)
  }
}

deb_d_lsd_print <- function(d) {
  if_else(d < 0, 
          paste0("-£", (-d %/% 12) %/% 20, ".", (-d %/% 12) %% 20, ".", round(-d %% 12, 3)),
          paste0("£", (d %/% 12) %/% 20, ".", (d %/% 12) %% 20, ".", round(d %% 12, 3)))
}

# Take lsd and return denari
deb_lsd_d <- function(l, s, d) {
  l * 240 + s * 12 + d
}

# Exchange rate by shillings
# This is set up to go from sterling to vlaams
deb_exchange_s <- function(l, s, d, rate = 31) {
  denari <- deb_lsd_d(l, s, d) * rate/20
  deb_d_lsd(denari)
}

# Exchange rate of ducats
# This is set up to go from ducats to vlaams
deb_exchange_ducats <- function(ducats, denari = 0, rate = 96) {
  denari <- denari/24
  deb_d_lsd((ducats + denari) * rate)
}

## Calculate interest
# Works by mutating lsd to denari,
# calculating interest, then back to lsd
deb_interest <- function(l, s, d, interest = 0.0625, years = 1) {
  per_year <- interest * deb_lsd_d(l, s, d)
  denari_interest <- years * per_year
  deb_d_lsd(denari_interest)
}

# Summarise a data frame with sum of l, s, and d
# groups by from and to in order to get all transaction types
# sums the pounds, shillings, and pence while refactoring
deb_sum_df <- function(df) {
  df %>% 
    group_by(from, to) %>% 
    summarise(
      l = deb_l_sum(l, s, d),
      s = deb_s_sum(s, d),
      d = deb_d_sum(d))
}

## Single account ##

# Take dataframe and account id
# Returns sum of credit and debit,
# and current amount by subtracting debit from credit
deb_account <- function(df, id) {
  credit <- filter(df, from == id) %>% 
    summarise(
      relation = "credit",
      l = deb_l_sum(l, s, d),
      s = deb_s_sum(s, d),
      d = deb_d_sum(d))
  
  debit <- filter(df, to == id) %>% 
    summarise(
      relation = "debit",
      l = deb_l_sum(l, s, d),
      s = deb_s_sum(s, d),
      d = deb_d_sum(d))
  
  credit_d <- deb_lsd_d(credit$l, credit$s, credit$d)
  debit_d <- deb_lsd_d(debit$l, debit$s, debit$d)
  
  denari <- (credit_d - debit_d)
  
  current <- bind_cols(relation = "current", deb_d_lsd(denari))
  
  bind_rows(credit, debit, current)
}

## Create current data frame ##

# Take a data frame with l, s, and d columns and
# return a data frame with summed credit subtracted from summed debit
# Resulting data frame has l, s, and d columns for debit, credit, and current
deb_current <- function(df) {
  credit <- df %>% group_by(from) %>% 
    summarise(
      l_c = deb_l_sum(l, s, d),
      s_c = deb_s_sum(s, d),
      d_c = deb_d_sum(d)) %>% 
    mutate(denari_c = deb_lsd_d(l_c, s_c, d_c))
  
  debit <- df %>% group_by(to) %>% 
    summarise(
      l_d = deb_l_sum(l, s, d),
      s_d = deb_s_sum(s, d),
      d_d = deb_d_sum(d)) %>% 
    mutate(denari_d = deb_lsd_d(l_d, s_d, d_d))
  
  accounts_sum <- full_join(credit, debit, by = c("from" = "to")) %>% 
    replace_na(list(l_c = 0, s_c = 0, d_c = 0, l_d = 0, s_d = 0, d_d = 0,
                    denari_c = 0, denari_d = 0)) %>% 
    rename(id = from)
  
  accounts_sum %>% mutate(denari_current = denari_c - denari_d) %>% 
    mutate(denari_pos = if_else(denari_current < 0, -denari_current, denari_current)) %>% 
    mutate(relation = if_else(denari_current < 0, "debit", "credit")) %>% 
    mutate(l = (denari_pos %/% 12) %/% 20,
           s = (denari_pos %/% 12) %% 20,
           d = denari_pos %% 12) %>% 
    select(-starts_with("denari"))
}

deb_current_print <- function(df) {
  
  # Change transactions to denari and simplify
  df <- df %>% 
    mutate(denari = deb_lsd_d(l, s, d))
  
  # Get total credit and debit for each accout by date
  credit <- df %>% group_by(from) %>% 
    summarise(denari_c = sum(denari))
  debit <- df %>% group_by(to) %>% 
    summarise(denari_d = -sum(denari))
  
  accounts_sum <- full_join(credit, debit, by = c("from" = "to")) %>% 
    replace_na(list(denari_c = 0, denari_d = 0))
  
  denari_sum <- accounts_sum %>% mutate(current_d = denari_c + denari_d)
  
  denari_sum %>% mutate(credit = deb_d_lsd_print(denari_c),
                        debit = deb_d_lsd_print(denari_d),
                        current = deb_d_lsd_print(current_d)) %>% 
    select(-contains("_"))
}

## Create tibble of open accounts ##

# Uses deb_current() function to get current amount
# Selects only current l, s, and d and filters out accounts that are even
deb_open <- function(df) {
  df %>% deb_current() %>% 
  select(id, relation:d) %>% 
    filter(l + s + d != 0) %>% 
    arrange(id)
}

### Create tibble of credit and debit transactions from one account or vector of accounts ###
# This function needs account_names tibble to exist. If it does not, function gives a warning
# Can be run on transactions tibble or transactions_sum tibble
# Single accounts must have quotations around account id
# These functions make percentage from denari

deb_sub_credit <- function(df, id){
  if (exists("account_names")) {
    df %>% filter(from %in% id) %>%
      left_join(account_names, by = c("to" = "id")) %>% 
      select(from:to, account, l:d, everything()) %>% 
      mutate(denari = deb_lsd_d(l, s, d),
             pct = round(denari*100/sum(denari), 2)) %>% 
      arrange(desc(l))
  } else {
    warning("account_names tibble needs to exist")
  } 
}

deb_sub_debit <- function(df, id){
  if (exists("account_names")) {
    df %>% filter(to %in% id) %>%
      left_join(account_names, by = c("from" = "id")) %>% 
      select(from:to, account, l:d, everything()) %>% 
      mutate(denari = deb_lsd_d(l, s, d),
             pct = round(denari*100/sum(denari), 2)) %>% 
      arrange(desc(l))
  } else {
    warning("account_names tibble needs to exist")
  } 
}

--------------------------------------------------------------------------------


### Extra Functions ###

# Refactor to tibble
deb_refactor_tb <- function(l, s, d) {
  tibble(
    l = deb_livre(l, s, d),
    s = deb_solidi(s, d),
    d = deb_denari(d))
}

# Calculate total credit of an account
deb_account_c <- function(df, id) {
  filter(df, from == id) %>% deb_sum()
}

# Calculate total debt of an account
deb_account_d <- function(df, id) {
  filter(df, to == id) %>% deb_sum()
}

## Functions for negative s and d after subtraction ##
deb_neg_sd <- function(l, s, d) {c(l - 1, s + 19, d + 12)} # negative s and d
deb_neg_s <- function(l, s, d) {c(l - 1, s + 20, d)} # negative s
deb_neg_d <- function(l, s, d) {c(l, s - 1, d + 12)} # negative d with positive s

deb_neg_l <- function(l, s, d) {c(l + 1, -(s) + 19, -(d) + 12)} # l is negative and s, d are positive
deb_neg_ls <- function(l, s, d) {c(l, -(s) - 1, -(d) + 12)} # l is negative or  and s is negative
# d has to be more than 0 if l is 0
deb_neg_ld <- function(l, s, d) {c(l + 1, -(s) + 20, -(d))} # l is negative and d is negative
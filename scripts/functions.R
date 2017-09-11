## Double-Entry Bookkeeping Functions ##

# Requires
library(tidyverse)

# Functions to refactor gr, sc, and d
deb_gr <- function(gr, sc, d) {gr + ((sc + d %/% 12) %/% 20)}
deb_sc <- function(sc, d) {(sc + d %/% 12) %% 20}
deb_d <- function(d) {d %% 12}

# Functions that take sum and refactor gr, sc, and d
# These are helper functions
deb_gr_sum <- function(gr, sc, d) {sum(gr) + ((sum(sc) + (sum(d) %/% 12)) %/% 20)}
deb_sc_sum <- function(sc, d) {(sum(sc) + (sum(d) %/% 12)) %% 20}
deb_d_sum <- function(d) {sum(d) %% 12}

# Take sum of pounds, schillings, and pennies
# and refactor to correct limit
deb_refactor <- function(gr, sc, d) {
  c(
    gr = deb_gr(gr, sc, d),
    sc = deb_sc(sc, d),
    d = deb_d(d))
}

# Same as above but create a tibble
deb_refactor_tb <- function(gr, sc, d) {
  tibble(
    gr = deb_gr(gr, sc, d),
    sc = deb_sc(sc, d),
    d = deb_d(d))
}

# Refactor with only denari
# Can take postive or negative value
# If negative, returns negative gr, sc, and d in case one is 0
deb_d_lsd <- function(d) {
  if (d < 0) {
    d <- -d
    tibble(
      gr = -((d %/% 12) %/% 20),
      sc = -((d %/% 12) %% 20),
      d = -(d %% 12)) 
  } else {
  tibble(
    gr = (d %/% 12) %/% 20,
    sc = (d %/% 12) %% 20,
    d = d %% 12)
  }
}

# Take lsd and return denari
deb_lsd_d <- function(gr, sc, d) {
  gr * 240 + sc * 12 + d
}

# Exchange rate by shillings
# This is set up to go from sterling to vlaams
deb_exchange_s <- function(gr, sc, d, rate = 31) {
  denari <- deb_lsd_d(gr, sc, d) * rate/20
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
deb_interest <- function(gr, sc, d, interest = 0.0625, years = 1) {
  per_year <- interest * deb_lsd_d(gr, sc, d)
  denari_interest <- years * per_year
  deb_d_lsd(denari_interest)
}

## Functions for negative sc and d after subtraction ##

deb_neg_scd <- function(gr, sc, d) {c(gr - 1, sc + 19, d + 12)} # negative sc and d
deb_neg_sc <- function(gr, sc, d) {c(gr - 1, sc + 20, d)} # negative sc
deb_neg_d <- function(gr, sc, d) {c(gr, sc - 1, d + 12)} # negative d with positive sc

deb_neg_gr <- function(gr, sc, d) {c(gr + 1, -(sc) + 19, -(d) + 12)} # gr is negative and sc, d are positive
deb_neg_grsc <- function(gr, sc, d) {c(gr, -(sc) - 1, -(d) + 12)} # gr is negative or  and sc is negative
  # d has to be more than 0 if gr is 0
deb_neg_grd <- function(gr, sc, d) {c(gr + 1, -(sc) + 20, -(d))} # gr is negative and d is negative

# Summarise a data frame with sum of gr, sc, and d
# groups by from and to in order to get all transaction types
# sums the pounds, shillings, and pence while refactoring
deb_sum_df <- function(df) {
  df %>% 
    group_by(from, to) %>% 
    summarise(
      gr = deb_gr_sum(gr, sc, d),
      sc = deb_sc_sum(sc, d),
      d = deb_d_sum(d))
}

## Single account ##

# Take dataframe and account id
# Returns sum of credit and debit,
# and current amount by subtracting debit from credit
deb_account <- function(df, id) {
  credit <- filter(df, from == id) %>% summarise(
    relation = "credit",
    gr = deb_gr_sum(gr, sc, d),
    sc = deb_sc_sum(sc, d),
    d = deb_d_sum(d))
  
  debit <- filter(df, to == id) %>% summarise(
    relation = "debit",
    gr = deb_gr_sum(gr, sc, d),
    sc = deb_sc_sum(sc, d),
    d = deb_d_sum(d))
  
  credit_d <- deb_lsd_d(credit$gr, credit$sc, credit$d)
  debit_d <- deb_lsd_d(debit$gr, debit$sc, debit$d)
  
  denari <- (credit_d - debit_d)
  
  current <- bind_cols(relation = "current", deb_d_lsd(denari))
  
  bind_rows(credit, debit, current)
}

# Calculate total credit of an account
deb_account_c <- function(df, id) {
  filter(df, from == id) %>% deb_sum()
}

# Calculate total debt of an account
deb_account_d <- function(df, id) {
  filter(df, to == id) %>% deb_sum()
}

## Create current data frame ##

# Take a data frame with gr, sc, and d columns and
# return a data frame with summed credit subtracted from summed debit
# Resulting data frame has gr, sc, and d columns for debit, credit, and current
deb_current <- function(df) {
  credit <- df %>% group_by(from) %>% summarise(
    gr_c = deb_gr_sum(gr, sc, d),
    sc_c = deb_sc_sum(sc, d),
    d_c = deb_d_sum(d)) %>% 
    mutate(denari_c = deb_lsd_d(gr_c, sc_c, d_c))
  
  debit <- df %>% group_by(to) %>% summarise(
    gr_d = deb_gr_sum(gr, sc, d),
    sc_d = deb_sc_sum(sc, d),
    d_d = deb_d_sum(d)) %>% 
    mutate(denari_d = deb_lsd_d(gr_d, sc_d, d_d))
  
  accounts_sum <- full_join(credit, debit, by = c("from" = "to")) %>% 
    replace_na(list(gr_c = 0, sc_c = 0, d_c = 0, gr_d = 0, sc_d = 0, d_d = 0,
                    denari_c = 0, denari_d = 0)) %>% 
    rename(id = from)
  
  accounts_sum %>% mutate(denari_current = denari_c - denari_d) %>% 
    mutate(denari_pos = if_else(denari_current < 0, -denari_current, denari_current)) %>% 
    mutate(relation = if_else(denari_current < 0, "debit", "credit")) %>% 
    mutate(gr = (denari_pos %/% 12) %/% 20,
           sc = (denari_pos %/% 12) %% 20,
           d = denari_pos %% 12) %>% 
    select(-starts_with("denari"))
}

## Create tibble of open accounts ##

# Uses deb_current() function to get current amount
# Selects only current gr, sc, and d and filters out accounts that are even
deb_open <- function(df) {
  df %>% deb_current() %>% 
  select(id, relation:d) %>% 
    filter(gr + sc + d != 0) %>% 
    arrange(id)
}

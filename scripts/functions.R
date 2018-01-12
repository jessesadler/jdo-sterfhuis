## Double-Entry Bookkeeping Functions ##

# Requires
library(dplyr)

### Refactor functions ###

# Helper functions to refactor l, s, and d
deb_livre <- function(l, s, d) {l + ((s + d %/% 12) %/% 20)}
deb_solidi <- function(s, d) {(s + d %/% 12) %% 20}
deb_denari <- function(d) {d %% 12}

# Take l, s, and d and refactors to correct limit of 20s and 12d
deb_refactor <- function(l, s, d, vector = FALSE) {
  if (vector == FALSE) {
    tibble(
      l = deb_livre(l, s, d),
      s = deb_solidi(s, d),
      d = deb_denari(d))
  } else {
    c(
      l = deb_livre(l, s, d),
      s = deb_solidi(s, d),
      d = deb_denari(d))
  }
}

# Helper functions that take sum of data frame and refactor l, s, and d
deb_l_sum <- function(l, s, d) {sum(l) + ((sum(s) + (sum(d) %/% 12)) %/% 20)}
deb_s_sum <- function(s, d) {(sum(s) + (sum(d) %/% 12)) %% 20}
deb_d_sum <- function(d) {round(sum(d) %% 12, 3)}

# Sum of l, s, d from data frame
# Creates a new data frame with one row of l, s, d columns
deb_refactor_sum <- function(df, l, s, d) {
  df %>% 
    summarise(l = deb_l_sum(l, s, d),
              s = deb_s_sum(s, d),
              d = deb_d_sum(d))
}

# Similar to above, but with group_by()
# Sum of l, s, d from data frame with `group_by()` of credit and debit account
# Uses tidyeval, so can have different names for credit and debit columns
deb_group_sum <- function(df, credit = from, debit = to) {
  credit <- enquo(credit)
  debit <- enquo(debit)
  df %>% 
    group_by(!!credit, !!debit) %>% 
    summarise(
      l = deb_l_sum(l, s, d),
      s = deb_s_sum(s, d),
      d = deb_d_sum(d))
}

### Refactor with through denari ###
# Functions to go between d and lsd

# Take lsd and return denari
deb_lsd_d <- function(l, s, d) {
  l * 240 + s * 12 + d
}

# Helper functions to return livre, solidi, and denari separately from denari
# Can take postive or negative value
# If negative, returns negative l, s, and d in case one is 0
deb_d_livre <- function(d) {
  if_else(d < 0, -((-d %/% 12) %/% 20), (d %/% 12) %/% 20)
}

deb_d_solidi <- function(d) {
  if_else(d < 0, -((-d %/% 12) %% 20), (d %/% 12) %% 20)
}
deb_d_denari <- function(d) {
  if_else(d < 0, -(-d %% 12), d %% 12)
}

# Create tibble with lsd from d
deb_d_lsd <- function(d, vector = FALSE) {
  if (vector == FALSE) {
    tibble(
      l = deb_d_livre(d),
      s = deb_d_solidi(d),
      d = deb_d_denari(d))
  } else {
    c(
      l = deb_d_livre(d),
      s = deb_d_solidi(d),
      d = deb_d_denari(d))
  }
}

deb_d_lsd_print <- function(d) {
  if_else(d < 0, 
          paste0("-£", (-d %/% 12) %/% 20, ".", (-d %/% 12) %% 20, ".", round(-d %% 12, 3)),
          paste0("£", (d %/% 12) %/% 20, ".", (d %/% 12) %% 20, ".", round(d %% 12, 3)))
}

# Exchange rate by shillings
# This is set up to go from sterling to vlaams
deb_exchange_s <- function(l, s, d, rate = 31) {
  denari <- deb_lsd_d(l, s, d) * rate/20
  deb_d_lsd(denari)
}

# Exchange rate of ducats
# From ducats to lsd with rate as denarius per ducat
deb_ducats_lsd <- function(ducats, denari = 0, rate = 96) {
  denari <- denari/24
  deb_d_lsd((ducats + denari) * rate)
}

# From lsd to ducats with rate as denarius per ducat
deb_lsd_ducats <- function(l, s, d, rate = 96) {
  denari <- deb_lsd_d(l, s, d) * 24/rate
  tibble(ducats = denari %/% 24,
         denaro = denari %% 24)
}

## Calculate interest
# Works by mutating lsd to denari,
# calculating interest, then back to lsd
deb_interest <- function(l, s, d, interest = 0.0625, years = 1) {
  per_year <- interest * deb_lsd_d(l, s, d)
  denari_interest <- years * per_year
  deb_d_lsd(denari_interest)
}

## Single accounts ##

# Take dataframe and account id
# Returns sum of credit and debit,
# and current amount by subtracting debit from credit
deb_account <- function(df, account, credit = from, debit = to) {
  credit <- enquo(credit)
  debit <- enquo(debit)
  
  credit <- df %>% 
    filter((!!credit) == account) %>% 
    summarise(
      relation = "credit",
      l = deb_l_sum(l, s, d),
      s = deb_s_sum(s, d),
      d = deb_d_sum(d))
  
  debit <- df %>% 
    filter((!!debit) == account) %>% 
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

### Create tibble of credit and debit transactions from one account or vector of accounts ###
# This function needs accounts tibble to exist. If it does not, function gives a warning
# Can be run on transactions tibble or transactions_sum tibble
# Single accounts must have quotations around account id
# These functions make percentage from denari

deb_account_credit <- function(df, id){
  if (exists("accounts")) {
    
    account_names <- select(accounts, id, account)
    
    df %>% filter(from %in% id) %>%
      left_join(account_names, by = c("to" = "id")) %>% 
      select(from:to, account, l:d, date) %>% 
      mutate(denari = deb_lsd_d(l, s, d),
             pct = round(denari*100/sum(denari), 2)) %>% 
      arrange(desc(denari))
  } else {
    warning("accounts tibble needs to exist")
  } 
}

deb_account_debit <- function(df, id){
  if (exists("accounts")) {
    
    account_names <- select(accounts, id, account)
    
    df %>% filter(to %in% id) %>%
      left_join(account_names, by = c("from" = "id")) %>% 
      select(from:to, account, l:d, date) %>% 
      mutate(denari = deb_lsd_d(l, s, d),
             pct = round(denari*100/sum(denari), 2)) %>% 
      arrange(desc(denari))
  } else {
    warning("accounts tibble needs to exist")
  } 
}

### Create current and open data frames ###

# Take a data frame with l, s, and d columns and
# return a data frame with summed credit subtracted from summed debit
# Resulting data frame has l, s, and d columns for debit, credit, and current

deb_current <- function(df, credit = from, debit = to) {
  credit <- enquo(credit)
  debit <- enquo(debit)
  
  credits <- df %>% 
    group_by(!!credit) %>% 
    summarise(
      relation = "credit",
      l = deb_l_sum(l, s, d),
      s = deb_s_sum(s, d),
      d = deb_d_sum(d)) %>% 
    mutate(denari = deb_lsd_d(l, s, d)) %>% 
    rename(id = !!credit)
  
  debits <- df %>% 
    group_by(!!debit) %>% 
    summarise(
      relation = "debit",
      l = deb_l_sum(l, s, d),
      s = deb_s_sum(s, d),
      d = deb_d_sum(d)) %>% 
    mutate(denari = -(deb_lsd_d(l, s, d))) %>% 
    rename(id = !!debit)
  
  accounts_sum <- bind_rows(credits, debits)
  
  current <- accounts_sum %>% 
    group_by(id) %>% 
    summarise(
      relation = "current",
      denari = sum(denari),
      l = deb_d_livre(denari),
      s = deb_d_solidi(denari),
      d = deb_d_denari(denari))
  
  bind_rows(accounts_sum, current) %>% 
    arrange(id)
}

## Create tibble of open accounts ##

# Uses deb_current() function to get current amount
# Changes relation to either debit or credit

deb_open <- function(df) {
  df %>% deb_current() %>% 
    filter(relation == "current" & denari != 0) %>% 
    mutate(relation = if_else(denari < 0, "debit", "credit"))
}

### Current and open print functions ###
# Current and open data frames, but with
# credit, debit, and current values printed out

deb_current_print <- function(df) {
  
  # Change transactions to denari and simplify
  df <- df %>% 
    mutate(denari = deb_lsd_d(l, s, d))
  
  # Get total credit and debit for each accout by date
  credit <- df %>% 
    group_by(from) %>% 
    summarise(denari_c = sum(denari))
  debit <- df %>% group_by(to) %>% 
    summarise(denari_d = -sum(denari))
  
  accounts_sum <- full_join(credit, debit, by = c("from" = "to")) %>% 
    replace_na(list(denari_c = 0, denari_d = 0))
  
  denari_sum <- accounts_sum %>% 
    mutate(current_d = denari_c + denari_d)
  
  denari_sum %>% 
    mutate(credit = deb_d_lsd_print(denari_c),
           debit = deb_d_lsd_print(denari_d),
           current = deb_d_lsd_print(current_d)) %>% 
    select(-contains("_"), id = from)
}

deb_open_print <- function(df) {
  df %>% deb_current_print() %>% 
    filter(current != "£0.0.0" & current != "-£0.0.0")
}
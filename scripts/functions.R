## Double-Entry Bookkeeping Functions ##

# Requires
library(dplyr)
library(tibble)

### Refactor functions ###

# Helper functions to refactor l, s, and d
deb_librae <- function(l, s, d) {l + ((s + d %/% 12) %/% 20)}
deb_solidi <- function(s, d) {(s + d %/% 12) %% 20}
deb_denarii <- function(d) {d %% 12}

# Take l, s, and d and refactors to correct limit of 20s and 12d
deb_refactor <- function(l, s, d, vector = FALSE) {
  if (vector == FALSE) {
    tibble(
      l = deb_librae(l, s, d),
      s = deb_solidi(s, d),
      d = deb_denarii(d))
  } else {
    c(
      l = deb_librae(l, s, d),
      s = deb_solidi(s, d),
      d = deb_denarii(d))
  }
}

# Helper functions that take sum of data frame and refactor l, s, and d
deb_l_sum <- function(l, s, d) {sum(l) + ((sum(s) + (sum(d) %/% 12)) %/% 20)}
deb_s_sum <- function(s, d) {(sum(s) + (sum(d) %/% 12)) %% 20}
deb_d_sum <- function(d) {round(sum(d) %% 12, 3)}

# Sum of l, s, d from data frame
# Adds lsd columns and refactors
deb_lsd_sum <- function(df, l, s, d) {
    summarise(df,
              l = deb_l_sum(l, s, d),
              s = deb_s_sum(s, d),
              d = deb_d_sum(d))
}

# Similar to above, but with group_by(). This is not really necessary.
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

# Convert lsd to l with decimal
deb_lsd_decimal <- function(l, s, d) {
  l + s/20 + d/240
}

### Refactor through denarii ###
# Functions to go between d and lsd

# Take lsd and return denarii
deb_lsd_d <- function(l, s, d) {
  l * 240 + s * 12 + d
}

# Helper functions to return librae, solidi, and denarii separately from denarii
# Can take postive or negative value
# If negative, returns negative l, s, and d in case one is 0
deb_d_librae <- function(d) {
  if_else(d < 0, -((-d %/% 12) %/% 20), (d %/% 12) %/% 20)
}

deb_d_solidi <- function(d) {
  if_else(d < 0, -((-d %/% 12) %% 20), (d %/% 12) %% 20)
}
deb_d_denarii <- function(d) {
  if_else(d < 0, -(-d %% 12), d %% 12)
}

# Create tibble with lsd from d
deb_d_lsd <- function(d, vector = FALSE) {
  if (vector == FALSE) {
    tibble(
      l = deb_d_librae(d),
      s = deb_d_solidi(d),
      d = deb_d_denarii(d))
  } else {
    c(
      l = deb_d_librae(d),
      s = deb_d_solidi(d),
      d = deb_d_denarii(d))
  }
}

# Print out lsd form
deb_d_lsd_print <- function(d) {
  if_else(d < 0, 
          paste0("-£", (-d %/% 12) %/% 20, ".", (-d %/% 12) %% 20, ".", round(-d %% 12, 3)),
          paste0("£", (d %/% 12) %/% 20, ".", (d %/% 12) %% 20, ".", round(d %% 12, 3)))
}

# Convert denarii to l with decimal
deb_d_lsd_decimal <- function(d) {
  deb_d_librae(d) + deb_d_solidi(d)/20 + deb_d_denarii(d)/240
}

# Exchange rate by shillings
# This is set up to go from sterling to vlaams
deb_exchange_s <- function(l, s, d, rate = 31) {
  denarii <- deb_lsd_d(l, s, d) * rate/20
  deb_d_lsd(denarii)
}

# Exchange rate of ducats
# From ducats to lsd with rate as denarii per ducat
deb_ducats_lsd <- function(ducats, deniers = 0, rate = 96) {
  deniers <- deniers/24
  deb_d_lsd((ducats + deniers) * rate)
}

# From lsd to ducats with rate as denarii per ducat
deb_lsd_ducats <- function(l, s, d, rate = 96) {
  denarii <- deb_lsd_d(l, s, d) * 24/rate
  tibble(ducats = denarii %/% 24,
         deniers = denarii %% 24)
}

## Calculate interest
# Works by mutating lsd to denarii,
# calculating interest, then back to lsd
deb_interest <- function(l, s, d, interest = 0.0625, years = 1) {
  denarii_interest <- interest * deb_lsd_d(l, s, d) * per_year
  deb_d_lsd(denarii_interest)
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
  
  denarii <- (credit_d - debit_d)
  
  current <- bind_cols(relation = "current", deb_d_lsd(denarii))
  
  bind_rows(credit, debit, current)
}

### Create tibble of credit and debit transactions from one account or vector of accounts ###
# This function needs accounts tibble to exist. If it does not, function gives a warning
# Can be run on transactions tibble or transactions_sum tibble
# Single accounts must have quotations around account id
# These functions make percentage from denarii

deb_account_credit <- function(df, id){
  if (exists("accounts")) {
    
    account_names <- select(accounts, id, account)
    
    df %>% filter(from %in% id) %>%
      left_join(account_names, by = c("to" = "id")) %>% 
      select(from:to, account, l:d, date) %>% 
      mutate(denarii = deb_lsd_d(l, s, d),
             pct = round(denarii*100/sum(denarii), 2)) %>% 
      arrange(desc(denarii))
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
      mutate(denarii = deb_lsd_d(l, s, d),
             pct = round(denarii*100/sum(denarii), 2)) %>% 
      arrange(desc(denarii))
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
    mutate(denarii = deb_lsd_d(l, s, d)) %>% 
    rename(id = !!credit)
  
  debits <- df %>% 
    group_by(!!debit) %>% 
    summarise(
      relation = "debit",
      l = deb_l_sum(l, s, d),
      s = deb_s_sum(s, d),
      d = deb_d_sum(d)) %>% 
    mutate(denarii = -(deb_lsd_d(l, s, d))) %>% 
    rename(id = !!debit)
  
  accounts_sum <- bind_rows(credits, debits)
  
  current <- accounts_sum %>% 
    group_by(id) %>% 
    summarise(
      relation = "current",
      denarii = sum(denarii),
      l = deb_d_librae(denarii),
      s = deb_d_solidi(denarii),
      d = deb_d_denarii(denarii))
  
  bind_rows(accounts_sum, current) %>% 
    arrange(id)
}

## Create tibble of open accounts ##

# Uses deb_current() function to get current amount
# Changes relation to either debit or credit

deb_open <- function(df) {
  df %>% deb_current() %>% 
    filter(relation == "current" & denarii != 0) %>% 
    mutate(relation = if_else(denarii < 0, "debit", "credit"))
}

### Current and open print functions ###
# Current and open data frames, but with
# credit, debit, and current values printed out

deb_current_print <- function(df) {
  
  # Change transactions to denarii and simplify
  df <- df %>% 
    mutate(denarii = deb_lsd_d(l, s, d))
  
  # Get total credit and debit for each accout by date
  credit <- df %>% 
    group_by(from) %>% 
    summarise(denarii_c = sum(denarii))
  debit <- df %>% group_by(to) %>% 
    summarise(denarii_d = -sum(denarii))
  
  accounts_sum <- full_join(credit, debit, by = c("from" = "to")) %>% 
    replace_na(list(denarii_c = 0, denarii_d = 0))
  
  denarii_sum <- accounts_sum %>% 
    mutate(current_d = denarii_c + denarii_d)
  
  denarii_sum %>% 
    mutate(credit = deb_d_lsd_print(denarii_c),
           debit = deb_d_lsd_print(denarii_d),
           current = deb_d_lsd_print(current_d)) %>% 
    select(-contains("_"), id = from)
}

deb_open_print <- function(df) {
  df %>% deb_current_print() %>% 
    filter(current != "£0.0.0" & current != "-£0.0.0")
}
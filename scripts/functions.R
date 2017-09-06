## Double-Entry Bookkeeping Functions ##

# Functions to refactor gr, sc, and d
deb_d <- function(d) {d %% 12}
deb_sc <- function(sc, d) {(sc + d %/% 12) %% 20}
deb_gr <- function(gr, sc, d) {gr + ((sc + d %/% 12) %/% 20)}

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
deb_refactor_d <- function(d) {
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

## Functions for negative sc and d after subtraction ##

deb_neg_scd <- function(gr, sc, d) {c(gr - 1, sc + 19, d + 12)} # negative sc and d
deb_neg_sc <- function(gr, sc, d) {c(gr - 1, sc + 20, d)} # negative sc
deb_neg_d <- function(gr, sc, d) {c(gr, sc - 1, d + 12)} # negative d with positive sc

deb_neg_gr <- function(gr, sc, d) {c(gr + 1, -(sc) + 19, -(d) + 12)} # gr is negative and sc, d are positive
deb_neg_grsc <- function(gr, sc, d) {c(gr, -(sc) - 1, -(d) + 12)} # gr is negative or  and sc is negative
  # d has to be more than 0 if gr is 0
deb_neg_grd <- function(gr, sc, d) {c(gr + 1, -(sc) + 20, -(d))} # gr is negative and d is negative

# Summarise a data frame
# Add gr, sc, d from a data frame and refactor
# Does not do group_by()
deb_sum <- function(df) {
  summarise(df,
   gr = sum(gr) + ((sum(sc) + (sum(d) %/% 12)) %/% 20),
   sc = (sum(sc) + (sum(d) %/% 12)) %% 20,
   d = sum(d) %% 12)
}

## Single account ##

# Take dataframe and account id
# Returns sum of credit and debit,
# and current amount by subtracting debit from credit
deb_account <- function(df, id) {
  credit <- filter(df, from == id) %>% summarise(
    relation = "credit",
    gr = sum(gr) + ((sum(sc) + (sum(d) %/% 12)) %/% 20),
    sc = (sum(sc) + (sum(d) %/% 12)) %% 20,
    d = sum(d) %% 12)
  
  debit <- filter(df, to == id) %>% summarise(
    relation = "debit",
    gr = sum(gr) + ((sum(sc) + (sum(d) %/% 12)) %/% 20),
    sc = (sum(sc) + (sum(d) %/% 12)) %% 20,
    d = sum(d) %% 12)
  
  credit_dec <- credit$gr + credit$sc / 20 + credit$d / 240
  debit_dec <- debit$gr + debit$sc / 20 + debit$d / 240
  
  denari <- (credit_dec - debit_dec)*240
  
  current <- bind_cols(relation = "current", deb_refactor_d(denari))
  
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
    gr_c = sum(gr) + ((sum(sc) + (sum(d) %/% 12)) %/% 20),
    sc_c = (sum(sc) + (sum(d) %/% 12)) %% 20,
    d_c = sum(d) %% 12)
  
  debit <- df %>% group_by(to) %>% summarise(
    gr_d = sum(gr) + ((sum(sc) + (sum(d) %/% 12)) %/% 20),
    sc_d = (sum(sc) + (sum(d) %/% 12)) %% 20,
    d_d = sum(d) %% 12)
  
  accounts_sum <- full_join(credit, debit, by = c("from" = "to")) %>% 
    replace_na(list(gr_c = 0, sc_c = 0, d_c = 0, gr_d = 0, sc_d = 0, d_d = 0)) %>% 
    rename(id = from)
  
  mutate(accounts_sum, gr = gr_c - gr_d, sc = sc_c - sc_d, d = d_c - d_d)
}

## Create tibble of open accounts ##

# Uses deb_current() function to get current amount
# Selects only current gr, sc, and d and filters out accounts that are even
deb_open <- function(df) {
  df %>% deb_current() %>% 
  select(id, gr:d) %>% 
    filter(gr + sc + d != 0) %>% 
    arrange(id)
}

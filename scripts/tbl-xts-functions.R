## Function to convert running accounts to xts and from xts ##

library(tidyr)
library(timetk)
library(xts)

# Create running accounts by group function takes a transactions data frame and
# vector of ids to filter by. Returns a running account with group, date, and
# cumulative sum of denari. Function comes from time-series-branch script.
# Can also be used as cumulative for single group, such as single heir.
deb_running_group <- function(df, ids) {
  df_d <- df %>% 
    mutate(denari = deb_lsd_d(l, s, d)) %>% 
    select(from, to, date, denari)
  
  credit <- df_d %>% 
    group_by(from, date) %>% 
    summarise(denari = sum(denari)) %>% 
    rename(id = from)
  debit <- df_d %>% 
    group_by(to, date) %>% 
    summarise(denari = -sum(denari)) %>% 
    rename(id = to)
  
  account_groups <- accounts %>% select(id, group)
  
  bind_rows(credit, debit) %>% 
    filter(id %in% ids) %>% 
    left_join(account_groups, by = "id") %>% 
    group_by(group, date) %>% 
    summarise(denari = sum(denari)) %>% 
    mutate(current = cumsum(denari)) %>% 
    select(-denari) %>% 
    ungroup()
}

# Cumulative running value of a set of accounts. Same as above, but instead of
# grouping by a group, this only groups by date. Output is tibble with date and
# cumulative column. This can work with above to add cumulative to a group of
# running accounts. Result cannot be placed into to_fill_xts, because there is
# no group.

deb_running_cumulative <- function(df, ids) {
  df_d <- df %>% 
    mutate(denari = deb_lsd_d(l, s, d)) %>% 
    select(from, to, date, denari)
  
  credit <- df_d %>% 
    group_by(from, date) %>% 
    summarise(denari = sum(denari)) %>% 
    rename(id = from)
  debit <- df_d %>% 
    group_by(to, date) %>% 
    summarise(denari = -sum(denari)) %>% 
    rename(id = to)
  
  bind_rows(credit, debit) %>% 
    filter(id %in% ids) %>% 
    group_by(date) %>% 
    summarise(denari = sum(denari)) %>% 
    mutate(cumulative = cumsum(denari)) %>% 
    select(-denari) %>% 
    ungroup()
}

# To xts takes a running account tbl with date, id/group, and current value columns
# It uses tidyr to widen the tbl, timetk to convert to xts object,
# and the na.locf function to fill in amounts for each day
# Returns an xts function with one observation for each id for each day

to_fill_xts <- function(df, group = id, current = current) {
  group <- enquo(group)
  df_wide <- df %>% spread(!!group, current)
  df_xts <- tk_xts(df_wide, silent = TRUE)
  na.locf(merge(df_xts, seq(min(df$date),
                                  max(df$date), by = 1)))
}

# From xts to tbl
# Reverses `to_fill_xts`, but with value for each day for each id/group
# Only needs xts object with dates as row names, ids as column names, and values
# Result is a tibble with date, id, and current variables

from_fill_xts <- function(xts) {
  tbl <- tk_tbl(xts, rename_index = "date")
  tbl %>% gather(id, current, -date)
}

### Credit and debit data frame functions ###

### Create tibble of credit and debit transactions from one account or vector of accounts ###
# This function needs accounts tibble to exist. If it does not, function gives a warning
# Can be run on transactions tibble or transactions_sum tibble
# Single accounts must have quotations around account id
# These functions make percentage from denarii

deb_account_credit <- function(df,
                               account_id,
                               credit = credit,
                               l = l,
                               s = s,
                               d = d,
                               round = 3){
  credit <- rlang::enquo(credit)
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)
    
  df %>% 
    filter(!! credit %in% !! account_id) %>%
    mutate(denarii = deb_lsd_d(!! l, !! s, !! d), 
           pct = round(denarii*100/sum(denarii), 2)) %>% 
    arrange(desc(denarii)) %>% 
    select(-(!! credit))
}

deb_account_debit <- function(df,
                              account_id,
                              debit = debit,
                              l = l,
                              s = s,
                              d = d,
                              round = 3){
  debit <- rlang::enquo(debit)
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)
  
  df %>% 
    filter(!! debit %in% !! account_id) %>%
    mutate(denarii = deb_lsd_d(!! l, !! s, !! d), 
           pct = round(denarii*100/sum(denarii), 2)) %>% 
    arrange(desc(denarii)) %>% 
    select(-(!! debit))
}

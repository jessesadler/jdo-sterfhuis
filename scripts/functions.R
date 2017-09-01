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


deb_sum <- function(x) {
  summarise(x,
   gr = sum(gr) + ((sum(sc) + (sum(d) %/% 12)) %/% 20),
   sc = (sum(sc) + (sum(d) %/% 12)) %% 20,
   d = sum(d) %% 12)
}

deb_account_d <- function(x, id) {
  filter(x, to == id) %>% deb_sum()
}

deb_account_c <- function(x, id) {
  filter(x, from == id) %>% deb_sum()
}

# Take a data frame with gr, sc, and d columns and
# return a data frame with summed credit subtracted from summed debit
deb_current <- function(x) {
  credit <- x %>% group_by(from) %>% summarise(
    gr_c = sum(gr) + ((sum(sc) + (sum(d) %/% 12)) %/% 20),
    sc_c = (sum(sc) + (sum(d) %/% 12)) %% 20,
    d_c = sum(d) %% 12)
  
  debit_vlams <- x %>% group_by(to) %>% summarise(
    gr_d = sum(gr) + ((sum(sc) + (sum(d) %/% 12)) %/% 20),
    sc_d = (sum(sc) + (sum(d) %/% 12)) %% 20,
    d_d = sum(d) %% 12)
  
  accounts_sum <- full_join(credit_vlams, debit_vlams, by = c("from" = "to")) %>% 
    replace_na(list(gr_c = 0, sc_c = 0, d_c = 0, gr_d = 0, sc_d = 0, d_d = 0)) %>% 
    rename(id = from)
  
  mutate(accounts_sum, gr = gr_c - gr_d, sc = sc_c - sc_d, d = d_c - d_d)
}


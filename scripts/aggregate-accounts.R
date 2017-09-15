## Aggregate Accounts ##

library(stringr)

## Estate
estate_accounts <- filter(accounts, type == "Estate") %>% 
  select(id) %>% flatten() %>% as_vector()
estate_replace <- set_names(replicate(length(estate_accounts), "dfl12_151"), estate_accounts)

transactions$from <- str_replace_all(transactions$from, estate_replace)
transactions$to <- str_replace_all(transactions$to, estate_replace)

## Profits and losses
transactions$from <- str_replace_all(transactions$from, "dfl12_445", "dfl12_038")
transactions$to <- str_replace_all(transactions$to, "dfl12_445", "dfl12_038")

## Branches

# Verona
transactions$from <- str_replace_all(transactions$from, "dfl12_446", "dfl12_110")
transactions$to <- str_replace_all(transactions$to, "dfl12_446", "dfl12_110")

# Venice
transactions$from <- str_replace_all(transactions$from, "dfl12_181", "dfl12_111")
transactions$to <- str_replace_all(transactions$to, "dfl12_181", "dfl12_111")

# London
transactions$from <- str_replace_all(transactions$from, "dfl12_477", "dfl12_112")
transactions$to <- str_replace_all(transactions$to, "dfl12_477", "dfl12_112")


## Wissels
wissel_accounts <- filter(accounts, type == "Wissel") %>% 
  select(id) %>% flatten() %>% as_vector()
wissel_replace <- set_names(replicate(length(wissel_accounts), "dfl12_117"), wissel_accounts)

transactions$from <- str_replace_all(transactions$from, wissel_replace)
transactions$to <- str_replace_all(transactions$to, wissel_replace)

## Erffgoed
erffgoed_accounts <- filter(accounts, type == "Erffgoed") %>% 
  select(id) %>% flatten() %>% as_vector()
erffgoed_replace <- set_names(replicate(length(erffgoed_accounts), erffgoed_accounts[1]), erffgoed_accounts)

transactions$from <- str_replace_all(transactions$from, erffgoed_replace)
transactions$to <- str_replace_all(transactions$to, erffgoed_replace)

## Written_off
written_off_accounts <- filter(accounts, type == "Written_off") %>% 
  select(id) %>% flatten() %>% as_vector()
written_off_replace <- set_names(replicate(length(written_off_accounts), written_off_accounts[1]), written_off_accounts)

transactions$from <- str_replace_all(transactions$from, written_off_replace)
transactions$to <- str_replace_all(transactions$to, written_off_replace)

## Bequest
bequest_accounts <- filter(accounts, type == "Bequest") %>% 
  select(id) %>% flatten() %>% as_vector()
bequest_replace <- set_names(replicate(length(bequest_accounts), "dfl12_074"), bequest_accounts)

transactions$from <- str_replace_all(transactions$from, bequest_replace)
transactions$to <- str_replace_all(transactions$to, bequest_replace)

## Goods
goods_accounts <- filter(accounts, type == "Goods") %>% 
  select(id) %>% flatten() %>% as_vector()
goods_replace <- set_names(replicate(length(goods_accounts), goods_accounts[1]), goods_accounts)

transactions$from <- str_replace_all(transactions$from, goods_replace)
transactions$to <- str_replace_all(transactions$to, goods_replace)

## Goods with Companies
goods_accounts <- filter(accounts, type == "Goods" | type == "Company") %>% 
  select(id) %>% flatten() %>% as_vector()
goods_replace <- set_names(replicate(length(goods_accounts), goods_accounts[1]), goods_accounts)

transactions$from <- str_replace_all(transactions$from, goods_replace)
transactions$to <- str_replace_all(transactions$to, goods_replace)

## Companies
companies_accounts <- filter(accounts, type == "Companies") %>% 
  select(id) %>% flatten() %>% as_vector()
goods_replace <- set_names(replicate(length(companies_accounts), companies_accounts[1]), companies_accounts)

transactions$from <- str_replace_all(transactions$from, companies_replace)
transactions$to <- str_replace_all(transactions$to, companies_replace)

## Trade
trade_accounts <- filter(accounts, type == "Trade") %>% 
  select(id) %>% flatten() %>% as_vector()
trade_replace <- set_names(replicate(length(trade_accounts), trade_accounts[1]), trade_accounts)

transactions$from <- str_replace_all(transactions$from, trade_replace)
transactions$to <- str_replace_all(transactions$to, trade_replace)

## Factors
factor_accounts <- filter(accounts, type == "Factor") %>% 
  select(id) %>% flatten() %>% as_vector()
factor_replace <- set_names(replicate(length(factor_accounts), factor_accounts[1]), factor_accounts)

transactions$from <- str_replace_all(transactions$from, factor_replace)
transactions$to <- str_replace_all(transactions$to, factor_replace)

## Giovane
giovane_accounts <- filter(accounts, type == "Giovane") %>% 
  select(id) %>% flatten() %>% as_vector()
giovane_replace <- set_names(replicate(length(giovane_accounts), giovane_accounts[1]), giovane_accounts)

transactions$from <- str_replace_all(transactions$from, giovane_replace)
transactions$to <- str_replace_all(transactions$to, giovane_replace)

## Factors and Giovanes
factors_accounts <- filter(accounts, type == "Factor" | type == "Giovane") %>% 
  select(id) %>% flatten() %>% as_vector()
factors_replace <- set_names(replicate(length(factors_accounts), factors_accounts[1]), factors_accounts)

transactions$from <- str_replace_all(transactions$from, factors_replace)
transactions$to <- str_replace_all(transactions$to, factors_replace)

## Political
political_accounts <- filter(accounts, type == "Political") %>% 
  select(id) %>% flatten() %>% as_vector()
political_replace <- set_names(replicate(length(political_accounts), political_accounts[1]), political_accounts)

transactions$from <- str_replace_all(transactions$from, political_replace)
transactions$to <- str_replace_all(transactions$to, political_replace)

## Miscellaneous and kin
misc_accounts <- filter(accounts, type == "Miscellaneous" | type == "Kin") %>% 
  select(id) %>% flatten() %>% as_vector()
misc_replace <- set_names(replicate(length(misc_accounts), misc_accounts[1]), misc_accounts)

transactions$from <- str_replace_all(transactions$from, misc_replace)
transactions$to <- str_replace_all(transactions$to, misc_replace)

## Law
law_accounts <- filter(accounts, type == "Law") %>% 
  select(id) %>% flatten() %>% as_vector()
law_replace <- set_names(replicate(length(law_accounts), law_accounts[1]), law_accounts)

transactions$from <- str_replace_all(transactions$from, law_replace)
transactions$to <- str_replace_all(transactions$to, law_replace)

## Loan
loan_accounts <- filter(accounts, type == "Loan") %>% 
  select(id) %>% flatten() %>% as_vector()
loan_replace <- set_names(replicate(length(loan_accounts), loan_accounts[1]), loan_accounts)

transactions$from <- str_replace_all(transactions$from, loan_replace)
transactions$to <- str_replace_all(transactions$to, loan_replace)

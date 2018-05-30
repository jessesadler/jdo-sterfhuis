## Aggregate accounts by type of account##

library(stringr)

## Estate
estate_accounts <- filter(accounts, type == "Estate") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
estate_replace <- set_names(replicate(length(estate_accounts), "dfl12_151"), estate_accounts)

transactions$credit <- str_replace_all(transactions$credit, estate_replace)
transactions$debit <- str_replace_all(transactions$debit, estate_replace)

## Profits and losses
transactions$credit <- str_replace_all(transactions$credit, "dfl12_445", "dfl12_038")
transactions$debit <- str_replace_all(transactions$debit, "dfl12_445", "dfl12_038")

## Branches
branch_accounts <- filter(accounts, type == "Branch") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()

# Verona
transactions$credit <- str_replace_all(transactions$credit, "dfl12_446", "dfl12_110")
transactions$debit <- str_replace_all(transactions$debit, "dfl12_446", "dfl12_110")

# Venice
transactions$credit <- str_replace_all(transactions$credit, "dfl12_181", "dfl12_111")
transactions$debit <- str_replace_all(transactions$debit, "dfl12_181", "dfl12_111")

# London
transactions$credit <- str_replace_all(transactions$credit, "dfl12_477", "dfl12_112")
transactions$debit <- str_replace_all(transactions$debit, "dfl12_477", "dfl12_112")


## Wissels
wissel_accounts <- filter(accounts, type == "Wissel") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
wissel_replace <- set_names(replicate(length(wissel_accounts), "dfl12_117"), wissel_accounts)

transactions$credit <- str_replace_all(transactions$credit, wissel_replace)
transactions$debit <- str_replace_all(transactions$debit, wissel_replace)

## Erffgoed
erffgoed_accounts <- filter(accounts, type == "Erffgoed") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
erffgoed_replace <- set_names(replicate(length(erffgoed_accounts), erffgoed_accounts[1]), erffgoed_accounts)

transactions$credit <- str_replace_all(transactions$credit, erffgoed_replace)
transactions$debit <- str_replace_all(transactions$debit, erffgoed_replace)

## Written_off
written_off_accounts <- filter(accounts, type == "Written_off") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
written_off_replace <- set_names(replicate(length(written_off_accounts), written_off_accounts[1]), written_off_accounts)

transactions$credit <- str_replace_all(transactions$credit, written_off_replace)
transactions$debit <- str_replace_all(transactions$debit, written_off_replace)

## Bequest
bequest_accounts <- filter(accounts, type == "Bequest") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
bequest_replace <- set_names(replicate(length(bequest_accounts), "dfl12_074"), bequest_accounts)

transactions$credit <- str_replace_all(transactions$credit, bequest_replace)
transactions$debit <- str_replace_all(transactions$debit, bequest_replace)

## Goods
goods_accounts <- filter(accounts, type == "Goods") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
goods_replace <- set_names(replicate(length(goods_accounts), goods_accounts[1]), goods_accounts)

transactions$credit <- str_replace_all(transactions$credit, goods_replace)
transactions$debit <- str_replace_all(transactions$debit, goods_replace)

## Goods with Companies
goods_accounts <- filter(accounts, type == "Goods" | type == "Company") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
goods_replace <- set_names(replicate(length(goods_accounts), goods_accounts[1]), goods_accounts)

transactions$credit <- str_replace_all(transactions$credit, goods_replace)
transactions$debit <- str_replace_all(transactions$debit, goods_replace)

## Companies
companies_accounts <- filter(accounts, type == "Companies") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
goods_replace <- set_names(replicate(length(companies_accounts), companies_accounts[1]), companies_accounts)

transactions$credit <- str_replace_all(transactions$credit, companies_replace)
transactions$debit <- str_replace_all(transactions$debit, companies_replace)

## Trade
trade_accounts <- filter(accounts, type == "Trade") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
trade_replace <- set_names(replicate(length(trade_accounts), trade_accounts[1]), trade_accounts)

transactions$credit <- str_replace_all(transactions$credit, trade_replace)
transactions$debit <- str_replace_all(transactions$debit, trade_replace)

## Factors
facdebitr_accounts <- filter(accounts, type == "Facdebitr") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
facdebitr_replace <- set_names(replicate(length(facdebitr_accounts), facdebitr_accounts[1]), facdebitr_accounts)

transactions$credit <- str_replace_all(transactions$credit, facdebitr_replace)
transactions$debit <- str_replace_all(transactions$debit, facdebitr_replace)

## Giovane
giovane_accounts <- filter(accounts, type == "Giovane") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
giovane_replace <- set_names(replicate(length(giovane_accounts), giovane_accounts[1]), giovane_accounts)

transactions$credit <- str_replace_all(transactions$credit, giovane_replace)
transactions$debit <- str_replace_all(transactions$debit, giovane_replace)

## Facdebitrs and Giovanes
facdebitrs_accounts <- filter(accounts, type == "Facdebitr" | type == "Giovane") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
facdebitrs_replace <- set_names(replicate(length(facdebitrs_accounts), facdebitrs_accounts[1]), facdebitrs_accounts)

transactions$credit <- str_replace_all(transactions$credit, facdebitrs_replace)
transactions$debit <- str_replace_all(transactions$debit, facdebitrs_replace)

## Political
political_accounts <- filter(accounts, type == "Political") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
political_replace <- set_names(replicate(length(political_accounts), political_accounts[1]), political_accounts)

transactions$credit <- str_replace_all(transactions$credit, political_replace)
transactions$debit <- str_replace_all(transactions$debit, political_replace)

## Miscellaneous and kin
misc_accounts <- filter(accounts, type == "Miscellaneous" | type == "Kin") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
misc_replace <- set_names(replicate(length(misc_accounts), misc_accounts[1]), misc_accounts)

transactions$credit <- str_replace_all(transactions$credit, misc_replace)
transactions$debit <- str_replace_all(transactions$debit, misc_replace)

## Law
law_accounts <- filter(accounts, type == "Law") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
law_replace <- set_names(replicate(length(law_accounts), law_accounts[1]), law_accounts)

transactions$credit <- str_replace_all(transactions$credit, law_replace)
transactions$debit <- str_replace_all(transactions$debit, law_replace)

## Loan
loan_accounts <- filter(accounts, type == "Loan") %>% 
  select(id) %>% flatten() %>% as_vecdebitr()
loan_replace <- set_names(replicate(length(loan_accounts), loan_accounts[1]), loan_accounts)

transactions$credit <- str_replace_all(transactions$credit, loan_replace)
transactions$debit <- str_replace_all(transactions$debit, loan_replace)

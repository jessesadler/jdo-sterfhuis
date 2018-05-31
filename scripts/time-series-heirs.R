## Time series for individual heirs ##

library(tidyverse)
library(xts)
library(timetk)
library(lubridate)
library(debkeepr)
library(ggrepel)
library(hrbrthemes)
source("scripts/times-series-functions.R")

transactions <- read_csv("data/transactions.csv")
accounts <- read_csv("data/accounts.csv")

## Example of Marten

marten_accounts <- filter(accounts, group == "Marten") %>% 
  select(id) %>% flatten() %>% as_vector()

# Create Marten running with Marten's accounts
marten_running <- deb_running(transactions, accounts, label = account, marten_accounts)
marten_cumulative <- deb_running_cumulative(transactions, accounts, marten_accounts, account)

marten_running <- bind_rows(marten_running, marten_cumulative)

# To xts
marten_xts <- to_fill_xts(marten_running, group = account) %>% 
  replace_na(0)

# To tbl
marten_tbl <- from_fill_xts(marten_xts) %>% 
  deb_d_mutate(current) %>% 
  mutate(lsd = if_else(current >= 0,
                       paste0("£", scales::comma(l), " ", s, "s. ", round(d), "d."),
                       paste0("-£", scales::comma(-l), " ", -s, "s. ", round(-d), "d.")))

distinct(marten_tbl, id)

marten_tbl$id <- str_replace_all(marten_tbl$id, "Capitael.*", "Capital of Marten")
marten_tbl$id <- str_replace_all(marten_tbl$id, "Marten.della.Faille_1", "Personal 1")
marten_tbl$id <- str_replace_all(marten_tbl$id, "Marten.della.Faille_2", "Personal 2")
marten_tbl$id <- str_replace_all(marten_tbl$id, "Marten.della.Faille_paternal", "Paternal")
marten_tbl$id <- str_replace_all(marten_tbl$id, "Marten.della.Faille_sororal_1", "Sororal 1")
marten_tbl$id <- str_replace_all(marten_tbl$id, "Marten.della.Faille_sororal_2", "Sororal 2")

# Plot
ggplot(marten_tbl) + 
  geom_line(aes(x = date, y = l, group = id, color = id), size = 1) + 
  geom_hline(yintercept = 0, size = 1) + 
  scale_y_continuous(labels = scales::dollar_format("£")) + 
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") + 
  labs(y = NULL, x = NULL, color = "Marten's account") + 
  theme_ipsum(base_size = 14) + 
  ggtitle("Inheritance due to Marten della Faille",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594")

ggsave("plots-aans/marten-running.png", width = 10, height = 8)

# Plot with only before 16 March 1585 and after 1 December 1594
date_break1 <- ymd("1585-03-16")
date_break2 <- ymd("1594-09-01")

# Find max lsd
marten_tbl %>% 
  filter(date < date_break1) %>% 
  filter(current == max(current))


marten1 <- marten_tbl %>% 
  filter(date < date_break1) %>% 
  mutate(label = case_when(date == ymd("1582-11-08") & current > 0 ~ lsd,
                           date == ymd("1585-03-15") & current > 0 ~ lsd,
                           current == max(current) & date == ymd("1583-12-26") ~
                             paste("26 December 1583", lsd, sep = ": ")))

ggplot(marten1) + 
  geom_line(aes(x = date, y = l, group = id, color = id), size = 1) + 
  geom_text_repel(aes(x = date, y = l, label = label),
            nudge_y = 250) + 
  geom_hline(yintercept = 0, size = 1, alpha = 0.8) + 
  scale_y_continuous(labels = scales::dollar_format("£")) + 
  scale_x_date(date_breaks = "4 month", date_labels = "%m-%Y",
               expand = c(0.1, 0.1)) + 
  labs(y = NULL, x = NULL, color = "Branches") + 
  theme_ipsum(base_size = 14) + 
  ggtitle("Inheritance due to Marten della Faille",
          subtitle = "November 1582 to March 1585")

ggsave("plots-aans/marten-running-1.png", width = 10, height = 8)

marten2 <- marten_tbl %>% 
  filter(date >= date_break2) %>% 
  mutate(label = case_when(date == ymd("1594-09-01") & l > 0 ~ lsd,
                           date == ymd("1594-12-16") & id == "Cumulative" ~ lsd))

ggplot(marten2) + 
  geom_line(aes(x = date, y = l, group = id, color = id), size = 1) + 
  geom_text_repel(aes(x = date, y = l, label = label),
            nudge_y = -50,
            nudge_x = 4) + 
  geom_hline(yintercept = 0, size = 1) + 
  scale_y_continuous(labels = scales::dollar_format("£")) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y",
               expand = c(0.1, 0.1)) + 
  labs(y = NULL, x = NULL, color = "Branches") + 
  theme_ipsum(base_size = 14) + 
  ggtitle("Inheritance due to Marten della Faille",
          subtitle = "September 1594 to 16 December 1594")

ggsave("plots-aans/marten-running-2.png", width = 10, height = 8)

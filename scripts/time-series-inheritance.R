## Time series inheritance ##

# Get xts object of running value of cumulative credit of all nine heirs

library(tidyverse)
library(xts)
library(timetk)
library(lubridate)
library(dygraphs)
library(debkeepr)
library(ggrepel)
library(hrbrthemes)
source("scripts/functions/times-series-functions.R")

# This script uses tbl-xts functions

# Load data
transactions <- read_csv("data/transactions.csv")
accounts <- read_csv("data/accounts.csv")

inheritance_ids <- filter(accounts, type == "Inheritance") %>% 
  select(id) %>% flatten() %>% as_vector()

# Create inheritance running with groups from inheritance accounts
inheritance_running <- deb_running(transactions, accounts, label = group, inheritance_ids)

# To xts
inheritance_xts <- to_fill_xts(inheritance_running, group = group) %>% 
  replace_na(0)

# xts in pounds
inheritance_xts_l <- floor(inheritance_xts/240)

# To tibble
inheritance_tbl <- from_fill_xts(inheritance_xts) %>% 
  deb_d_mutate(current) %>% 
  mutate(lsd = if_else(current >= 0,
                       paste0("£", scales::comma(l), " ", s, "s. ", round(d), "d."),
                       paste0("-£", scales::comma(-l), " ", -s, "s. ", round(-d), "d.")),
         label = case_when(date == ymd("1582-11-08") & current > 0 & id != "Hester" ~ lsd,
                           current == max(current) & date == ymd("1583-12-26") ~
                             paste("26 December 1583", lsd, sep = ": ")))

# Plot
ggplot(inheritance_tbl) + 
  geom_line(aes(x = date, y = l, group = id, color = id), size = 1) + 
  geom_text_repel(aes(x = date, y = l, label = label),
            nudge_y = 250,
            nudge_x = 50) + 
  geom_hline(yintercept = 0, size = 1) + 
  scale_y_continuous(labels = scales::dollar_format("£")) + 
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") + 
  labs(y = NULL, x = NULL, color = "Heirs") + 
  theme_ipsum(base_size = 14) + 
  ggtitle("Inheritance due to the heirs of Jan de Oude",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594")

ggsave("plots-aans/inheritance-running.png", width = 10, height = 8)

# Plot with only before 16 March 1585 and after 1 December 1594
date_break1 <- ymd("1585-03-16")
date_break2 <- ymd("1594-09-01")

# Find max lsd
inheritance_tbl %>% 
  filter(date < date_break1) %>% 
  filter(current == max(current))

inheritance1 <- inheritance_tbl %>% 
  filter(date < date_break1)

ggplot(inheritance1) + 
  geom_line(aes(x = date, y = l, group = id, color = id), size = 1) + 
  geom_text(aes(x = date, y = l, label = label),
            nudge_y = 250) + 
  geom_hline(yintercept = 0, size = 1, alpha = 0.8) + 
  scale_y_continuous(labels = scales::dollar_format("£")) + 
  scale_x_date(date_breaks = "4 month", date_labels = "%m-%Y",
               expand = c(0.1, 0.1)) + 
  labs(y = NULL, x = NULL, color = "Branches") + 
  theme_ipsum(base_size = 14) + 
  ggtitle("Inheritance due to the heirs of Jan de Oude",
          subtitle = "November 1582 to March 1585")

ggsave("plots-aans/inheritance-running-1.png", width = 10, height = 8)

# Find when Hester is zeroed out
inheritance_tbl %>% 
  filter(date >= date_break2) %>% 
  filter(current == 0 & id == "Hester")

inheritance2 <- inheritance_tbl %>% 
  filter(date >= date_break2) %>% 
  mutate(label = case_when(date == ymd("1594-12-16") & (l > 10 | l < 0) ~ lsd,
                           current == 0 & date == ymd("1594-10-15") ~ 
                             paste("", "", "Hester: 15 October 1594,", lsd, sep = "\n")))

ggplot(inheritance2) + 
  geom_line(aes(x = date, y = l, group = id, color = id), size = 1) + 
  geom_text(aes(x = date, y = l, label = label),
            nudge_y = -50,
            nudge_x = 4) + 
  geom_hline(yintercept = 0, size = 1) + 
  scale_y_continuous(labels = scales::dollar_format("£")) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y",
               expand = c(0.1, 0.1)) + 
  labs(y = NULL, x = NULL, color = "Branches") + 
  theme_ipsum(base_size = 14) + 
  ggtitle("Inheritance due to the heirs of Jan de Oude",
          subtitle = "September 1594 to 16 December 1594")

ggsave("plots-aans/inheritance-running-2.png", width = 10, height = 8)

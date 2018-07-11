### Time series with grouped branches ###

library(tidyverse)
library(xts)
library(timetk)
library(dygraphs)
library(lubridate)
library(debkeepr)
library(hrbrthemes)

# Load data
transactions <- read_csv("data/transactions.csv")
accounts <- read_csv("data/accounts.csv")

# Change transactions to denarii and simplify
transactions_d <- transactions %>% 
  deb_lsd_d_mutate(column_name = denarii) %>% 
  select(credit, debit, date, denarii)

# Get total credit and debit for each accout by date
# Make denarii negative for debit transactions
credit <- transactions_d %>% 
  group_by(credit, date) %>% 
  summarise(denarii = sum(denarii)) %>% 
  rename(id = credit)
debit <- transactions_d %>% 
  group_by(debit, date) %>% 
  summarise(denarii = -sum(denarii)) %>% 
  rename(id = debit)

# Get groups from accounts tibble
account_groups <- accounts %>% select(id, group)

branch_accounts <- filter(accounts, type == "Branch") %>% 
  select(id) %>% flatten() %>% as_vector()

# Create running values
# Filter to only include branch accounts
# Left join with account_groups to get grouping information
# Summarize by group rather than id to join together
# the two accounts for each branch
branches_running <- bind_rows(credit, debit) %>% 
  filter(id %in% branch_accounts) %>% 
  left_join(account_groups, by = "id") %>% 
  group_by(group, date) %>% 
  summarise(denarii = sum(denarii)) %>% 
  mutate(current = cumsum(denarii)) %>% 
  select(-denarii) %>% 
  ungroup()

# Plot of branches data
before <- ggplot(branches_running) + 
  geom_line(aes(x = date, y = current/240, group = group, color = group))

### Convert to xts ###

# Could use to_fill_xts and from_fill xts with group

# Widen tbl then convert
branches_wide <- branches_running %>% spread(group, current)

# Convert to xts
branches_xts <- tk_xts(branches_wide, silent = TRUE)

# Fill in values for every day
branches_xts <- na.locf(merge(branches_xts, seq(min(branches_running$date),
                                                max(branches_running$date), by = 1)))

# Convert denarii to pounds rounded down for plotting
branches_xts_l <- floor(branches_xts/240)

# Plot
plot(branches_xts_l)
dygraph(branches_xts_l) %>% dyRangeSelector() %>% dyOptions(stackedGraph = TRUE)

### Return to tibble ###
branches_tbl <- tk_tbl(branches_xts, rename_index = "date")

branches_running_fill <- branches_tbl %>% 
  gather(id, current, -date) %>% 
  mutate(group = case_when(id == "London.branch" ~ "London branch",
                           id == "Venice.branch" ~ "Venice branch",
                           id == "Verona.branch" ~ "Verona branch")) %>% 
  deb_d_mutate(current) %>% 
  select(-id)

# Plot
ggplot(branches_running_fill) + 
  geom_line(aes(x = date, y = l, group = group, color = group), size = 1) + 
  geom_hline(yintercept = 0, size = 1) + 
  scale_y_continuous(labels = scales::dollar_format("£")) + 
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") + 
  labs(y = NULL, x = NULL, color = "Branches") + 
  theme_ipsum(base_size = 14) + 
  ggtitle("Running values of the branches in the trade of Jan de Oude",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594")

ggsave("plots-aans/branches-running.png", width = 10, height = 8)

# Plot with only before 16 March 1585 and after 1 December 1594
date_break1 <- ymd("1585-03-16")
date_break2 <- ymd("1594-09-01")

# Make labels
branches1 <- branches_running_fill %>% 
  filter(date < date_break1) %>% 
  mutate(label = case_when(date == ymd("1582-11-08") ~ 
                             paste0("-£", scales::comma(-l), " ", -s, "s. ", round(-d), "d."),
                           date == ymd("1585-03-15") ~ 
                             paste0("-£", scales::comma(-l), " ", -s, "s. ", round(-d), "d."),
                           l == min(l) & date == ymd("1583-12-11") ~
                             paste0("-£", scales::comma(-l), " ", -s, "s. ", round(-d), "d.",
                                    "\n", "11 Dec 1583"),
                           l == max(l) & date == ymd("1583-07-29") ~
                             paste0("£", scales::comma(l), " ", s, "s. ", round(d), "d.",
                                    "\n", "29 July 1583")))

branches2 <- branches_running_fill %>% 
  filter(date >= date_break2) %>% 
  mutate(label = case_when(date == ymd("1594-09-01") ~ 
                             paste0("-£", scales::comma(-l), " ", -s, "s. ", round(-d), "d."),
                           l == 0 & date == ymd("1594-10-04") ~ paste("£0 0s. 0d."),
                           date == ymd("1594-12-16") & group != "London branch" ~ 
                             paste0("-£", scales::comma(-l), " ", -s, "s. ", round(-d), "d.")))

# Separate graphs with date breaks
ggplot(branches1) + 
  geom_line(aes(x = date, y = l, group = group, color = group), size = 1) + 
    geom_text(aes(x = date, y = l, label = label),
              nudge_y = -700) + 
  geom_hline(yintercept = 0, size = 1, alpha = 0.8) + 
  scale_y_continuous(labels = scales::dollar_format("£")) + 
  scale_x_date(date_breaks = "4 month", date_labels = "%m-%Y",
               expand = c(0.1, 0.1)) + 
  labs(y = NULL, x = NULL, color = "Branches") + 
  theme_ipsum(base_size = 14) + 
  ggtitle("Running values of the branches in the trade of Jan de Oude",
          subtitle = "November 1582 to March 1585")

ggsave("plots-aans/branches-running-1.png", width = 10, height = 8)

ggplot(branches2) + 
  geom_line(aes(x = date, y = l, group = group, color = group), size = 1) + 
  geom_text(aes(x = date, y = l, label = label),
            nudge_y = 500) + 
  geom_hline(yintercept = 0, size = 1) + 
  scale_y_continuous(labels = scales::dollar_format("£")) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y",
               expand = c(0.1, 0.1)) + 
  labs(y = NULL, x = NULL, color = "Branches") + 
  theme_ipsum(base_size = 14) + 
  ggtitle("Running values of the branches in the trade of Jan de Oude",
          subtitle = "September 1594 to 16 December 1594")

ggsave("plots-aans/branches-running-2.png", width = 10, height = 8)

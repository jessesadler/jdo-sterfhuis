library(tidyverse)
library(debkeepr)
library(packcircles)

transactions <- read_csv("data/transactions.csv")
accounts <- read_csv("data/accounts.csv") %>% 
  select(-id_int, -account, -kinship, -location)

accounts_debit <- deb_debit(transactions) %>% 
  left_join(accounts, by = c("account_id" = "id")) %>% 
  filter(account_id != "dfl12_001")

groups_debit <- accounts_debit %>% 
  group_by(group) %>% 
  deb_sum(l, s, d) %>% 
  mutate(debit = round(deb_lsd_l(l, s, d), 3))

group_types <- accounts %>% 
  group_by(type, group) %>% 
  summarise()

# Add type to data frame. This adds extra rows, to zap extra rows
# Create label for groups over £2000.
groups_debit <- left_join(groups_debit, group_types, by = "group") %>% 
  distinct(group, .keep_all = TRUE) %>% 
  mutate(lsd = paste0("£", scales::comma(l), " ", s, "s. ", round(d), "d.")) %>% 
  mutate(label = if_else(debit > 2000, paste(group, lsd, sep = "-"), ""))

packing <- circleProgressiveLayout(groups_debit$debit, sizetype = "area")

debit_circles <- bind_cols(groups_debit, packing)

type_id <- debit_circles %>% 
  rowid_to_column("id") %>% 
  select(id, type)

# Add type for color of circles
dat.gg <- circleLayoutVertices(packing, npoints = 50) %>% 
  left_join(type_id, by = "id")

# Plot
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill = as.factor(type)),
               color = "black", alpha = 0.6) +
  geom_text(data = debit_circles, aes(x, y, size = debit,
                                       label = str_replace_all(label, "-", "\n"))) +
  scale_size_continuous(range = c(2, 5)) +
  theme_void() + 
  theme(legend.position = "none") +
  coord_equal()

ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill = as.factor(type)),
               color = "black", alpha = 0.6) +
  geom_text(data = debit_circles, aes(x, y, size = debit,
                                       label = str_replace_all(label, "-", "\n"))) +
  scale_size_continuous(range = c(2, 5)) +
  guides(size = FALSE) + 
  labs(fill = "Account groups") + 
  theme_void() + 
  coord_equal()

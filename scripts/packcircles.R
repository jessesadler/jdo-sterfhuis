library(tidyverse)
library(debkeepr)
library(packcircles)

# Use group transactions and accounts
transactions_group <- read_csv("data/transactions_group.csv")
accounts_group <- read_csv("data/accounts_group.csv") %>% 
  select(-account, -kinship, -location)

groups_debit <- deb_debit(transactions_group) %>% 
  left_join(accounts_group, by = c("account_id" = "id")) %>% 
  rename(id = account_id, account_id = account_id.y) %>% 
  filter(account_id != "dfl12_001") %>% 
  select(-id, -account_id) %>% 
  mutate(debit = round(deb_lsd_l(l, s, d), 3)) %>% 
  mutate(lsd = paste0("£", scales::comma(l), " ", s, "s. ", round(d), "d.")) %>% 
  mutate(label = if_else(debit > 2000, paste(group, lsd, sep = "-"), ""))

## Make circles ##
packing <- circleProgressiveLayout(groups_debit$debit, sizetype = "area")
debit_circles <- bind_cols(groups_debit, packing)

# Add type to circles data
type_id <- debit_circles %>% 
  rowid_to_column("id") %>% 
  select(id, type)

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

# With legend
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

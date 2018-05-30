library(tidyverse)
library(debkeepr)
library(packcircles)
library(hrbrthemes)

# Use group transactions and accounts
transactions_group <- read_csv("data/transactions_group.csv")
accounts_group <- read_csv("data/accounts_group.csv") %>% 
  select(-account, -kinship, -location)

groups_debit <- deb_debit(transactions_group) %>% 
  left_join(accounts_group, by = c("account_id" = "id")) %>% 
  rename(id = account_id, account_id = account_id.y) %>% 
  filter(account_id != "dfl12_001") %>% 
  select(-id, -account_id) %>% 
  mutate(debit = round(deb_lsd_l(l, s, d), 3),
         lsd = paste0("£", scales::comma(l), " ", s, "s. ", round(d), "d."),
         label = if_else(debit >= 2000, paste(group, lsd, sep = "-"), ""))

## Make circles ##
packing_debit <- circleProgressiveLayout(groups_debit$debit, sizetype = "area")
debit_circles <- bind_cols(groups_debit, packing_debit)

# Add type to circles data
type_id <- debit_circles %>% 
  rowid_to_column("id") %>% 
  select(id, type)

dat.gg_debit <- circleLayoutVertices(packing_debit, npoints = 50) %>% 
  left_join(type_id, by = "id")

# Plot
ggplot() + 
  geom_polygon(data = dat.gg_debit, aes(x, y, group = id, fill = type),
               color = "black", alpha = 0.6) +
  geom_text(data = debit_circles, aes(x, y, size = debit,
                                       label = str_replace_all(label, "-", "\n"))) +
  scale_size_continuous(range = c(2, 5)) +
  theme_void() + 
  theme(legend.position = "none") +
  coord_equal()

# With legend
ggplot() + 
  geom_polygon(data = dat.gg_debit, aes(x, y, group = id, fill = type),
               color = "black", alpha = 0.6) +
  geom_text(data = debit_circles, aes(x, y, size = debit,
                                      label = str_replace_all(label, "-", "\n"))) +
  scale_size_continuous(range = c(1, 4.5)) +
  guides(size = FALSE) + 
  labs(fill = "Account types") +
  ggtitle("Total value received by groups of accounts",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594") + 
  theme_ipsum(plot_margin = margin(10, 0, 10, 0)) + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) + 
  coord_equal()

ggsave("plots-aans/circles-debit.png", width = 10, height = 8)


###############################################################################
############################## Opening values #################################
###############################################################################

transactions <- read_csv("data/transactions.csv")
accounts <- read_csv("data/accounts.csv") %>% 
  select(id, label)

# Simplify account names that will be used for labels
accounts$label <- str_replace(accounts$label, "Jan van Borne running account",
                              "Jan van Borne \n running account")
accounts$label <- str_replace(accounts$label, "Jan de Backere and Sevaes Wouters",
                              "Jan de Backere \n and Sevaes Wouters")
accounts$label <- str_replace(accounts$label, "Gillis del Ponte and Hilaria della Faille",
                              "Gillis del Ponte \n and Hilaria della Faille")

opening_debit <- transactions %>% 
  filter(credit == "dfl12_001") %>% 
  select(id = debit, l, s, d) %>% 
  add_column(relation = "Debtor", .after = 1)

opening_credit <- transactions %>% 
  filter(debit == "dfl12_001") %>% 
  select(id = credit, l, s, d) %>% 
  add_column(relation = "Creditor", .after = 1)

opening <- bind_rows(opening_debit, opening_credit) %>% 
  left_join(accounts, by = "id") %>% 
  mutate(value = round(deb_lsd_l(l, s, d), 3),
         lsd = paste0("£", scales::comma(l), " ", s, "s. ", round(d), "d."),
         label = if_else(l >= 1000, paste(label, lsd, sep = "-"), ""))

# Relation as factor to make debit be red
opening$relation <- as_factor(opening$relation) %>% 
  fct_relevel(c("Debtor", "Creditor"))

## Make circles ##
packing_opening <- circleProgressiveLayout(opening$value, sizetype = "area")
opening_circles <- bind_cols(opening, packing_opening)

# Add type to circles data
relation_id_opening <- opening %>% 
  rowid_to_column("row_id") %>% 
  select(id = row_id, relation)

dat.gg_opening <- circleLayoutVertices(packing_opening, npoints = 50) %>% 
  left_join(relation_id_opening, by = "id")

# Total value at opening
deb_account(transactions, "dfl12_001") %>% 
  mutate(lsd = paste0("£", scales::comma(l), " ", s, "s. ", round(d), "d."))

# Plot
ggplot() + 
  geom_polygon(data = dat.gg_opening, aes(x, y, group = id, fill = relation),
               color = "black", alpha = 0.6) +
  geom_text(data = opening_circles, aes(x, y, size = value,
                                      label = str_replace_all(label, "-", "\n"))) +
  scale_size_continuous(range = c(1, 4.5)) +
  guides(size = FALSE) + 
  labs(fill = "Relation") +
  ggtitle("Value of accounts in the estate of Jan della Faille de Oude, 8 December 1582",
          subtitle = "Opening value of the estate: £82,813 5s. 8d.") + 
  theme_ipsum(plot_margin = margin(20, 20, 20, 20)) + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) + 
  coord_equal()

ggsave("plots-aans/circles-opening.png", width = 10, height = 8)


###############################################################################
############################## Closing values #################################
###############################################################################

# Simplify account names that will be used for labels
accounts$label <- str_replace_all(accounts$label, "Heirs of Maria.*", "Heirs of Maria")
accounts$label <- str_replace(accounts$label, "Marten della Faille’s.*", "Heirs of Anna de Hane")
accounts$label <- str_replace(accounts$label, "Jacques, Jan.*", "Jacques, Jan, \n Carlo, and Hester")
accounts$label <- str_replace(accounts$label, "Branch of Venice new.*", "Branch of Venice \n new account")
accounts$label <- str_replace(accounts$label, "Marten, Steven.*", "Marten, Steven, \n and Anna")
accounts$label <- str_replace(accounts$label, "Heirs of Gilles.*",
                              "Heirs of Gilles Hasebaert \n and Magdalena du Boys")

closing <- deb_open(transactions) %>% 
  left_join(accounts, by = c("account_id" = "id")) %>% 
  mutate(relation = if_else(l + s + d > 0, "Creditor", "Debtor"),
         l = if_else(l < 0, -l, l),
         s = if_else(s < 0, -s, s),
         d = if_else(d < 0, -d, d),
         value = round(deb_lsd_l(l, s, d), 3),
         lsd = paste0("£", scales::comma(l), " ", s, "s. ", round(d), "d."),
         label = if_else(l >= 300, paste(label, lsd, sep = "-"), ""))

closing$relation <- as_factor(closing$relation) %>% 
  fct_relevel(c("Debtor", "Creditor"))

## Make circles ##
packing_closing <- circleProgressiveLayout(closing$value, sizetype = "area")
closing_circles <- bind_cols(closing, packing_closing)

# Add type to circles data
relation_id_closing <- closing %>% 
  rowid_to_column("row_id") %>% 
  select(id = row_id, relation)

dat.gg_closing <- circleLayoutVertices(packing_closing, npoints = 50) %>% 
  left_join(relation_id_closing, by = "id")

# Total value at end of 1594
deb_balance(transactions) %>% 
  mutate(lsd = paste0("£", scales::comma(l), " ", s, "s. ", round(d), "d."))

# Plot
ggplot() + 
  geom_polygon(data = dat.gg_closing, aes(x, y, group = id, fill = relation),
               color = "black", alpha = 0.6) +
  geom_text(data = closing_circles, aes(x, y, size = value,
                                        label = str_replace_all(label, "-", "\n"))) +
  scale_size_continuous(range = c(1, 4.5)) +
  guides(size = FALSE) + 
  labs(fill = "Relation") +
  ggtitle("Value of accounts in the estate of Jan della Faille de Oude, 31 December 1594",
          subtitle = "Current value of the estate: £9,768 1s. 8d.") + 
  theme_ipsum(plot_margin = margin(20, 20, 20, 20)) + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) + 
  coord_equal()

ggsave("plots-aans/circles-closing.png", width = 10, height = 8)

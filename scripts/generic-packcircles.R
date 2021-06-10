## Generic packcircles ##

library(tidyverse)
library(debkeepr)
library(packcircles)
library(hrbrthemes)

transactions <- read_rds("data/transactions-lsd.rds")
accounts <- read_csv("data/accounts.csv") %>% 
  select(id, label)

circle_account <- "dfl12_038"

credit <- transactions %>% 
  filter(credit == circle_account) %>% 
  group_by(credit, debit) %>% 
  deb_summarise(lsd) %>% 
  ungroup() %>% 
  mutate(l = deb_lsd_l(lsd)) %>% 
  select(id = debit, l) %>% # need to take out lsd column to bind rows
  add_column(relation = "Creditor", .after = 1)

debit <- transactions %>% 
  filter(debit == circle_account) %>% 
  group_by(credit, debit) %>% 
  deb_summarise(lsd) %>% 
  ungroup() %>% 
  mutate(l = deb_lsd_l(lsd)) %>% 
  select(id = credit, l) %>% # need to take out lsd column to bind rows
  add_column(relation = "Debtor", .after = 1)

values <- bind_rows(credit, debit) %>% 
  left_join(accounts, by = "id") %>% 
  mutate(lsd = deb_l_lsd(l),
         value = paste0("£", scales::comma(purrr::map_dbl(lsd, 1)), " ",
                        purrr::map_dbl(lsd, 2), "s. ",
                        purrr::map_dbl(lsd, 3), "d."),
         label = if_else(l >= 500, paste(label, value, sep = "-"), ""))

# Relation as factor to make debit be red
values$relation <- as_factor(values$relation) %>% 
  fct_relevel(c("Debtor", "Creditor"))

## Make circles ##
packing_values <- circleProgressiveLayout(values$l, sizetype = "area")
circle_text <- bind_cols(values, packing_values)

# Add type to circles data
relation_id <- values %>% 
  rowid_to_column("row_id") %>% 
  select(id = row_id, relation)

gg_data <- circleLayoutVertices(packing_values, npoints = 50) %>% 
  left_join(relation_id, by = "id")

# Plot
ggplot() + 
  geom_polygon(data = gg_data, aes(x, y, group = id, fill = relation),
               color = "black", alpha = 0.6) +
  geom_text(data = circle_text, aes(x, y, size = l,
                                    label = str_replace_all(label, "-", "\n"))) +
  scale_size_continuous(range = c(1, 4.5)) +
  guides(size = FALSE) + 
  labs(fill = "Relation") +
  ggtitle("Profits and losses in the trade of Jan de Oude",
          subtitle = "1 January 1579 to 26 December 1583") + 
  theme_ipsum(plot_margin = margin(20, 20, 20, 20)) + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) + 
  coord_equal()

# width = 10, height = 8
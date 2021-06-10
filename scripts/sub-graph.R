sub_graph <- function(df, account) {
  trans_sub <- filter(transactions, credit %in% account | debit %in% account)
  trans_sum <- trans_sub %>%
    group_by(credit, debit) %>%
    deb_summarise(lsd) %>%
    mutate(l = deb_lsd_l(lsd))

  credit <- trans_sub %>%
    deb_credit() %>%
    mutate(pounds = deb_lsd_l(lsd),
           relation = "credit") %>%
    select(id = account_id, relation, pounds, -lsd)
  debit <- trans_sub %>%
    deb_debit() %>%
    mutate(pounds = deb_lsd_l(lsd),
           relation = "debit") %>%
    select(id = account_id, relation, pounds, -lsd)
  nodes <- bind_rows(credit, debit) %>%
    arrange(desc(pounds)) %>%
    distinct(id, .keep_all = TRUE)

  sub_graph <- graph_from_data_frame(d = trans_sum,
                                     vertices = nodes,
                                     directed = TRUE)

  p <- ggraph(sub_graph, layout = "fr") +
    geom_edge_fan(aes(color = l),
                  arrow = arrow(length = unit(2, 'mm')),
                  end_cap = circle(3, 'mm')) +
    scale_edge_color_distiller(palette = "Greys",
                               direction = 1,
                               labels = scales::dollar_format(prefix = "£")) +
    geom_node_point(aes(size = pounds), color = "grey", alpha = 0.7) +
    geom_node_text(aes(label = name)) +
    scale_size_continuous(range = c(1, 10), labels = scales::dollar_format(prefix = "£")) +
    labs(size = "Accumulated Value",
         color = "Transactions") +
    theme_graph()

  p
}

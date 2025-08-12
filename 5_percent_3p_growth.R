
#### DISTRIBUTION 3 PERCENT ####

lowest <- read_csv("weights/most_prices.csv") %>% filter(category != "Meta", lowest == 1)
core <- lowest %>% filter(category %in% c("Services","Goods"))

MI_dates <- cpi %>% filter(date > "2012-01-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates_three = MI_dates[seq(1, length(MI_dates), 12)]

all <- cpi %>% filter(item_name %in% lowest$item_name, !is.na(Pchange1), date > "2012-01-01") %>% mutate(p3 = (Pchange1a > 0.03)) %>%
  group_by(date) %>%
  summarize(total_3 = sum(p3)/n()) %>% ungroup() %>% ungroup() %>% mutate(type = "All items")


cpi %>% filter(item_name %in% core$item_name, !is.na(Pchange1)) %>% mutate(p3 = (Pchange1a > 0.03)) %>%
  group_by(date) %>%
  summarize(total_3 = sum(p3)/n()) %>% ungroup() %>% ungroup() %>% mutate(type = "Core items") %>%
  #rbind(all) %>%
  group_by(type) %>%
  mutate(last_value = ifelse(date==max(date),total_3,NA)) %>%
  filter(date >= "2018-01-01") %>%
  ungroup() %>%
  ggplot(aes(date, total_3, label=label_percent()(last_value))) + geom_line(size=1) +
  labs(y = NULL,
       x = NULL,
       title = "Percent of Items With 3% Price Growth is Increasing",
       subtitle = "Percent of 82 item in core basket having at least 3 percent monthly price increases, annualized.",
       caption ="BLS, CPI, only seasonally adjusted items included. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates_three) +
  geom_text(show.legend=FALSE, nudge_x = 75) +
  theme(legend.position = c(0.35,0.8)) +
  theme_esp() +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )


ggsave("graphics/three_percent_growth_both.png", dpi="retina", width = 12, height=6.75, units = "in")

all %>%
  mutate(last_value = ifelse(date==max(date),total_3,NA),
         month3 = total_3 + lag(total_3, 1) + lag(total_3, 2),
         month3 = month3/3) %>%
  filter(date >= "2018-01-01") %>%
  ungroup() %>%
  ggplot(aes(date, y = total_3, label=label_percent()(last_value))) + geom_line(size=1) +
  labs(y = NULL,
       x = NULL,
       title = "Percent of Items With 3% Price Growth Has Increased in Recent Months",
       subtitle = "Percent of 140 CPI items (82 in core) having at least 3 percent monthly price increases, annualized.",
       caption ="BLS, CPI, only seasonally adjusted items included. Author's calculation. Mike Konczal.") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates_three) +
  geom_text(show.legend=FALSE, nudge_x = 125) +
  theme(legend.position = c(0.35,0.8)) +
  theme_esp() +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )

ggsave("graphics/three_percent_growth_all_items.png", dpi="retina", width = 12, height=6.75, units = "in")

cpi %>% filter(item_name %in% lowest$item_name, !is.na(Pchange1), date > "2012-01-01") %>% mutate(p3 = (Pchange1a > 0.03)) %>%
  left_join(lowest, by = "item_name") %>%
  group_by(date, category) %>%
  summarize(total_3 = sum(p3)/n()) %>% ungroup() %>% ungroup() %>% mutate(type = "All items") %>%
  mutate(last_value = ifelse(date==max(date),total_3,NA),
         month3 = total_3 + lag(total_3, 1) + lag(total_3, 2),
         month3 = month3/3) %>%
  filter(date >= "2018-01-01") %>%
  ungroup() %>%
  ggplot(aes(date, y = total_3, label=label_percent()(last_value))) + geom_line(size=1) +
  facet_wrap(~category) +
  labs(y = NULL,
       x = NULL,
       title = "Percent of Items With 3% Price Growth Has Increased in Recent Months",
       subtitle = "Percent of 140 CPI items (82 in core) having at least 3 percent monthly price increases, annualized.",
       caption ="BLS, CPI, only seasonally adjusted items included. Author's calculation. Mike Konczal.") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates_three) +
  geom_text(show.legend=FALSE, nudge_x = 125) +
  theme_esp() +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )

ggsave("graphics/three_percent_faceted.png", dpi="retina", width = 12, height=6.75, units = "in")

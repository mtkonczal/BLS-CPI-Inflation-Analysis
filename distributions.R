
lowest <- read_csv("weights/most_prices.csv") %>% filter(category != "Meta", lowest == 1)

MI_dates <- cpi %>% filter(date > "2012-01-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates_three = MI_dates[seq(1, length(MI_dates), 12)]

cpi %>% filter(item_name %in% lowest$item_name, !is.na(Pchange1)) %>% filter(date == max(date) | date == "2019-12-01" | date == max(date) %m-% months(3)) %>%
  left_join(lowest, by="item_name") %>%
  mutate(dateF = as.factor(date)) %>%
  mutate(dateM = paste(as.character(month(date, label = TRUE, abbr = FALSE)), ", ", as.character(year(date)), sep = "")) %>%
  mutate(category = factor(category, levels=c("Goods", "Services", "Food", "Energy"))) %>%
  ggplot(aes(Pchange3, dateF, fill=dateF)) + geom_boxplot() + facet_wrap(~category) +
  labs(title="Three month percent change, 141 CPI items", caption="Mike Konczal, Roosevelt Institute, Boxplots Guy", x="", y="") + theme_classic() +
  scale_x_continuous(labels = percent) + theme(legend.position = "none") + scale_y_discrete(labels = c("December, 2019","December, 2022", "March, 2023"))

ggsave("graphics/boxpots.png", dpi="retina", width = 12, height=6.75, units = "in")


  
  ggplot(aes(date, total_3, label=label_percent()(last_value))) + geom_line(size=1) + theme_lass +
  labs(y = NULL,
       x = NULL,
       title = "Percent of Items With 3% Price Growth Has Decreased From High Levels",
       subtitle = "Percent of 141 CPI items having at least 3 percent monthly price increases, annualized.",
       caption ="BLS, CPI, only seasonally adjusted items included. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates_three) +
  geom_text(show.legend=FALSE, nudge_x = 80, nudge_y = 0.011)


  a <- cpi %>% filter(item_name %in% lowest$item_name, !is.na(Pchange1)) %>% filter(date == max(date) | date == "2019-12-01" | date == max(date) %m-% months(3)) %>%
    left_join(lowest, by="item_name") %>%
    mutate(dateF = as.factor(date)) %>%
    mutate(dateM = paste(as.character(month(date, label = TRUE, abbr = FALSE)), ", ", as.character(year(date)), sep = ""))
# First of the month!

df <- cpi_data %>%
  filter(seasonal == "U", !is.na(date), area_code == "0000", substr(period, 1, 1) == "M") %>%
  group_by(item_name) %>%
  mutate(pchange1 = value / lag(value, 1) - 1) %>%
  ungroup()

first_title <- "March in Line With Prepandemic Values"
years_array <- c(2019,2022,2023, 2024, 2025)

second_title <- "Seasonally Unadjusted Values Returning to Broad Historical Range"
third_title <- "Compared to historical values, 2024 lines up"


cpi_data %>%
  filter(seasonal == "U", !is.na(date), area_code == "0000", substr(period, 1, 1) == "M",
         item_name %in% c("All items less food and energy")) %>%
  mutate(pchange1 = value / lag(value, 1) - 1,
         pchange12 = value / lag(value, 12) - 1) %>%
  group_by(year) %>%
  reframe(jan_change = pchange1[period == "M01"],
          year_change = pchange12[period == "M12"]) %>%
  ungroup() %>%
  filter(!is.na(jan_change), year <= 2019) %>%
  summarize(mean(jan_change/year_change))


#### First Graphic ####
df %>%
  filter(item_name %in% c("All items less food and energy")) %>%
  select(date, pchange1, item_name) %>%
  mutate(month = month(date), year = as.factor(year(date))) %>%
  filter(year(date) %in% years_array) %>%
  ggplot(aes(month, pchange1, color = year)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.2) +
  theme_lass +
  scale_x_continuous(breaks = 1:12, labels = month.name) +
  scale_y_continuous(labels = percent) +
  geom_text_repel(aes(label = year),
                  size = 7,
                  data = . %>% group_by(year) %>% filter(month == 1) %>% ungroup(), nudge_x = -0.3
  ) +
  labs(
    title = first_title,
    subtitle = "Seasonally unadjusted values for core CPI inflation, 1-month percent change, not annualized.",
    caption = "Inspired by Paul Romer's blog. Mike Konczal"
  )
ggsave("graphics/unadjusted_g1.png", dpi="retina", width = 12, height=6.75, units = "in")


#### First Graphic ####
df %>%
  filter(item_name %in% c("All items")) %>%
  select(date, pchange1, item_name) %>%
  mutate(month = month(date), year = as.factor(year(date))) %>%
  filter(year(date) %in% years_array) %>%
  ggplot(aes(month, pchange1, color = year)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.2) +
  theme_lass +
  scale_x_continuous(breaks = 1:12, labels = month.name) +
  scale_y_continuous(labels = percent) +
  geom_text_repel(aes(label = year),
                  size = 7,
                  data = . %>% group_by(year) %>% filter(month == 1) %>% ungroup(), nudge_x = -0.3
  ) +
  labs(
    title = first_title,
    subtitle = "Seasonally unadjusted values for CPI inflation, all items, 1-month percent change, not annualized.",
    caption = "Inspired by Paul Romer's blog. Mike Konczal"
  )
ggsave("graphics/unadjusted_g1_ALL_ITEMS.png", dpi="retina", width = 12, height=6.75, units = "in")

df %>%
  filter(item_name %in% c("All items less food and energy")) %>%
  filter(year >= 1998, year <= 2019) %>%
  mutate(month = month(date), yearF = as.factor(year(date)), monthF = as.factor(month(date))) %>%
  ggplot() + geom_boxplot(aes(monthF, pchange1), fill="skyblue") + theme_classic(base_size = 18) +
  geom_point(data = df %>%
               filter(item_name %in% c("All items less food and energy"), year >= 2022, month(date) %in% c(1,2,3,4,5,6))
             , aes(as.factor(month(date)), pchange1, color=as.factor(year)), size=5, show.legend = FALSE) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(labels = month.name) +
  labs(x=NULL, y=NULL,
       title=second_title,
       subtitle="Seasonally unadjusted 1-month change in core CPI. Boxplots for 1998-2019; center line is median, box 25th-75th percentile.",
       caption="Mike Konczal") +
  theme(plot.title.position = "plot") +
  geom_text(data = df %>%
              filter(item_name %in% c("All items less food and energy"), year >= 2022, month(date) == 1)
            , aes(as.factor(month(date)), pchange1, label=year, color=as.factor(year)), size=5, show.legend = FALSE, nudge_x = -0.35
  )
  
ggsave("graphics/unadjusted_g2.png", dpi="retina", width = 14, height=6.75, units = "in")

  
facet_categories <- c("Commodities less food and energy commodities", "Transportation services")

df %>%
  filter(item_name %in% facet_categories) %>%
  filter(year >= 1998, year <= 2019) %>%
  mutate(month = month(date), yearF = as.factor(year(date)), monthF = as.factor(month(date))) %>%
  ggplot() + geom_boxplot(aes(monthF, pchange1), fill="skyblue") + theme_classic(base_size = 18) +
  geom_point(data = df %>%
               filter(item_name %in% facet_categories, year >= 2023, month(date) %in% c(1,2,3,4,5,6))
             , aes(as.factor(month(date)), pchange1, color=as.factor(year)), size=5, show.legend = FALSE) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(labels = month.abb) +
  labs(x=NULL, y=NULL,
       title="Transportation Services Pop Up",
       subtitle="Seasonally unadjusted 1-month change in CPI. Boxplots for 1998-2019; center line is median, box 25th-75th percentile.",
       caption="Mike Konczal") +
  theme(plot.title.position = "plot") +
  geom_text(data = df %>%
              filter(item_name %in% facet_categories, year >= 2023, month(date) == 1)
            , aes(as.factor(month(date)), pchange1, label=year, color=as.factor(year)), size=5, show.legend = FALSE, nudge_x = 0.45
  ) +
  facet_wrap(~item_name, scales = "free")

ggsave("graphics/unadjusted_g3.png", dpi="retina", width = 20, height=6.75, units = "in")




View(df %>% filter(item_name == "All items less food and energy") %>%
  filter(month(date) == 1) %>%
  select(year, item_name, pchange1))



facet_categories <- c("Motor vehicle insurance")

df %>%
  filter(item_name %in% facet_categories) %>%
  filter(year >= 1998, year <= 2019) %>%
  mutate(month = month(date), yearF = as.factor(year(date)), monthF = as.factor(month(date))) %>%
  ggplot() + geom_boxplot(aes(monthF, pchange1), fill="skyblue") + theme_classic(base_size = 18) +
  geom_point(data = df %>%
               filter(item_name %in% facet_categories, year >= 2023, month(date) %in% c(1,2,3,4,5,6))
             , aes(as.factor(month(date)), pchange1, color=as.factor(year)), size=5, show.legend = FALSE) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(labels = month.abb) +
  labs(x=NULL, y=NULL,
       title="Transportation Services Pop Up",
       subtitle="Seasonally unadjusted 1-month change in CPI. Boxplots for 1998-2019; center line is median, box 25th-75th percentile.",
       caption="Mike Konczal") +
  theme(plot.title.position = "plot") +
  geom_text(data = df %>%
              filter(item_name %in% facet_categories, year >= 2023, month(date) == 1)
            , aes(as.factor(month(date)), pchange1, label=year, color=as.factor(year)), size=5, show.legend = FALSE, nudge_x = 0.45
  ) +
  facet_wrap(~item_name, scales = "free")
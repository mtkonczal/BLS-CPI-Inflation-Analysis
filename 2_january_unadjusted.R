# First of the month!

cpi %>%
  filter(seasonal == "U", item_name == "All items") %>%
  filter(year(date) == 2025)

cpi <- cpi_data %>% filter(period != "M13") %>% filter(seasonal == "S")
cpi <- fill_single_na_geomean(cpi)
cpi <- create_cpi_changes(cpi)


df <- cpi_data %>%
  filter(
    seasonal == "U",
    !is.na(date),
    area_code == "0000",
    substr(period, 1, 1) == "M"
  )

df <- fill_single_na_geomean(df)

df <- df %>%
  group_by(item_name) %>%
  mutate(pchange1 = value / lag(value, 1) - 1) %>%
  ungroup()

first_title <- "2026 Inflation Levels More Like 2022 Than 2024"
years_array <- c(2019, 2022, 2024, 2025, 2026)

second_title <- first_title
third_title <- first_title


cpi_data %>%
  filter(
    seasonal == "U",
    !is.na(date),
    area_code == "0000",
    substr(period, 1, 1) == "M",
    item_name %in% c("All items less food and energy")
  ) %>%
  mutate(
    pchange1 = value / lag(value, 1) - 1,
    pchange12 = value / lag(value, 12) - 1
  ) %>%
  group_by(year) %>%
  reframe(
    jan_change = pchange1[period == "M01"],
    year_change = pchange12[period == "M12"]
  ) %>%
  ungroup() %>%
  filter(!is.na(jan_change), year <= 2019) %>%
  summarize(mean(jan_change / year_change))


line_cols <- c(
  "2019" = "#4E79A7", # blue
  "2022" = "#59A14F", # green
  "2024" = "#6B7280", # slate/grey
  "2025" = "#E15759", # warm red/orange accent
  "2026" = "#B07AA1" # muted plum
)

#### First Graphic ####
df %>%
  filter(item_name %in% c("All items less food and energy")) %>%
  select(date, pchange1, item_name) %>%
  mutate(month = month(date), year = as.factor(year(date))) %>%
  filter(year(date) %in% years_array) %>%
  ggplot(aes(month, pchange1, color = year)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.2) +
  theme_esp() +
  scale_x_continuous(breaks = 1:12, labels = month.name) +
  scale_y_continuous(labels = percent) +
  geom_text_repel(
    aes(label = year),
    size = 7,
    data = . %>% group_by(year) %>% filter(month == 1) %>% ungroup(),
    nudge_x = -0.3
  ) +
  labs(
    title = first_title,
    subtitle = "Seasonally unadjusted values for core CPI inflation, 1-month percent change, not annualized.",
    caption = "Inspired by Paul Romer's blog. Mike Konczal"
  ) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  ) +
  scale_color_manual(values = line_cols) +
  guides(color = "none") +
  geom_hline(yintercept = 0)


ggsave(
  "graphics/unadjusted_g1.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

first_title_supercore <- "Seasonally Unadjusted Supercore CPI Negative In Recent Months"

df %>%
  filter(item_name %in% c("All items less food, shelter, and energy")) %>%
  select(date, pchange1, item_name) %>%
  mutate(month = month(date), year = as.factor(year(date))) %>%
  filter(year(date) %in% years_array) %>%
  ggplot(aes(month, pchange1, color = year)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.2) +
  theme_esp() +
  scale_x_continuous(breaks = 1:12, labels = month.name) +
  scale_y_continuous(labels = percent) +
  geom_text_repel(
    aes(label = year),
    size = 7,
    data = . %>% group_by(year) %>% filter(month == 1) %>% ungroup(),
    nudge_x = -0.3
  ) +
  labs(
    title = first_title_supercore,
    subtitle = "Seasonally unadjusted values for supercore CPI inflation, 1-month percent change, not annualized.",
    caption = "Inspired by Paul Romer's blog. Mike Konczal"
  ) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  ) +
  scale_color_manual(values = line_cols) +
  guides(color = "none") +
  geom_hline(yintercept = 0)

ggsave(
  "graphics/unadjusted_g1_SUPERCORE.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)


df %>%
  filter(item_name %in% c("All items")) %>%
  select(date, pchange1, item_name) %>%
  mutate(month = month(date), year = as.factor(year(date))) %>%
  filter(year(date) %in% years_array) %>%
  ggplot(aes(month, pchange1, color = year)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.2) +
  theme_esp() +
  scale_x_continuous(breaks = 1:12, labels = month.name) +
  scale_y_continuous(labels = percent) +
  geom_text_repel(
    aes(label = year),
    size = 7,
    data = . %>% group_by(year) %>% filter(month == 1) %>% ungroup(),
    nudge_x = -0.3
  ) +
  labs(
    title = first_title,
    subtitle = "Seasonally unadjusted values for CPI inflation, all items, 1-month percent change, not annualized.",
    caption = "Inspired by Paul Romer's blog. Mike Konczal"
  ) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  ) +
  scale_color_manual(values = line_cols) +
  guides(color = "none") +
  geom_hline(yintercept = 0)
ggsave(
  "graphics/unadjusted_g1_ALL_ITEMS.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

df %>%
  filter(item_name %in% c("All items less food and energy")) %>%
  filter(year >= 1998, year <= 2019) %>%
  mutate(
    month = month(date),
    yearF = as.factor(year(date)),
    monthF = as.factor(month(date))
  ) %>%
  ggplot() +
  geom_boxplot(aes(monthF, pchange1), fill = "skyblue") +
  theme_classic(base_size = 18) +
  geom_point(
    data = df %>%
      filter(
        item_name %in% c("All items less food and energy"),
        year >= 2022,
        month(date) %in% c(1, 2, 3, 4, 5, 6)
      ),
    aes(as.factor(month(date)), pchange1, color = as.factor(year)),
    size = 5,
    show.legend = FALSE
  ) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(labels = month.name) +
  labs(
    x = NULL,
    y = NULL,
    title = second_title,
    subtitle = "Seasonally unadjusted 1-month change in core CPI. Boxplots for 1998-2019; center line is median, box 25th-75th percentile.",
    caption = "Mike Konczal"
  ) +
  theme(plot.title.position = "plot") +
  geom_text(
    data = df %>%
      filter(
        item_name %in% c("All items less food and energy"),
        year >= 2022,
        month(date) == 1
      ),
    aes(
      as.factor(month(date)),
      pchange1,
      label = year,
      color = as.factor(year)
    ),
    size = 5,
    show.legend = FALSE,
    nudge_x = -0.35
  )

ggsave(
  "graphics/unadjusted_g2.png",
  dpi = "retina",
  width = 14,
  height = 6.75,
  units = "in"
)


facet_categories <- c(
  "Commodities less food and energy commodities",
  "Transportation services"
)

df %>%
  filter(item_name %in% facet_categories) %>%
  filter(year >= 1998, year <= 2019) %>%
  mutate(
    month = month(date),
    yearF = as.factor(year(date)),
    monthF = as.factor(month(date))
  ) %>%
  ggplot() +
  geom_boxplot(aes(monthF, pchange1), fill = "skyblue") +
  theme_classic(base_size = 18) +
  geom_point(
    data = df %>%
      filter(
        item_name %in% facet_categories,
        year >= 2023,
        month(date) %in% c(1, 2, 3, 4, 5, 6)
      ),
    aes(as.factor(month(date)), pchange1, color = as.factor(year)),
    size = 5,
    show.legend = FALSE
  ) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(labels = month.abb) +
  labs(
    x = NULL,
    y = NULL,
    title = "Transportation Services Pop Up",
    subtitle = "Seasonally unadjusted 1-month change in CPI. Boxplots for 1998-2019; center line is median, box 25th-75th percentile.",
    caption = "Mike Konczal"
  ) +
  theme(plot.title.position = "plot") +
  geom_text(
    data = df %>%
      filter(item_name %in% facet_categories, year >= 2023, month(date) == 1),
    aes(
      as.factor(month(date)),
      pchange1,
      label = year,
      color = as.factor(year)
    ),
    size = 5,
    show.legend = FALSE,
    nudge_x = 0.45
  ) +
  facet_wrap(~item_name, scales = "free")

ggsave(
  "graphics/unadjusted_g3.png",
  dpi = "retina",
  width = 20,
  height = 6.75,
  units = "in"
)


facet_categories <- c("Motor vehicle insurance")

df %>%
  filter(item_name %in% facet_categories) %>%
  filter(year >= 1998, year <= 2019) %>%
  mutate(
    month = month(date),
    yearF = as.factor(year(date)),
    monthF = as.factor(month(date))
  ) %>%
  ggplot() +
  geom_boxplot(aes(monthF, pchange1), fill = "skyblue") +
  theme_classic(base_size = 18) +
  geom_point(
    data = df %>%
      filter(
        item_name %in% facet_categories,
        year >= 2023,
        month(date) %in% c(1, 2, 3, 4, 5, 6)
      ),
    aes(as.factor(month(date)), pchange1, color = as.factor(year)),
    size = 5,
    show.legend = FALSE
  ) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(labels = month.abb) +
  labs(
    x = NULL,
    y = NULL,
    title = "Transportation Services Pop Up",
    subtitle = "Seasonally unadjusted 1-month change in CPI. Boxplots for 1998-2019; center line is median, box 25th-75th percentile.",
    caption = "Mike Konczal"
  ) +
  theme(plot.title.position = "plot") +
  geom_text(
    data = df %>%
      filter(item_name %in% facet_categories, year >= 2023, month(date) == 1),
    aes(
      as.factor(month(date)),
      pchange1,
      label = year,
      color = as.factor(year)
    ),
    size = 5,
    show.legend = FALSE,
    nudge_x = 0.45
  ) +
  facet_wrap(~item_name, scales = "free")

library(forcats)
library(gt)

goods_levels <- read_csv('data/core_goods_display_levels.csv') %>%
  rename(item_name = subcategory, goods_level = display_level)

# Graphic 1 - Known Targets ----
trump_targets <- c(
  "Appliances",
  "Transportation commodities less motor fuel",
  "Apparel",
  "Household furnishings and supplies",
  "Laundry equipment",
  "Audio equipment"
)

start_trump_year <- 2017

cpi %>%
  filter(item_name %in% trump_targets) %>%
  filter(year(date) >= start_trump_year) %>%
  ggplot(aes(date, Pchange1)) +
  geom_line(color = "#2c3254") +
  theme_esp() +
  facet_wrap(~item_name, scales = "free") +
  scale_y_continuous(label = percent) +
  labs(title="Likely targets for Trump tariffs.",
       subtitle = "Monthly change, seasonally adjusted, not-annualized.",
       caption="Seasonally adjusted, CPI. Mike Konczal.") +
  geom_hline(yintercept = 0) +
  theme(plot.title.position = "plot") +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )

ggsave("graphics/t1_trump_tariff_targets.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graph 2: Top 12 (or n) worst divergences ----

cpi_goods <- cpi %>%
  inner_join(goods_levels, by = "item_name") %>%
  group_by(item_name) %>%
  mutate(value = value/value[date == "2025-01-01"]) %>%
  ungroup() %>%
  mutate(
    trendline = logLinearProjection(
      tbl        = .,
      date       = date,
      value      = value,
      start_date = as.Date("2023-01-01"),
      end_date   = as.Date("2024-12-01"),
      group      = item_name
    )
  )

# Size of gap at latest date for each item
latest_gaps <- cpi_goods %>%
  group_by(item_name) %>%
  filter(date == max(date, na.rm = TRUE)) %>%
  summarise(
    latest_date = max(date),
    gap = value - trendline,
    .groups = "drop"
  )

topn_items <- latest_gaps %>%
  slice_max(order_by = gap, n = 12, with_ties = FALSE) %>%
  arrange(desc(gap))

# Plot only top n; order facets by gap
cpi_goods %>%
  semi_join(topn_items, by = "item_name") %>%
  left_join(select(topn_items, item_name, gap), by = "item_name") %>%
  mutate(item_name = fct_reorder(item_name, gap, .desc = TRUE)) %>%
  filter(year(date) >= 2023) %>%
  ggplot(aes(date, value)) +
  geom_line(color = esp_navy, size = 1.2) +
  geom_line(aes(y = trendline), linetype = "dashed", color = esp_navy, size = 1) +
  facet_wrap(~ item_name, scales = "free_y") +
  labs(title = "12 Commodity Items (All Levels) Where Prices Are Higher Than Their Trendline") +
  theme_esp() +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )
ggsave("graphics/t2_worst_12_price_increases.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graph 3: The major categories -----

# Plot only top n; order facets by gap
cpi_goods %>%
  filter(year(date) >= 2023,
         item_name %in% trump_targets) %>%
  ggplot(aes(date, value)) +
  geom_line(color = esp_navy, size = 1.2) +
  geom_line(aes(y = trendline), linetype = "dashed", color = esp_navy, size = 1) +
  facet_wrap(~ item_name, scales = "free_y") +
  labs(title = "The Likely Tariff Targets Are Seeing Large Price Increases",
       subtitle = "CPI. Log-linear trendline from Jan 2023 to Dec 2024.") +
  theme_esp() +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )
ggsave("graphics/t2_worst_12_price_increases.png", dpi="retina", width = 12, height=6.75, units = "in")


# Graph 4: Table on the major parts. -----

# 1) How many 2025 months are in the data up to the latest date?
latest_date <- max(cpi_goods$date, na.rm = TRUE)
months_in_2025 <- cpi_goods %>%
  filter(year(date) == 2025, date <= latest_date) %>%
  distinct(mo = month(date)) %>%
  nrow()

# safety: if latest_date has not entered 2025 yet, avoid divide-by-zero
if (months_in_2025 == 0) months_in_2025 <- 1

# 2) Build the summary table
tbl <- cpi_goods %>%
  group_by(item_name) %>%
  summarise(
    change2025 = {
      v_latest <- value[date == latest_date]
      v_dec24  <- value[date == as.Date("2024-12-01")]
      ann = (v_latest / v_dec24)^(12 / months_in_2025) - 1
      ann
    },
    change_versus_trendline = {
      v_latest <- value[date == latest_date]
      t_dec24  <- trendline[date == as.Date("2024-12-01")]
      ann = (v_latest / t_dec24)^(12 / months_in_2025) - 1
      ann
    },
    change_2023_2024 = {
      v_dec24 <- value[date == as.Date("2024-12-01")]
      v_dec22 <- value[date == as.Date("2022-12-01")]
      (v_dec24 / v_dec22)^(1/2) - 1
    },
    goods_level = goods_level[which.max(date)],
    .groups = "drop"
  ) %>%
  filter(goods_level %in% c(0, 1)) %>%
  # Row-group labels and ordering: level 0 first, then level 1
  mutate(group_label = if_else(goods_level == 0, "All core goods", "Major items")) %>%
  arrange(goods_level, desc(change2025))

# 3) Make the gt table, grouped by goods_level label
tbl %>%
  select(group_label, item_name, change2025, change_versus_trendline, change_2023_2024) %>%
  gt(groupname_col = "group_label") %>%
  fmt_percent(
    columns = c(change2025, change_versus_trendline, change_2023_2024),
    decimals = 1
  ) %>%
  cols_label(
    item_name = "Item",
    change2025 = glue::glue("’25 pace (ann., thru {format(latest_date, '%b %Y')})"),
    change_versus_trendline = "’25 pace vs. trend (ann.)",
    change_2023_2024 = "’23–’24 avg (ann.)"
  ) %>%
  tab_options(
    table.font.size = px(14),
    data_row.padding = px(4)
  )

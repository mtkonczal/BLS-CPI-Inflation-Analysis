
avg_prices <- getBLSFiles("averageprice", "konczal@gmail.com")


avg_prices_latest <- avg_prices %>%
  filter(area_code == "0000") %>%                 # keep all years for lag
  arrange(item_name, date, .by_group = TRUE) %>%
  group_by(item_name) %>%
  mutate(
    value_12m = lag(value, 6),
    yoyP = value / value_12m - 1,
    yoyA = value - value_12m
  ) %>%
  filter(end_year == 2025) %>%                    # now restrict to 2025
  slice_max(date, n = 1, with_ties = FALSE) %>%   # latest month in 2025
  ungroup() %>%
  select(item_name, date, value_now = value, value_12m, yoyP, yoyA)


View(avg_prices_latest)
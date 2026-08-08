library(lubridate)
item_list <- read_csv("weights/mediancpi_component_table.csv") %>%
  mutate(item_name = Component) %>%
  pull(item_name)


top_cut = 0.99
bottom_cut = 0.01
title = NA

median_data <- cpi %>%
  filter(
    item_name %in%
      item_list |
      item_name == "Owners' equivalent rent of residences"
  ) %>%
  filter(!is.na(date)) %>%
  arrange(date) %>%
  group_by(date) %>%
  mutate(normalized = sum(weight)) %>%
  mutate(weightN = weight / normalized) %>%
  arrange(Pchange6) %>%
  mutate(cumsum = cumsum(weight) / 100) %>%
  mutate(cumsumN = cumsum(weightN))

#quarters_backwards <- (month(max(cpi$date)) + c(0, 3, 6, 9) - 1) %% 12 + 1

# THIS IS THE GRAPHIC - 30 percent-trimmed distribution
median_data %>%
  mutate(dateF = as.factor(date)) %>%
  filter(cumsumN <= top_cut & cumsum >= bottom_cut) %>%
  filter(date == "2026-02-01" | date == "2024-12-01") %>%
  #filter(month(date) %in% quarters_backwards) %>%
  mutate(monthC = format(date, "%B, %Y")) %>%
  mutate(monthC = fct_reorder(monthC, date)) %>%
  mutate(monthCR = fct_rev(monthC)) %>%
  ggplot(aes(x = Pchange6, y = monthCR, fill = stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis(option = "H") +
  theme_ridges() +
  theme_esp() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = percent) +
  labs(
    title = title,
    subtitle = "Distribution of the Cleveland Fed's Median/Trimmed-Mean CPI price basket, 3-month change annualized, with components\nwhose expenditure weights fall above/below the 85/15th percentile of price changes removed.",
    x = "Three Month Percent Change",
    y = "",
    caption = "OER is treated as one value, instead of broken out by region and manually seasonally adjusted as per Cleveland Fed's methodology.Some 2020s removed as negative outlier. Mike Konczal"
  ) +
  theme(
    plot.title.position = "plot",
    legend.position = "none",
    legend.title = element_blank(),
    plot.title = element_text(size = 25, margin = margin(0, 0, 5, 0)),
    plot.subtitle = element_text(size = 15),
    plot.caption = element_text(size = 10, face = "italic"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12)
  )

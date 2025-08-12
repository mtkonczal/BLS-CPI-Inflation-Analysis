#### DISTRIBUTION 3 PERCENT ####

energy_items <- read_csv("weights/most_prices.csv") %>% filter(category == "Energy")

MI_dates <- cpi %>% filter(date > "2012-01-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates_three <- MI_dates[seq(1, length(MI_dates), 12)]

library(scales)

cpi %>%
  filter(item_name == "Electricity") %>%
  mutate(valueN = value / value[date == as.Date("2024-12-01")] - 1) %>%
  filter(year(date) >= 2023) %>%
  {
    last_point <- slice_tail(., n = 1)
    
    ggplot(., aes(date, valueN)) +
      geom_line(color = esp_navy, size = 1.2) +
      geom_text(
        data = last_point,
        aes(
          label = percent(valueN, accuracy = 0.1)
        ),
        vjust = -0.5, # adjust vertical position
        hjust = 0,    # push slightly to the right
        color = esp_navy,
        size = 5
      ) +
      labs(
        y = NULL,
        x = NULL,
        title = "Price of Electricity is Increasing Fast",
        subtitle = "CPI, Electricity, change since December 2024.",
        caption = "BLS, CPI, Author's calculation. Mike Konczal."
      ) +
      theme_esp() +
      theme(
        panel.grid.major.y = element_line(color = "grey80"),
      ) +
      scale_y_continuous(labels = percent)
  }


ggsave("graphics/electricity.png", dpi = "retina", width = 12, height = 6.75, units = "in")

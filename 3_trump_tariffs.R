# This is a test.



trump_targets <- c(
  "Appliances",
  "Transportation commodities less motor fuel",
  "Apparel",
  "Household furnishings and supplies",
  "Laundry equipment"
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
  theme(plot.title.position = "plot")

ggsave("graphics/trump_tariff_targets.png", dpi="retina", width = 12, height=6.75, units = "in")


breaks_value <- generate_dates(cpi$date, 12)

cpi %>%
  filter(item_name == "Transportation commodities less motor fuel") %>%
  filter(year(date) >= start_trump_year) %>%
  mutate(current = value[date == max(date)]) %>%
  ggplot(aes(date, value)) +
  geom_line(color="#9467BD", size=1.2) +
  geom_line(aes(date, current), color="#9467BD", linetype="dashed") +
  theme_classic() +
  labs(title="Autos Reverse Year-Long Price Decreases",
       subtitle = "Price Level, Transportation commodities less motor fuel (New and Used Autos, Motor Parts)",
       caption="Seasonally adjusted, CPI. Mike Konczal.") +
  theme_lass +
  scale_x_date(date_labels = "%b\n%Y", breaks=breaks_value)

ggsave("graphics/autos_takeoff.png", dpi="retina", width = 12, height=6.75, units = "in")


#Graphic 1: Overview
core_3_6_title <- "What's up with autos?"
three_six_graphic(cpi, "Transportation commodities less motor fuel", "2018-01-01", "2020-01-01", "2021-01-01",
                  title = core_3_6_title, include_3_6 = TRUE, column_alpha = 0.2,
                  colors = c("3-Month Change" = "#9467BD", "6-Month Change" = "#C5B0D5")
)
ggsave("graphics/autos.png", dpi="retina", width = 12, height=6.75, units = "in")


cpi %>%
  filter(item_name == "New vehicles",
         date >= "2024-01-01") %>%
  ggplot(aes(date, value)) + geom_line()
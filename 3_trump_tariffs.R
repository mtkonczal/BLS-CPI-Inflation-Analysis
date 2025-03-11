# First of the month!




trump_targets <- c(
  "Appliances",
  "Transportation commodities less motor fuel",
  "Apparel",
  "Household furnishings and supplies",
  "Laundry equipment"
)

start_trump_year <- 2012


cpi %>%
  filter(item_name %in% trump_targets) %>%
  filter(year(date) >= start_trump_year) %>%
  ggplot(aes(date, Pchange1)) +
  geom_line() + geom_line() +
  theme_classic() +
  facet_wrap(~item_name, scales = "free") +
  labs(title="Likely targets for Trump tariffs, monthly change not-annualized.",
       caption="Seasonally adjusted, CPI. Mike Konczal.")

ggsave("graphics/trump_tariff_targets.png", dpi="retina", width = 12, height=6.75, units = "in")







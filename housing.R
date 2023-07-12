



shelter <- cpi %>% filter(item_name == "Rent of shelter") %>% mutate(YoY = value/lag(value,12)-1) %>% select(date, value, YoY) %>% mutate(type = "CPI shelter inflation")

rents <- read_csv("data/metro_rents_zillow.csv") %>% mutate(YoY = value/lag(value,12)-1) %>% select(date, value, YoY) %>% mutate(type = "Zillow rents")

rbind(shelter, rents) %>% filter(date >= "2013-01-01") %>%
  ggplot(aes(date,YoY, color=type)) + geom_line() + theme_classic() +
  theme(legend.position = "bottom")
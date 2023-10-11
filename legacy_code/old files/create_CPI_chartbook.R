# This script creates a chartbook of relevant data. A placeholder until a former markdown file is created.
# Written by: Mike Konczal
# Overhaul begun Sep 19 2023
# Last Updated: 3-12-2022

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/")
library(janitor)
library(tidyverse)
library(ggtext)
library(huxtable)

##### SET UP SOME THINGS #####
source(file = "1_load_cpi_data.R")
source(file = "old files/load_helper_functions.R")


#######
cpi <- cpi_data %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Pchange1a = (1 + Pchange1)^12 - 1) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
  mutate(Pchange3 = (value/lag(value, 3)-1)) %>%
  mutate(three_months = (lag(value, 1)/lag(value, 4)-1)) %>%
  mutate(Wthree_months = (three_months*weight)/100) %>%
  mutate(Wthree_monthsA = (1 + Wthree_months)^4 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100) %>%
  ungroup()


################# SECTION 5: CREATE CHARTBOOK ###########################

# SO COOL LET'S DO THIS!
pdf("chartbook/chart_book.pdf")

p <- monthly_graph(cpi, "All items less food and energy", Gdate = "2017-01-01")
p + geom_line(aes(y=Pchange12*100), color="blue")

monthly_graph(cpi, "Energy", Gdate = "2020-01-01")

monthly_graph(cpi, "Household furnishings and supplies", Gdate = "2020-01-01")

monthly_graph(cpi, "Energy", Gdate = "2020-01-01")

monthly_graph(cpi, "Food", Gdate = "2020-01-01")

monthly_graph(cpi, "Commodities less food and energy commodities", Gdate = "2020-01-01")

monthly_graph(cpi, "Services less energy services", Gdate = "2020-01-01")

monthly_graph(cpi, "Transportation commodities less motor fuel", Gdate = "2020-01-01")

monthly_graph(cpi, item_basket_topline, Gdate = "2021-01-01")

Three_past_plot(cpi, item_basket_watch_categories)

Two_past_plot(cpi, item_basket_watch_categories)

Three_past_plot(cpi, item_basket_core_goods, "Core Goods")

Two_past_plot(cpi, item_basket_core_goods, "Core Goods", "Transportation Commodities are New/Used Cars, Motor Parts; they drive down core inflation this month")

Three_past_plot(cpi, item_basket_core_services, "Core Services")

Two_past_plot(cpi, item_basket_core_services, "Core Services", "Services are starting to pick up, a question if this is consistent or volatile month-to-month")

Three_past_plot(cpi, item_basket_transportation, "Transportation")

Two_past_plot(cpi, item_basket_transportation, "Transportation")

Two_past_plot(cpi, item_basket_topline)


###### OTHER STUFF ####

long_plot <- cpi_data %>%
  filter(item_name == "All items") %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  filter(date >= "2003-01-01") %>%
  mutate(logical2009 = (date >= "2008-01-01")) %>%
  mutate(value2009 = 1.025^(cumsum(logical2009)/12)) %>%
  mutate(value2009 = logical2009*value2009*value[date=="2008-01-01"]) %>%
  mutate(logical2020 = (date >= "2020-01-01")) %>%
  mutate(value2020 = 1.025^(cumsum(logical2020)/12)) %>%
  mutate(value2020 = logical2020*value2020*value[date=="2020-01-01"])

long_plot$final_value2 <- na_if(long_plot$value2020, 0)
long_plot$final_value <- na_if(long_plot$value2009, 0)

ggplot(long_plot, aes(x=date)) + 
  geom_line(aes(y = value), color = "darkred") + 
  geom_line(aes(y = final_value), color="steelblue", linetype="dashed") +
  geom_line(aes(y = final_value2), color="steelblue", linetype="dashed")  +
  theme_minimal() +
  labs(title = "Actual CPI Inflaton Versus Inflation Continued From Recent Recessions",
       subtitle = "Could it hit the trend from the Great Recession?",
       caption = "Projected CPI inflation is 2.5% annual at a monthly rate. Seasonally-adjusted. Author's calculation. @rortybomb",
       x="", y="") +
  theme(
    plot.title.position = "plot",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 22, color="steelblue"),
    plot.subtitle = element_text(size = 18, colour = "darkred", face = "italic"),
    plot.caption = element_text(size = 12, lineheight = 1.2)) +
  expand_limits(y = 175)
ggsave("graphic/long_plot.png")

dev.off()

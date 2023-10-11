###############################################################
# Graphics that I'm watching, Mid 2023, inflation
# Mike Konczal
# Last updated 7/13/2023

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/")
library(hrbrthemes)
library(janitor)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(huxtable)
library(scales)
library(lubridate)
library(tidytext)
library(viridis)
library(ggridges)
library(quantmod)
library(broom)

##### SET UP SOME THINGS #####
theme_lass <-   theme_modern_rc(ticks = TRUE) + theme(legend.position = "none", legend.title = element_blank(),
                                          panel.grid.major.y = element_line(size=0.5),
                                          panel.grid.minor.y = element_blank(),
                                          plot.title.position = "plot",
                                          axis.title.x = element_blank(),
                                          axis.title.y = element_blank(),
                                          plot.title = element_text(size = 25, face="bold"),
                                          plot.subtitle = element_text(size=15, color="white"),
                                          plot.caption = element_text(size=10, face="italic"),
                                          legend.text = element_text(size=12),
                                          axis.text.y = element_text(size=12, face="bold"),
                                          axis.text.x = element_text(size=12, face="bold"),
                                          strip.text = element_text(face = "bold", color="white", hjust = 0.5, size = 10),
                                          panel.grid.major.x = element_blank(),
                                          panel.grid.minor.x = element_blank(),
                                          strip.background = element_blank()) +
  theme(text = element_text(family = "Larsseit"),
        plot.title = element_text(family = "Larsseit"),
        plot.subtitle = element_text(family = "Larsseit"),
        plot.caption = element_text(family="Larsseit"),
        strip.text = element_text(family="Larsseit"))
#Either run Script 1 to download data fresh, or used locally stored data.
source(file = "/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/1_load_cpi_data.R")
#load("data/cpi_data.RData")

#######
#SET UP DATA:
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
  mutate(Pchange3a = (1 + Pchange3)^12 - 1) %>%
  mutate(Wchange3 = (Pchange3*weight)/100) %>%
  mutate(Wchange3a = (1 + Wchange3)^4 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100) %>%
  ungroup()

#### Three month change ####
item_basket_core_services <- c("Shelter",
                               "Medical care services",
                               "Transportation services",
                               "Recreation services",
                               "Education and communication services",
                               "Other personal services")

item_basket_core_goods <- c("Household furnishings and supplies",
                            "Apparel",
                            "Transportation commodities less motor fuel",
                            "Medical care commodities",
                            "Recreation commodities",
                            "Education and communication commodities",
                            "Alcoholic beverages",
                            "Other goods")


item_transportation <- c("Car and truck rental","Motor vehicle insurance","Airline fares")

item_basket_topline <- c("All items", "Energy", "Food", "Commodities less food and energy commodities", "Services less energy services")

item_basket_watch_categories <- c("All items", "New and used motor vehicles", "Shelter", "Other services",
                                  "Medical care services", "Food", "Energy", "Commodities less food and energy commodities")

item_basket <- item_basket_core_goods
dates_basket <- c(max(cpi$date), "2019-12-01")#, max(cpi$date) %m-% months(6))
for_graph <- cpi %>% filter(item_name %in% item_basket, date %in% dates_basket) %>% mutate(dateF = as.factor(date))

segment_data <- for_graph %>%
  arrange(item_name, dateF) %>%
  group_by(item_name) %>%
  summarise(
    x_start = first(Pchange3a),
    x_end = last(Pchange3a),
    y = first(item_name)
  )

# Main ggplot
ggplot() +
  geom_point(data = for_graph, aes(x = item_name, y = Pchange3a, color = dateF), size = 8) +
  geom_segment(data = segment_data,
               aes(x = y, xend = y, y = x_start, yend = x_end),
               arrow = arrow(type = "closed", length = unit(0.2, "inches")),
               lineend = "round") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "top")
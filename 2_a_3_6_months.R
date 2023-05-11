###############################################################
# Graphics that I'm watching, Early 2023, inflation
# Mike Konczal
# Last updated 2/13/2023

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

#source(file = "/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/1_load_cpi_data.R")
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
  mutate(three_months = (lag(value, 1)/lag(value, 4)-1)) %>%
  mutate(Wthree_months = (three_months*weight)/100) %>%
  mutate(Wthree_monthsA = (1 + Wthree_months)^4 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100) %>%
  ungroup()


# BASELINE FOR REFERENCE
title0 <- "This is just the percent change in core"
title1 <- "This is a graphic about core and services inflation"
title2 <- "This is a graphic about the split between shelter and service inflation"
title3 <- "this is a graphic of the three things Powell watches"
title4 <- "this is a graphic of the goods breakdown"
title5 <- "3 6 and 12 month value"
title_longer_graphic <- "This is 3 and 6 month value"
title_energy_food <- "Food Energy Graphic"

#CHANGE THESE
title0 <- "Core Inflation Moves Sideways"
title1 <- "Core inflation drops for two months in a row, the first time since 2021"
title2 <- "Core services outside housing flat again for two months in a row"
title3 <- "Housing Increasing, Services Not Slowing, Goods Not Deflating"
title4 <- "Goods at zero, with autos cancelling other goods"
title5 <- "3-month inflation finally breaks lower"
title_longer_graphic <- "Inflation is Moving Sideways Recently"
title_energy_food <- "Food Inflation is Slowing While Energy Falls"


###### BETTER THREE SIX ####
core <- cpi %>% filter(item_name == "All items less food and energy") %>%
  select(date, value) %>%
  mutate(ThreeMonth = (value/lag(value,3))^4-1) %>%
  mutate(SixMonth = (value/lag(value,6))^2-1) %>%
  mutate(YoY = (value/lag(value,12))-1) %>%
  select(-value, YoY) %>%
  pivot_longer(ThreeMonth:SixMonth, names_to = "time_length", values_to = "change") %>%
  mutate(time_length = str_replace_all(time_length,"SixMonth", "6-Month Change")) %>%
  mutate(time_length = str_replace_all(time_length,"ThreeMonth", "3-Month Change")) %>%
  mutate(last_value = ifelse(date==max(date),change,NA))

one_month <- cpi %>% filter(item_name == "All items less food and energy") %>%
  select(date, Pchange1a) %>% mutate(time_length = "3-Month Change")

MI_dates <- cpi %>% filter(date > "2010-12-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates = MI_dates[seq(1, length(MI_dates), 12)]

date_start = "2017-01-01"
date_end = "2019-01-01"
date_period <- interval(date_start, date_end)
date_period = date_period %/% months(1)

pre_core <- cpi %>% filter(item_name == "All items less food and energy", date == date_start | date == date_end) %>%
  mutate(change = value/lag(value,1)) %>% filter(!is.na(change)) %>% mutate(change = change^(12/date_period) - 1) %>% select(change)
pre_core <- as.numeric(pre_core)

core1 <- core %>% filter(date >= "2017-01-01") %>%
  left_join(one_month, by=c("date","time_length")) %>%
  mutate(type = "Core inflation") %>%
  mutate(pre_core_line = pre_core)


###### BETTER THREE SIX ####
core <- cpi %>% filter(item_name == "All items less food, shelter, energy, and used cars and trucks") %>%
  select(date, value) %>%
  mutate(ThreeMonth = (value/lag(value,3))^4-1) %>%
  mutate(SixMonth = (value/lag(value,6))^2-1) %>%
  mutate(YoY = (value/lag(value,12))-1) %>%
  select(-value, YoY) %>%
  pivot_longer(ThreeMonth:SixMonth, names_to = "time_length", values_to = "change") %>%
  mutate(time_length = str_replace_all(time_length,"SixMonth", "6-Month Change")) %>%
  mutate(time_length = str_replace_all(time_length,"ThreeMonth", "3-Month Change")) %>%
  mutate(last_value = ifelse(date==max(date),change,NA))

one_month <- cpi %>% filter(item_name == "All items less food, shelter, energy, and used cars and trucks") %>%
  select(date, Pchange1a) %>% mutate(time_length = "3-Month Change")

MI_dates <- cpi %>% filter(date > "2010-12-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates = MI_dates[seq(1, length(MI_dates), 12)]

date_start = "2017-01-01"
date_end = "2019-01-01"
date_period <- interval(date_start, date_end)
date_period = date_period %/% months(1)

pre_core <- cpi %>% filter(item_name == "All items less food, shelter, energy, and used cars and trucks", date == date_start | date == date_end) %>%
  mutate(change = value/lag(value,1)) %>% filter(!is.na(change)) %>% mutate(change = change^(12/date_period) - 1) %>% select(change)
pre_core <- as.numeric(pre_core)

core2 <- core %>% filter(date >= "2017-01-01") %>%
  left_join(one_month, by=c("date","time_length")) %>%
  mutate(pre_core_line = pre_core) %>%
  mutate(type = "Supercore: All items less food, shelter, energy, and used cars")


rbind(core1, core2) %>%
  ggplot(aes(date, change, color=time_length, label=label_percent()(round(last_value,3)))) + geom_line(size=1.6) +
  labs(x="", y="",
       title=title_longer_graphic,
       subtitle = paste("CPI inflation category, percentage change, annualized. Dotted line represents 2017 to 2019 values.", sep=""),
       caption = "All items less food and energy, monthly percent change, BLS, Author's calculations. Mike Konczal, Roosevelt Institute.") +
  theme_lass + facet_wrap(~type) +
  geom_line(aes(date, pre_core_line)) +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  theme(legend.position = c(0.65,0.85), legend.text = element_text(size=15)) +
  scale_color_manual(values=c("#2D779C", "#A4CCCC")) +
  geom_text_repel(show.legend=FALSE, nudge_x = 135, min.segment.length = Inf) +
  geom_col(aes(date, Pchange1a), alpha=0.1, size=0, show.legend = FALSE)

ggsave("graphics/three_six_core_inflation.png", dpi="retina", width = 12, height=6.75, units = "in")

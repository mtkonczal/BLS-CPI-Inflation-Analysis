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
title0 <- "Core Inflation Stays in a New, Lower, Range"
title1 <- "Core inflation drops for two months in a row, the first time since 2021"
title2 <- "Core services outside housing flat again for two months in a row"
title3 <- "Housing Leveled, Services Slowing, But Goods Back on the Rise"
title4 <- "Goods continue into deflation, with autos leading the way"
title5 <- "3-month inflation finally breaks lower"
title_longer_graphic <- "Inflation is Moving Sideways Recently"
title_energy_food <- "Food Inflation is Slowing While Energy Falls"

#### QUICK CHECK ###
item_basket_watch_categories <- c("All items", "New and used motor vehicles", "Shelter", "Food", "Energy", "Commodities less food and energy commodities",
                                  "All items less food and energy","Services less rent of shelter")

watch_values <- cpi %>% filter(item_name %in% item_basket_watch_categories) %>%
  select(date, item_name, Pchange1, Pchange1a, Wchange1a) %>%
  filter(date >= (max(date) %m-% months(2))) %>%
  mutate(Pchange1 = Pchange1*100, Pchange1a = Pchange1a*100, Wchange1a = Wchange1a*100) %>%
  arrange(item_name)

#View(watch_values)

##### Graphic1 Newer: Older Core #####

##### Graphic1: Core Inflation ####
MI_dates <- cpi %>% filter(date > "2017-12-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates_three = MI_dates[seq(1, length(MI_dates), 6)]

trend_2018_2020 <- cpi %>% filter(date > "2017-12-01", item_name == "All items less food and energy") %>%
  filter(date == min(date) | date == "2020-01-01") %>%
  arrange(date) %>%
  mutate(num_months = interval(lag(date,1), date)) %>%
  mutate(num_months2 = num_months %/% months(1)) %>%
  select(date, value, num_months2) %>%
  mutate(trend = value/lag(value,1)) %>%
  summarize(trenda = trend^(12/num_months2)-1) %>% filter(!is.na(trenda))

trend_2018_2020 <- as.numeric(trend_2018_2020)

cpi %>% filter(date > "2017-12-01", item_name == "All items less food and energy") %>%
  select(date, item_name, Pchange1a) %>%
  mutate(num_label = round(100*Pchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  mutate(num_label2 = ifelse(date >= "2021-10-01", num_label, NA)) %>%
  mutate(trend = trend_2018_2020) %>%
  ggplot(aes(x = date, y = Pchange1a, fill = item_name, label = num_label)) + theme_lass +
  geom_bar(stat = 'identity', size=0, fill="#2D779C") +
  geom_line(aes(date,trend), linetype=2, lineend="square", size=1, color="#E2E47E") +
  labs(y = NULL,
       x = NULL,
       title = title0,
       subtitle = "Monthly percent increase in core goods and services, annualized. Dotted line is 2018-2019 value.",
       caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  geom_text(aes(x=date, y=Pchange1a, label=num_label2), nudge_y = 0.003, size=3, color="#E2E47E") +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates_three)

ggsave("graphics/g1_core_newer.png", dpi="retina", width = 12, height=6.75, units = "in")


##### Graphic 2: Goods Versus Services with labels ####
MI_Graphic2a <- cpi %>% filter(item_name %in% c("Services less energy services", "Commodities less food and energy commodities"), date > "2019-01-01") %>%
  mutate(in_range = ifelse(date >= "2021-10-01", value, NA), trend = mean(in_range, na.rm=TRUE),
         trend = ifelse(date >= "2021-10-01", trend, NA))

placement <- MI_Graphic2a %>% select(Wchange1a,date,item_name) %>%
  mutate(Wchange1a2 = if_else(Wchange1a < 0, 0, Wchange1a)) %>%
  group_by(date) %>% summarize(place_y = sum(Wchange1a2)) %>%
  ungroup() %>% mutate(item_name = "Services less energy services")

MI_Graphic2a <- cpi %>% filter(item_name == "All items less food and energy", date > "2021-12-01") %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  select(date, num_label) %>% full_join(MI_Graphic2a, by="date") %>%
  left_join(placement, by=c("date","item_name"))

MI_Graphic2a$item_name <- str_replace_all(MI_Graphic2a$item_name, "Services less energy services", "Core Services")
MI_Graphic2a$item_name <- str_replace_all(MI_Graphic2a$item_name, "Commodities less food and energy commodities", "Core Goods")

MI_dates <- unique(MI_Graphic2a$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates = MI_dates[seq(1, length(MI_dates), 3)]

MI_Graphic2a %>% ggplot(aes(x = date, y = Wchange1a, fill = item_name)) +
  geom_bar(stat = 'identity', size=0) + theme_lass +
  labs(y = NULL,
       x = NULL,
       title = title1,
       subtitle ="",
       caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  geom_text(aes(x=date, y=place_y, label=num_label), nudge_y = 0.003, size=4, color="pink") +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size=18),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))

ggsave("graphics/g2_services_goods.png", dpi="retina", width = 12, height=6.75, units = "in")


##### FOOD AND ENERGY #####
food_energy_dates <- cpi %>% filter(date >= "2017-01-01") %>% select(date)
food_energy_dates <- unique(food_energy_dates$date)
food_energy_dates <- sort(food_energy_dates, decreasing = TRUE)
food_energy_dates = food_energy_dates[seq(1, length(food_energy_dates), 6)]

cpi %>% filter(item_name %in% c("Food", "Energy")) %>%
  filter(date >= "2017-01-01") %>%
  ggplot(aes(x = date, y = Wchange1a, fill = item_name)) +
  geom_bar(stat = 'identity', size=0) + theme_lass +
  labs(y = NULL,
       x = NULL,
       title = title_energy_food,
       subtitle = "Monthly Contribution to Inflation, Annualized.",
       caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="RdYlGn") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=food_energy_dates) +
  #geom_text(aes(x=date, y=place_y, label=num_label), nudge_y = 0.003, size=4, color="pink") +
  theme(legend.position = c(0.7,0.85), legend.title = element_blank(), legend.text = element_text(size=18),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))

ggsave("graphics/energy_food.png", dpi="retina", width = 12, height=6.75, units = "in")


###### Graphic 3: Services Breakdown #####
cpi %>% filter(date > "2020-12-01", item_name %in% c("Services less energy services", "Shelter")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core services` = `Services less energy services` - `Shelter`) %>%
  select(-`Services less energy services`) %>%
  pivot_longer(c(Shelter, `Rest of core services`), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("Shelter", "Rest of core services"))) %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity', size=0) +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = title2,
       subtitle = "Monthly core services contribution to inflation, annualized.",
       caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  geom_text(size = 4, position = position_stack(vjust = 0.5), color="black") +
  scale_fill_brewer(palette="RdPu") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  theme(legend.position = c(0.25,0.75), legend.title = element_blank(), legend.text = element_text(size=16),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))

ggsave("graphics/g3_services.png", dpi="retina", width = 12, height=6.75, units = "in")


#### THREE CATEGORIES ####

MI_dates_three_categories <- cpi %>% filter(date > "2017-12-01")
MI_dates_three_categories <- unique(MI_dates_three_categories$date)
MI_dates_three_categories <- sort(MI_dates_three_categories, decreasing = TRUE)
MI_dates_three_categories = MI_dates_three_categories[seq(1, length(MI_dates_three_categories), 12)]


cpi %>% filter(date > "2017-12-01", item_name %in% c("Services less energy services", "Shelter", "Commodities less food and energy commodities")) %>%
  mutate(item_name = str_replace_all(item_name, "Commodities less food and energy commodities","Core_goods")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core services` = `Services less energy services` - `Shelter`) %>%
  select(-`Services less energy services`) %>%
  pivot_longer(c(Shelter, `Rest of core services`,Core_goods), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = str_replace_all(item_name, "Core_goods","Core goods")) %>%
  mutate(item_name = factor(item_name, levels = c("Core goods", "Shelter", "Rest of core services"))) %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  
  ggplot(aes(x = date, y = Wchange1a, fill=item_name)) +
  geom_bar(stat = 'identity', size=0) +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  facet_grid(~item_name) +
  labs(y = NULL,
       x = NULL,
       title = title3,
       subtitle = "Monthly contribution to inflation, annualized.",
       caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  scale_fill_brewer(palette="RdPu", name = "item_name") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates_three_categories) +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))

ggsave("graphics/three_categories.png", dpi="retina", width = 12, height=6.75, units = "in")
#####


###### Graphic 3: Services Breakdown Medical #####
cpi %>% filter(date > "2020-12-01", item_name %in% c("Services less energy services", "Shelter", "Medical care services","Food away from home")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core services` = `Services less energy services` - `Medical care services` - `Shelter`) %>%
  select(-`Services less energy services`) %>%
  pivot_longer(c(Shelter, `Medical care services`, `Rest of core services`,`Food away from home`), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("Food away from home","Shelter", "Medical care services","Rest of core services"))) %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity', size=0) +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "Services Inflation Down Across Categories",
       subtitle = "Monthly core services contribution to Inflation, Annualized.",
       caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  geom_text(size = 4, position = position_stack(vjust = 0.5), color="black") +
  scale_fill_brewer(palette="RdPu") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  theme(legend.position = c(0.4,0.80), legend.title = element_blank(), legend.text = element_text(size=18),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))

ggsave("graphics/g3_services_medical.png", dpi="retina", width = 12, height=6.75, units = "in")

###### GRAPHIC 3a : Goods Breakdown - Autos ####

cpi %>% filter(date > "2020-12-01", item_name %in% c("Commodities less food and energy commodities", "Transportation commodities less motor fuel")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core goods` = `Commodities less food and energy commodities` - `Transportation commodities less motor fuel`) %>%
  select(-`Commodities less food and energy commodities`) %>%
  rename(`All autos` = `Transportation commodities less motor fuel`) %>%
  pivot_longer(c(`All autos`, `Rest of core goods`), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("All autos", "Rest of core goods"))) %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity', size=0) +
  labs(y = NULL,
       x = NULL,
       title = title4,
       subtitle = "Monthly core goods contribution to inflation, annualized.",
       caption ="Autos is New and Used Cars and Motor Parts. BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  geom_text(size = 4, position = position_stack(vjust = 0.5), color="black") +
  scale_fill_brewer(palette="Greens") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  theme(legend.position = c(0.85,0.65), legend.title = element_blank(), legend.text = element_text(size=16),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))


ggsave("graphics/g3_goods_autos.png", dpi="retina", width = 12, height=6.75, units = "in")


##### Health Insurance ######
HI_dates <- cpi_data %>% filter(item_name %in% c("Health insurance")) %>%
  filter(period != "M13", begin_period != "S01") %>% filter(area_code == "0000") %>%
  select(date)


HI_dates <- unique(HI_dates$date)
HI_dates <- sort(HI_dates, decreasing = TRUE)
HI_dates = HI_dates[seq(1, length(HI_dates), 12)]

cpi_data %>% filter(item_name %in% c("Health insurance")) %>%
  filter(period != "M13", begin_period != "S01") %>% filter(area_code == "0000") %>%
  arrange(date) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%

  ggplot(aes(x = date, y = Wchange1a)) +
  geom_bar(stat = 'identity', size=0) + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "Health Insurance Goes Sharply Negative, Likely to Continue",
       subtitle = "Monthly contribution to inflation, annualized. Values tend to reset Sep/Oct.",
       caption ="BLS, CPI, 2022 weights prior to 2023, Seasonally Unadjusted. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b %y", breaks=HI_dates) +
  theme(panel.grid.major.x = element_line(size=1),
        panel.grid.major.y = element_blank()
        )

ggsave("graphics/g1_health_insurance.png", dpi="retina", width = 12, height=6.75, units = "in")


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

extra_value <- core %>% group_by(time_length) %>% filter(year(date)==2022) %>% filter(change == max(change)) %>%
  ungroup() %>% select(date, time_length, extra_value = change)

MI_dates <- cpi %>% filter(date > "2010-12-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates = MI_dates[seq(1, length(MI_dates), 12)]

date_start = "2014-01-01"
date_end = "2019-01-01"
date_period <- interval(date_start, date_end)
date_period = date_period %/% months(1)

pre_core <- cpi %>% filter(item_name == "All items less food and energy", date == date_start | date == date_end) %>%
  mutate(change = value/lag(value,1)) %>% filter(!is.na(change)) %>% mutate(change = change^(12/date_period) - 1) %>% select(change)
pre_core <- as.numeric(pre_core)

core %>% filter(date > "2014-12-01") %>%
  left_join(extra_value, by=c("date","time_length")) %>%
  mutate(last_value = ifelse(!is.na(extra_value),extra_value,last_value)) %>%
  ggplot(aes(date, change, color=time_length, label=label_percent()(last_value))) + geom_line(size=1.2) +
  labs(x="", y="",
       title=title_longer_graphic,
       subtitle = paste("Core CPI inflation, monthly percentage change, annualized. Dotted line represented 2014 to 2019 value of ", round(pre_core,3)*100, "%, annualized.", sep=""),
       caption = "All items less food and energy, monthly percent change, BLS, Author's calculations. Mike Konczal, Roosevelt Institute.") +
  theme_lass +
  geom_hline(yintercept = pre_core, linetype="dashed", color="#A4CCCC") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  theme(legend.position = c(0.90,0.15), legend.text = element_text(size=15)) +
  scale_color_manual(values=c("#2D779C", "#A4CCCC")) +
  geom_text_repel(show.legend=FALSE)

ggsave("graphics/three_six_core_inflation.png", dpi="retina", width = 12, height=6.75, units = "in")

#### RIDGELINE GRAPH

median_terms <- read_csv("weights/mediancpi_component_table.csv") %>% mutate(item_name = Component)
median <- cpi %>%
  filter(item_name %in% median_terms$item_name | item_name == "Owners' equivalent rent of residences") %>%
  filter(!is.na(date)) %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Pchange1a = (1 + Pchange1)^12 - 1) %>%
  ungroup() %>%
  
  group_by(date) %>%
  mutate(normalized = sum(weight)) %>%
  mutate(weightN = weight/normalized) %>%
  arrange(Pchange3) %>%
  mutate(cumsum = cumsum(weight)/100) %>%
  mutate(cumsumN = cumsum(weightN)) %>%
  ungroup() %>%
  mutate(Pchange3a = (1+Pchange3)^4-1)

#THIS IS THE GRAPHIC - 30 percent-trimmed distribution
median %>% mutate(dateF = as.factor(date)) %>%
  filter(cumsumN <= 0.85 & cumsum >= 0.15) %>%
  mutate(Pchange3a = (1+Pchange3)^4-1) %>%
  filter(date >= "2017-06-01") %>%
  filter(date != "2020-06-01") %>%
  filter(month(date) %in% c(1,4,7,10)) %>%
  mutate(monthC = format(date, "%B, %Y")) %>%
  mutate(monthC = fct_reorder(monthC,date)) %>%
  mutate(monthCR = fct_rev(monthC)) %>%
  ggplot(aes(x = Pchange3a, y = monthCR, fill = stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis(option = "H") +
  theme_ridges() + theme_lass +
  theme(legend.position = "none") +
  scale_x_continuous(labels = percent) +
  labs(title="Distribution of Price Increases Moved Out, But Now Returning Back",
       subtitle="Distribution of the Cleveland Fed's Median/Trimmed-Mean CPI price basket, 3-month change annualized, with components\nwhose expenditure weights fall above/below the 85/15th percentile of price changes removed.",
       x="Three Month Percent Change", y="", caption="OER is treated as one value, instead of broken out by region and manually seasonally adjusted as per Cleveland Fed's methodology.\nJune 2020 removed as negative outlier. Mike Konczal, Roosevelt Institute") +
  theme(plot.title.position = "plot", legend.position = "none", legend.title = element_blank(),
        plot.title = element_text(size = 25,margin=margin(0,0,5,0)),
        plot.subtitle = element_text(size=15),
        plot.caption = element_text(size=10, face="italic"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.text.x = element_text(size=12))

ggsave("graphics/trimmed_dist.png", dpi="retina", width = 12, height=12, units = "in", bg = "white")





##### DOUBLE GRAPHIC - CHANGING MEASURES ####
MI_dates <- cpi %>% filter(date > "2010-12-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates = MI_dates[seq(1, length(MI_dates), 12)]


core_services <- cpi %>% filter(item_name == "Services less rent of shelter") %>%
  select(date, value) %>%
  mutate(ThreeMonth = (value/lag(value,3))^4-1) %>%
  mutate(SixMonth = (value/lag(value,6))^2-1) %>%
  mutate(YoY = (value/lag(value,12))-1) %>%
  select(-value, YoY) %>%
  pivot_longer(ThreeMonth:SixMonth, names_to = "time_length", values_to = "change") %>%
  mutate(time_length = str_replace_all(time_length,"SixMonth", "6-Month Change")) %>%
  mutate(time_length = str_replace_all(time_length,"ThreeMonth", "3-Month Change")) %>%
  mutate(last_value = ifelse(date==max(date),change,NA)) %>%
  mutate(type = "Services less rent of shelter")

supercore <- cpi %>% filter(item_name == "All items less food, shelter, energy, and used cars and trucks") %>%
  select(date, value) %>%
  mutate(ThreeMonth = (value/lag(value,3))^4-1) %>%
  mutate(SixMonth = (value/lag(value,6))^2-1) %>%
  mutate(YoY = (value/lag(value,12))-1) %>%
  select(-value, YoY) %>%
  pivot_longer(ThreeMonth:SixMonth, names_to = "time_length", values_to = "change") %>%
  mutate(time_length = str_replace_all(time_length,"SixMonth", "6-Month Change")) %>%
  mutate(time_length = str_replace_all(time_length,"ThreeMonth", "3-Month Change")) %>%
  mutate(last_value = ifelse(date==max(date),change,NA)) %>%
  mutate(type = "All items less food, shelter, energy, and used cars and trucks") %>%
  rbind(core_services)

date_start = "2010-01-01"
date_end = "2019-01-01"
date_period <- interval(date_start, date_end)
date_period = date_period %/% months(1)

pre_supercore <- cpi %>% filter(item_name == "Services less rent of shelter" | item_name == "All items less food, shelter, energy, and used cars and trucks",
                                date == date_start | date == date_end) %>% group_by(item_name) %>%
  mutate(change = value/lag(value,1)) %>% filter(!is.na(change)) %>% mutate(change = change^(12/date_period) - 1) %>% select(pre_values = change, type = item_name)


supercore %>% filter(date > "2014-12-01") %>%
  left_join(pre_supercore, by=c("type")) %>%
  mutate(type = str_replace_all(type, "Services less rent of shelter", "Non-housing services")) %>%
  mutate(type = str_replace_all(type, "All items less food, shelter, energy, and used cars and trucks", "Supercore: All items less food, shelter, energy, and used autos")) %>%
  ggplot(aes(date, change, color=time_length, label=label_percent(accuracy=0.1)(last_value))) + geom_line(size=1.2) + facet_wrap(~type) +
  geom_line(aes(date,pre_values), linetype="dashed", color="#FFD3B5") +
  labs(x="", y="",
       title="Even Supercore Inflations Are Moving Sideways Right Now",
       subtitle = "Monthly percent change, annualized.",
       caption = "BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculations. Mike Konczal, Roosevelt Institute.") +
  theme_lass +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  theme(legend.position = c(0.90,0.15), legend.text = element_text(size=15)) +
  scale_color_manual(values=c("#FFD3B5", "#F67280")) +
  geom_text(show.legend=FALSE, nudge_x = 155)

ggsave("graphics/three_six_supercores.png", dpi="retina", width = 12, height=6.75, units = "in")


#### DISTRIBUTION 3 PERCENT ####

lowest <- read_csv("weights/most_prices.csv") %>% filter(category != "Meta", lowest == 1)

MI_dates <- cpi %>% filter(date > "2012-01-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates_three = MI_dates[seq(1, length(MI_dates), 12)]

cpi %>% filter(item_name %in% lowest$item_name, !is.na(Pchange1), date > "2012-01-01") %>% mutate(p3 = (Pchange1a > 0.03)) %>%
  group_by(date) %>%
  summarize(total_3 = sum(p3)/n()) %>% ungroup() %>% ungroup() %>%
  mutate(last_value = ifelse(date==max(date) | total_3==max(total_3),total_3,NA)) %>%
  ggplot(aes(date, total_3, label=label_percent()(last_value))) + geom_line(size=1) + theme_lass +
  labs(y = NULL,
       x = NULL,
       title = "Percent of Items With 3% Price Growth Has Decreased From High Levels",
       subtitle = "Percent of 141 CPI items having at least 3 percent monthly price increases, annualized.",
       caption ="BLS, CPI, only seasonally adjusted items included. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates_three) +
  geom_text(show.legend=FALSE, nudge_x = 80, nudge_y = 0.011)
  

ggsave("graphics/three_percent_growth.png", dpi="retina", width = 12, height=6.75, units = "in")


##### just used autos ####
###### Used Cars - Short and Long ########
HI_dates <- cpi %>% filter(date >= "2019-06-01")
HI_dates <- unique(HI_dates$date)
HI_dates <- sort(HI_dates, decreasing = TRUE)
HI_dates = HI_dates[seq(1, length(HI_dates), 6)]

cpi %>% filter(date >= "2019-06-01", item_name %in% c("Commodities less food and energy commodities", "Used cars and trucks")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core goods` = `Commodities less food and energy commodities` - `Used cars and trucks`) %>%
  select(-`Commodities less food and energy commodities`) %>%
  rename(`Used autos` = `Used cars and trucks`) %>%
  pivot_longer(c(`Used autos`, `Rest of core goods`,), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("Used autos", "Rest of core goods"))) %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity', size=0) +
  labs(y = NULL,
       x = NULL,
       title = "Recent Deflation in Used Cars Couldn't Offset Overall Goods Inflation",
       subtitle = "Monthly contribution to inflation, annualized.",
       caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  geom_text(size = 2, position = position_stack(vjust = 0.5), color="black") +
  scale_fill_brewer(palette="Greens") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=HI_dates) +
  theme(legend.position = c(0.30,0.75), legend.title = element_blank(), legend.text = element_text(size=16),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))


ggsave("graphics/g3_goods_just_used_autos.png", dpi="retina", width = 12, height=6.75, units = "in")


##### FOOD #####


food_index <- c("Rice, pasta, cornmeal","Breakfast cereal", "Flour and prepared flour mixes",
                "Bread", "Fresh biscuits, rolls, muffins", "Cakes, cupcakes, and cookie",
                "Beef and veal", "Pork", "Other meats", "Poultry", "Fish and seafood", "Eggs",
                "Milk", "Fresh fruits", "Fresh vegetables", "Processed fruits and vegetables", "Juices and nonalcoholic drinks",
                "Beverage materials including coffee and tea", "Fats and oils", "Sugar and sweets")

# total weights of food_index
cpi %>% filter(item_name %in% food_index, date == max(date)) %>% summarize( n = sum(weight))
cpi %>% filter(item_name == "Food at home", date == max(date)) %>% summarize( n = sum(weight))

#### GRAPHIC FOOD ####
# Comparison point
#food_dates <- cpi %>% filter(item_name %in% food_index) %>% filter(date == max(date) | date == max(date) %m-% months(7) | date == max(date) %m-% months(4)) %>%
food_dates <- cpi %>% filter(item_name %in% food_index) %>% filter(date == max(date) | date == "2022-08-01" | date == "2022-04-01") %>%
  mutate(date = paste(as.character(month(date, label = TRUE, abbr = FALSE)), ", ", as.character(year(date)), sep = "")) %>%
  mutate(date = factor(date,levels=c("January, 2023","August, 2022","April, 2022")), name = reorder_within(item_name, Pchange3, date))

ggplot(food_dates, aes(name, Pchange3, fill = date)) +
  geom_col(show.legend = FALSE, size=0) +
  facet_wrap(~date, scales = "free_y", ncol=1) + theme_lass +
  coord_flip() +
  scale_x_reordered() +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot") +
  labs(y = "Price Increase Over Previous Three Months",
       x = NULL,
       title = "Food Price Increases Have Broadened Over The Past Year",
       subtitle = "Price increase over previous three months of listed date. These categories represent 68 percent of all spending on food at home.",
       caption ="BLS, CPI, all subcategories of 'Food at home.' Seasonally Adjusted. Author's Calculation. @rortybomb") +

  theme(panel.grid.major.x = element_line(size=0.5)) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot") +
  labs(y = "Price Increase Over Previous Three Months",
       x = NULL,
       title = "Food Price Increases Have Narrowed Recently, Though Eggs Remain High",
       subtitle = "Price increase over previous three months of date. Categories represent 68 percent of spending on food at home.",
       caption ="BLS, CPI, all subcategories of 'Food at home.' Seasonally Adjusted. Author's Calculation. @rortybomb") +
  theme(axis.text.y = element_text(size=5),
  axis.text.x = element_text(size=12, face="bold"), strip.text = element_text(color="white", size = 8)) +
  theme(panel.spacing.y=unit(0, "lines"), panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size=12, color="white"))

ggsave("graphics/food_chart.png", dpi="retina", width = 12, height=6.75, units = "in")
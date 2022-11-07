###############################################################
# Graphics that I'm watching, Summer 2022, inflation
# Mike Konczal
# Last updated 9/12/2022

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/")
library(janitor)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(huxtable)
library(scales)
library(lubridate)
library(tidytext)
library(viridis)

##### SET UP SOME THINGS #####

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


#### TIK TOK GRAPHICS ####

# THIS WORKS
cpi %>% filter(item_name %in% c("Services less energy services", "Commodities less food and energy commodities"), date > "2022-04-01") %>%
  mutate(title = if_else(item_name == "Services less energy services", "Core Services", "Core Goods")) %>%
  mutate(num_value = ifelse(Wchange1a > 0, Wchange1a, 0)) %>%
  group_by(date) %>%
  mutate(num_label = sum(round(100*num_value, 2))) %>% mutate(num_label = num_label/100) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = Wchange1a, fill = title, label=scales::percent (num_label,accuracy=0.1))) +
  geom_bar(stat = 'identity') + theme_classic() +
  theme(legend.position = "top", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "") +
  scale_fill_brewer(palette="Paired") +
  theme(plot.title = element_blank(), axis.text=element_text(size=25),
        legend.text = element_text(size = 30),
        axis.line = element_blank(),
        axis.text.y = element_blank()) +
  geom_text(aes(x=date, y=num_label+0.0015), size=10) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%B", breaks = "1 month", position="top")

ggsave("graphics/tiktok_core.png", dpi="retina", width = 9, height=16, units = "in")

#This works
cpi %>% filter(date > "2020-12-01", item_name %in% c("Services less energy services", "Shelter")) %>%
  filter(date > "2022-04-01") %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of services` = `Services less energy services` - `Shelter`) %>%
  select(-`Services less energy services`) %>% rename(Housing = `Shelter`) %>%
  pivot_longer(c(Housing, `Rest of services`), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("Airline fares", "Housing", "Rest of services"))) %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.31, NA, num_label)) %>%
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity') + theme_classic() +
  theme(legend.position = "top", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "") +
  scale_fill_brewer(palette="Paired") +
  theme(plot.title = element_blank(), axis.text=element_text(size=25),
        legend.text = element_text(size = 30),
        axis.line = element_blank(),
        axis.text.y = element_blank()) +
  geom_text(size = 15, position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%B", breaks = "1 month", position="top")
  
ggsave("graphics/tiktok_core2.png", dpi="retina", width = 9, height=16, units = "in")



##### 3 and 6 month core ####
core <- cpi %>% filter(item_name == "All items less food and energy") %>%
  mutate(change = value - lag(value,1), change = change/lag(value,1)) %>%
  mutate(ThreeMonth = change + lag(change,1) + lag(change,2)) %>%
  mutate(SixMonth = ThreeMonth + lag(change,3) + lag(change,4) + lag(change,5)) %>%
  mutate(NineMonth = SixMonth + lag(change,6) + lag(change,7) + lag(change,8)) %>%
  mutate(ThreeMonth = ThreeMonth/3, SixMonth = SixMonth/6, NineMonth = NineMonth/9) %>%
  rename(`Three Month` = ThreeMonth,
         `Six Month` = SixMonth,
         `Nine Month` = NineMonth) %>%
  pivot_longer(c(`Three Month`,`Six Month`,`Nine Month`), names_to = "item_names", values_to = "values")


core %>% filter(date > "2022-05-01") %>%
  rename(`Average Inflation:` = item_names) %>%
  ggplot(aes(x = date, y = values, color = `Average Inflation:`, label=scales::percent (values,accuracy=0.1))) +
  geom_line(size=3) + theme_classic() +
  theme(legend.position = "top", legend.title = element_text(size=16), legend.text = element_text(size=16)) + 
  labs(y = NULL,
       x = NULL,
       title = "") +
  scale_fill_brewer(palette="Paired") +
  theme(plot.title = element_blank(), axis.text=element_text(size=25),
        axis.line = element_blank()) +
  scale_y_continuous(labels = percent, limits = c(0.003,0.008)) +
  scale_x_date(date_labels = "%b", breaks = "1 month", position="top") 


ggsave("graphics/tiktok_averages3.png", dpi="retina", width = 9, height=16, units = "in")
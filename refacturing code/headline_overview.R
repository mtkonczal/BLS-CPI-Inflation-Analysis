# For a job writing assignment memo. May-23-2022. Mike Konczal
setwd("/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/")
library(janitor)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(huxtable)
library(scales)
library(lubridate)

##### SET UP SOME THINGS #####
source(file = "1_load_cpi_data.R")

item_basket_topline <- c("All items", "All items less food and energy", "Energy", "Food", "Commodities less food and energy commodities",
                         "Services less energy services", "Airline fares",
                         "Shelter", "Transportation commodities less motor fuel", "New vehicles", "Used cars and trucks")

item_basket_watch_categories <- c("All items", "New and used motor vehicles", "Shelter", "Other services",
                                  "Medical care services", "Food", "Energy", "Commodities less food and energy commodities")

cpi0 <- cpi_data %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
  mutate(Pchange3 = (value/lag(value, 3)-1)) %>%
  mutate(Wchange3 = (Pchange3*weight)/100) %>%
  mutate(Wchange3a = (1 + Wchange3)^4 - 1) %>%
  mutate(Pchange6 = (value/lag(value, 6)-1)) %>%
  mutate(Wchange6 = (Pchange3*weight)/100) %>%
  mutate(Wchange6a = (1 + Wchange3)^2 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100) %>%
  ungroup()

months2022 <- interval(ymd("2021-12-01"), max(cpi0$date))
months2022 = months2022 %/% months(1)

average_month_pre_pandemic <- cpi0 %>%
  filter(date == "2014-01-01" | date == "2019-12-01") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(pre_value = (1+ (value/lag(value)-1)*weight/100)^(1/6)-1) %>%
  ungroup() %>%
  filter(!is.na(pre_value))

value_2021 <- cpi0 %>%
  filter(date == "2020-12-01" | date == "2021-12-01") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(v_2021 = (1+ (value/lag(value)-1)*weight/100)^(1/1)-1) %>%
  ungroup() %>%
  filter(!is.na(v_2021))

recovery_period_months <- interval(ymd("2020-12-01"), max(cpi0$date))
recovery_period_months = recovery_period_months %/% months(1)

recovery_period <- cpi0 %>%
  filter(date == "2020-12-01" | date == max(date)) %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(recovery_inflaton = (1+ (value/lag(value)-1)*weight/100)^(12/recovery_period_months)-1) %>%
  ungroup() %>%
  filter(!is.na(recovery_inflaton))

cpi <- cpi0 %>%
  group_by(item_name) %>%
  mutate(Pchange_2022 = value/lag(value, months2022)-1) %>%
  mutate(Wchange_2022 = (Pchange_2022*weight)/100) %>%
  mutate(Wchange_2022a = (1+Wchange_2022)^(12/months2022)-1) %>%
  select(item_name, date, value, weight, Pchange1, Wchange1a, Pchange_2022, Wchange_2022a, Wchange12) %>%
  ungroup() %>%
  left_join(average_month_pre_pandemic, by="item_name") %>%
  left_join(value_2021, by="item_name") %>%
  left_join(recovery_period, by="item_name")

##################

cpi %>% filter(item_name %in% item_basket_topline) %>%
  filter(date == max(date)) %>%
  select(item_name, `Contribution to Inflation, 2021` = v_2021, `Contribution to Inflation, 2022` = Wchange_2022a, pre_value)

cpi %>% filter(item_name %in% c("Medical care services","Education and communication services")) %>%
  filter(date == max(date)) %>%
  select(item_name, `Before Crisis Value` = pre_value, `Contribution to Inflation, 2021` = v_2021, `Contribution to Inflation, 2022` = Wchange_2022a)

a <- cpi %>% filter(item_name == "Food", date > "2020-12-01")

cpi %>% filter(item_name == "Used cars and trucks", date > "2020-12-01")

cpi %>% filter(item_name == "Transportation commodities less motor fuel", date > "2021-12-01")

cpi %>% filter(item_name == "Commodities less food and energy commodities")

cpi %>% filter(item_name == "All items less food and energy") %>%
  select(item_name, date, Pchange1, Wchange1a, Wchange12) %>%
  ggplot(aes(date, Wchange12)) + geom_line() + theme_classic()
###################

tail(cpi %>% filter(item_name == "All items less food and energy") %>%
       select(item_name, date, Pchange1, Wchange12))

tail(cpi %>% filter(item_name == "Energy"))

###### JW'S TEST #####

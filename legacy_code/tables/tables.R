###############################################################
# Graphics that I'm watching, Summer 2022, inflation
# Mike Konczal
# Last updated 11/07/2022

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
  mutate(Pchange3a = (1+Pchange3)^4 - 1) %>%
  mutate(three_months = (value/lag(value, 3)-1)) %>%
  mutate(Wthree_months = (three_months*weight)/100) %>%
  mutate(Wthree_monthsA = (1 + Wthree_months)^4 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100) %>%
  ungroup()


categories_number <- cpi %>% filter(item_name %in% c("Services less rent of shelter","Shelter","Commodities less food and energy commodities"))

three_month <- categories_number %>% filter(date == max(date)) %>% select(item_name, three_months = Wthree_monthsA)
previous_12 <- categories_number %>% filter(date == "2022-09-01") %>% select(item_name, previous_12 = Wchange12)
pre_time <- categories_number %>% filter(date == "2019-01-01") %>% select(item_name, pre_time = Wchange12)

inflation_table <- pre_time %>% left_join(previous_12, by="item_name") %>% left_join(three_month, by="item_name") %>%
  mutate(item_name = str_replace_all(item_name, "Commodities less food and energy commodities", "Core goods")) %>%
  select(`Item Name` = item_name, `2019 Values` = pre_time, `Sept 2021 to Sept 2022` = previous_12, `Last Three Months` = three_months)

###############################################################
# Code to evaluate the 'Handoff Theory' of inflation normalizing in 2021-2022.
# Mike Konczal
# Last updated 4/10/22

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/")
library(tidyverse)

##### SET UP SOME THINGS #####

#Either run Script 1 to download data fresh, or used locally stored data.
source(file = "1_load_cpi_data.R")
#load("data/cpi_data.RData")

#######
#SET UP DATA:
cpi <- cpi_data %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
  mutate(three_months = (lag(value, 1)/lag(value, 4)-1)) %>%
  mutate(Wthree_months = (three_months*weight)/100) %>%
  mutate(Wthree_monthsA = (1 + Wthree_months)^4 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100) %>%
  ungroup()

######

item_basket <- c("All items less food and energy", "Commodities less food and energy commodities", "Transportation commodities less motor fuel",
                 "Services less energy services", "Shelter", "Core Goods Minus Autos", "Core Services Minus Shelter")

# Calculate observations for Core Goods Minus Autos.
minus_autos <- cpi %>% filter(item_name %in% c("Commodities less food and energy commodities", "Transportation commodities less motor fuel")) %>%
  group_by(date) %>%
  mutate(inside_difference1 = abs(Wchange1a - lag(Wchange1a,1)), inside_difference2 = abs(Wthree_monthsA - lag(Wthree_monthsA,1))) %>%
  mutate(inside_difference12 = abs(Wchange12 - lag(Wchange12,1)), temp_name = "Core Goods Minus Autos") %>% ungroup() %>%
  filter(!is.na(inside_difference12)) %>%
  select(date, item_name = temp_name, Wchange1a = inside_difference1, Wthree_monthsA = inside_difference2, Wchange12 = inside_difference12) %>%
  arrange(date)

# Calculate observations for Core Services Minus Shelter.
minus_shelter <- cpi %>% filter(item_name %in% c("Services less energy services", "Shelter")) %>%
  group_by(date) %>%
  mutate(inside_difference1 = abs(Wchange1a - lag(Wchange1a,1)), inside_difference2 = abs(Wthree_monthsA - lag(Wthree_monthsA,1))) %>%
  mutate(inside_difference12 = abs(Wchange12 - lag(Wchange12,1)), temp_name = "Core Services Minus Shelter") %>% ungroup() %>%
  filter(!is.na(inside_difference12)) %>%
  select(date, item_name = temp_name, Wchange1a = inside_difference1, Wthree_monthsA = inside_difference2, Wchange12 = inside_difference12) %>%
  arrange(date)

# Add those observations to the overall CPI dataset before calculating values.
cpi_with_minus <- cpi %>% select(date, item_name, Wchange1a, Wthree_monthsA, Wchange12) %>%
  rbind(minus_autos) %>% rbind(minus_shelter) %>% arrange(date)

# Start selecting the observations to look for
most_recent <- cpi_with_minus %>% filter(item_name %in% item_basket, date == max(date)) %>%
  select(date, item_name, "This Month's Change" = Wchange1a)
  
three_months_prior <- cpi_with_minus %>% filter(item_name %in% item_basket) %>% filter(date == max(date)) %>%
  select(item_name, "Prior Three Months" = Wthree_monthsA)

values_2021 <- cpi_with_minus %>% filter(item_name %in% item_basket) %>%
  filter(date == "2021-12-01") %>% select(item_name, "2021 Values" = Wchange12)

average_month_pre_pandemic <- cpi_with_minus %>% filter(item_name %in% item_basket) %>%
  group_by(item_name) %>% filter(date >= "2014-01-01", date <= "2019-12-01") %>%
  summarize("Average 2014-2019" = mean(Wchange1a))

# Create the chart and then clean it up
handoff_chart <- average_month_pre_pandemic %>% left_join(values_2021, by="item_name") %>%
  left_join(three_months_prior, by="item_name") %>% left_join(most_recent, by="item_name") %>%
  select(-date) %>%
  filter(item_name != "All items less food and energy") %>%
  filter(item_name != "Commodities less food and energy commodities") %>%
  filter(item_name != "Services less energy services")

handoff_chart$item_name <- str_replace_all(handoff_chart$item_name, "Transportation commodities less motor fuel", "Autos")
handoff_chart$item_name <- str_replace_all(handoff_chart$item_name, "Minus", "Ex")
handoff_chart <- handoff_chart %>% rename("Item" = item_name) %>% cbind(X = c(1,3,4,2)) %>% arrange(X) %>% select(-X)
###############################################################
# Code to evaluate the 'Handoff Theory' of inflation normalizing in 2021-2022.
# Mike Konczal
# Last updated 4/10/22

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/")
library(tidyverse)
library(lubridate)

##### SET UP SOME THINGS #####

#Either run Script 1 to download data fresh, or used locally stored data.
source(file = "1_load_cpi_data.R")
#load("data/cpi_data.RData")

#######


#SET UP DATA:
cpi_chart <- cpi_data %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
  mutate(Pchange3 = (value/lag(value,3)-1)) %>%
  mutate(Wchange3 = (Pchange3*weight)/100) %>%
  mutate(Wchange3a = (1 + Wchange3)^12 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100) %>%
  mutate(Pchange48 = (value/lag(value, 48)-1)) %>%
  mutate(Wchange48 = (Pchange48*weight)/100) %>%
  mutate(Wchange48a = (1 + Wchange48)^12 - 1) %>%
  ungroup()




cpi_handoff_chart_values <- cpi_chart %>% filter(item_name %in% c("Services less energy services", "Commodities less food and energy commodities")) %>%
  select()
  mutate(item_name = str_replace_all(item_name, "Commodities less food and energy commodities", "Core_goods")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core services` = `Services less energy services` - `Shelter`) %>%
  select(-`Services less energy services`) %>%
  pivot_longer(c(Shelter, `Rest of core services`,Core_goods), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = str_replace_all(item_name, "Core_goods","Core goods")) %>%
  mutate(item_name = factor(item_name, levels = c("Core goods", "Shelter", "Rest of core services")))



item_basket <- c("All items less food and energy", "Commodities less food and energy commodities", "Transportation commodities less motor fuel",
                 "Services less energy services", "Shelter", "Core Goods Minus Autos", "Core Services Minus Shelter")

# Calculate observations for Core Goods Minus Autos.

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
  filter(item_name %in% c("Commodities less food and energy commodities","Shelter")) %>%
  rbind(minus_shelter) %>% arrange(date)

# Start selecting the observations to look for
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




months2022 <- interval(ymd("2021-12-01"), max(cpi$date))
months2022 = months2022 %/% months(1)

average_month_pre_pandemic <- cpi %>%
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



cpi_handoff_chart_values <- cpi %>% filter(item_name %in% c("Services less energy services", "Shelter", "Commodities less food and energy commodities")) %>%
  mutate(item_name = str_replace_all(item_name, "Commodities less food and energy commodities","Core_goods")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core services` = `Services less energy services` - `Shelter`) %>%
  select(-`Services less energy services`) %>%
  pivot_longer(c(Shelter, `Rest of core services`,Core_goods), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = str_replace_all(item_name, "Core_goods","Core goods")) %>%
  mutate(item_name = factor(item_name, levels = c("Core goods", "Shelter", "Rest of core services")))




average_month_pre_pandemic <- cpi_handoff_chart_values %>%
  filter(date >= "2016-01-01" & date <= "2019-12-01") %>%
  group_by(item_name) %>%
  summarize(pre_value = cumprod(1+Wchange1a)) %>%
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
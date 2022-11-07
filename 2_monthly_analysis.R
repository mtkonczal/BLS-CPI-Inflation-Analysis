###############################################################
# Graphics that I'm watching, Summer 2022, inflation
# Mike Konczal
# Last updated 11/07/2022

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


#### function ####

##### Graphic1: Core Inflation ####

trend <- cpi %>% filter(date >= "2021-10-01" & date <= "2022-06-01") %>%
  filter(item_name == "All items less food and energy") %>%
  summarize(n = exp(mean(log(Wchange1a))))
trend <- as.numeric(trend)

trend2 <- cpi %>% filter(date >= "2021-10-01") %>%
  filter(item_name == "All items less food and energy") %>%
  summarize(n = exp(mean(log(Pchange1a))))
trend2 <- as.numeric(trend2)

MI_Graphic1 <- cpi %>% filter(date > "2020-12-01", item_name == "All items less food and energy") %>%
  select(date, item_name, Pchange1a) %>%
  mutate(num_label = round(100*Pchange1a, 2)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  mutate(trend = ifelse(date >= "2022-01-01", trend2, NA)) %>%
  ggplot(aes(x = date, y = Pchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity') + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "Monthly Percent Increase in Core Goods and Services, Annualized",
       subtitle = "Consistently high levels since last October.",
       caption ="BLS, CPI, 2022 Weights, Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  geom_line(aes(x=date, y=trend), linetype="dotted", size=1.2, color = "black", alpha=1) +
  annotate(geom="text", x=as.Date("2022-03-01"), y=trend2+0.004, label=scales::percent(trend2, accuracy=0.1), size=5, color="black") +
  scale_x_date(date_labels = "%b %Y", breaks = "2 month")

ggsave("graphics/g1_core.png", dpi="retina", width = 12, height=6.75, units = "in")
ggsave("graphics/g1_core2.png", width = 9.5, height=5.34, dpi="retina")

##### Graphic 2: Goods Versus Services ####

MI_Graphic2 <- cpi %>% filter(item_name %in% c("Services less energy services", "Commodities less food and energy commodities"), date > "2019-01-01") %>%
  mutate(num_label = round(100*Wchange1a, 2)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  mutate(in_range = ifelse(date >= "2021-10-01", trend, NA), trend = mean(in_range, na.rm=TRUE),
         trend = ifelse(date >= "2021-10-01", trend, NA))

MI_Graphic2$item_name <- str_replace_all(MI_Graphic2$item_name, "Services less energy services", "Core Services")
MI_Graphic2$item_name <- str_replace_all(MI_Graphic2$item_name, "Commodities less food and energy commodities", "Core Goods")

MI_Graphic2 <- MI_Graphic2 %>% ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity') + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "Core Inflation, Consistent Since October 2021, Has a Second Month Much Lower Than Average",
       subtitle = paste("Monthly Contribution to Inflation, Annualized. Dotted line reflects an average of ", round(trend,3)*100, "% from Oct 2021 to Jul 2022.", sep= ""),
       caption ="BLS, CPI, 2021 Weights, Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  geom_line(aes(x=date, y=trend), linetype=2, lineend="square", size=1, color = "black") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b %Y", breaks = "3 month")

ggsave("graphics/g2_services_goods.png", dpi="retina", width = 12, height=6.75, units = "in")

#### GRAPHIC 2: WITH LABELS ###


#### GRAPHIC 2: WITH LABELS ####

MI_Graphic2a <- cpi %>% filter(item_name %in% c("Services less energy services", "Commodities less food and energy commodities"), date > "2019-01-01") %>%
  mutate(in_range = ifelse(date >= "2021-10-01", trend, NA), trend = mean(in_range, na.rm=TRUE),
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

MI_Graphic2a %>% ggplot(aes(x = date, y = Wchange1a, fill = item_name)) +
  geom_bar(stat = 'identity') + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(y = NULL,
       x = NULL,
       title = "Driven All by Services This Month, Core Inflation Continues at the High Trend It's Been at Since October 2021",
       subtitle = paste("Monthly Contribution to Inflation, Annualized. Dotted line reflects an average of ", round(trend,3)*100, "% from October 2021 to June 2022.", sep= ""),
       caption ="BLS, CPI, 2021 Weights, Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  geom_line(aes(x=date, y=trend), linetype=2, lineend="square", size=1, color = "black") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b %Y", breaks = "3 month") +
  geom_text(aes(x=date, y=place_y, label=num_label), nudge_y = 0.003, color = "#6d0000", size=4)

ggsave("graphics/g2a_services_goods.png", dpi="retina", width = 12, height=6.75, units = "in")
###### Graphic 3: Services Breakdown #####

MI_Graphic3 <- cpi %>% filter(date > "2020-12-01", item_name %in% c("Services less energy services", "Shelter", "Airline fares")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core services` = `Services less energy services` - `Airline fares` - `Shelter`) %>%
  select(-`Services less energy services`) %>%
  pivot_longer(c(Shelter, `Airline fares`, `Rest of core services`), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("Airline fares", "Shelter", "Rest of core services"))) %>%
  mutate(num_label = round(100*Wchange1a, 2)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity') + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "Monthly Core Services Contribution to Inflation, Annualized",
       subtitle = "Services are up this month, driven largely but not entirely by housing.",
       caption ="BLS, CPI, 2022 Weights, Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_x_date(date_labels = "%b %Y", breaks = "2 month")

ggsave("graphics/g3_test_services.png", dpi="retina", width = 12, height=6.75, units = "in")

###### GRAPHIC 3a : Goods Breakdown ####

MI_Graphic3 <- cpi %>% filter(date > "2020-12-01", item_name %in% c("Commodities less food and energy commodities", "Transportation commodities less motor fuel")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core goods` = `Commodities less food and energy commodities` - `Transportation commodities less motor fuel`) %>%
  select(-`Commodities less food and energy commodities`) %>%
  rename(`All autos` = `Transportation commodities less motor fuel`) %>%
  pivot_longer(c(`All autos`, `Rest of core goods`), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("All autos", "Rest of core goods"))) %>%
  mutate(num_label = round(100*Wchange1a, 2)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity') + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "Monthly Core Goods Contribution to Inflation, Annualized",
       subtitle = "Goods are still elevated across the board, the drop in autos remains to be seen.",
       caption ="Autos is New and Used Cars and Motor Parts. BLS, CPI, 2022 Weights, Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_x_date(date_labels = "%b %Y", breaks = "2 month")

ggsave("graphics/g3_test_goods.png", dpi="retina", width = 12, height=6.75, units = "in")


##### GRAPHIC 4: LM #####
# Creating a supercore metric

date_LM = "2017-01-01"
item_basket_LM <- c("All items less food and energy", "Shelter", "Transportation commodities less motor fuel",
                    "Airline fares", "Admissions")
LM <- cpi %>% filter(item_name %in% item_basket_LM) %>%
  filter(date >= date_LM)

LM1 <- LM %>% filter(item_name == "All items less food and energy") %>% select(date, Wchange1, Wchange1a, weight)
LM2 <- LM %>% filter(item_name != "All items less food and energy") %>%
  group_by(date) %>% summarize(totalW1 = sum(Wchange1), totalWeight = sum(weight)) %>% ungroup() %>% select(date, totalW1, totalWeight) %>%
  left_join(LM1, by="date") %>% mutate(diff_change1 = Wchange1 - totalW1) %>% mutate(diff_change1a = (1+diff_change1/100)^12 - 1)


LM2 <- LM %>% filter(item_name != "All items less food and energy") %>%
  group_by(date) %>% summarize(totalW1 = sum(Wchange1a), totalWeight = sum(weight)) %>% ungroup() %>% select(date, totalW1, totalWeight) %>%
  left_join(LM1, by="date") %>% mutate(diff_change1a = Wchange1a - totalW1)


trends <- LM2 %>% filter(date < "2020-01-01" | date > "2022-01-01") %>% mutate(before = date < "2020-01-01") %>%
  group_by(before) %>% summarize(value = mean(diff_change1a))

LM2 <- LM2 %>% mutate(trend = ifelse(date < "2020-01-01", trends$value[trends$before==TRUE], NA)) %>%
  mutate(trend = ifelse(date > "2021-12-01", trends$value[trends$before==FALSE], trend))

st <- paste("This basket, constituting", round(mean(LM2$totalWeight),0), "percent of spending, contributed", round(100*trends$value[trends$before==TRUE],1), "percent to inflation, annualized, in 2017-2019. So far in 2022 it has contributed", round(100*trends$value[trends$before==FALSE],1), "percent to inflation.")

MI_Graphic4 <- ggplot(LM2, aes(date, diff_change1a)) + geom_bar(stat="identity", fill="coral2") + theme_classic() +
  geom_line(aes(x=date, y=trend), linetype="dashed", size=1.2, color = "black", alpha=1) +
  labs(title="CPI Monthly Inflation, Annualized, Excluding Food, Energy, Shelter/Housing/Rent, New/Used Cars and Motor Parts, Airfares, and Admissions",
       subtitle = st,
       x="", y="",
       caption ="BLS, CPI, 2022 Weights, Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b %Y", breaks = "4 month") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot")


ggsave("graphics/g4_exclusive_core.png", dpi="retina", width = 12, height=6.75, units = "in")


##### YEARLY CODE FOR ANALYSIS ####

##### Graphic 2: Transitory #####

m_g2 <- c("Transportation commodities less motor fuel",
          "Airline fares", "Admissions", "Other lodging away from home including hotels and motels", "Car and truck rental")
# CHANGE BELOW
m_g2d <- "2019-01-01"
transitory_inflation <- cpi %>% filter(date > m_g2d, item_name %in% m_g2) %>%
  select(date, item_name, Wchange1a) %>%
  group_by(date) %>%
  summarize(`transitory_inflation` = sum(Wchange1a))

cpi %>% filter(date > m_g2d, item_name == "All items less food and energy") %>%
  select(date, item_name, Wchange1a) %>%
  left_join(transitory_inflation, by="date") %>%
  mutate(rest_inflation = Wchange1a - transitory_inflation) %>%
  select(-item_name, -Wchange1a) %>%
  pivot_longer(c(rest_inflation, transitory_inflation), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("transitory_inflation", "rest_inflation"))) %>%
  mutate(num_label = round(100*Wchange1a, 2)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity') + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "Monthly Core Services Contribution to Inflation, Annualized",
       subtitle = "High levels even as it fluctuates between shelter and ex shelter monthly.",
       caption ="BLS, CPI, 2022 Weights, Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent)

ggsave("graphics/MI_GraphicT.png", dpi="retina", width = 12, height=6.75, units = "in")

cpi %>% filter(item_name %in% c("Services less energy services", "Commodities less food and energy commodities"), date > "2019-01-01") %>%
  mutate(num_value = ifelse(Wchange1a > 0, Wchange1a, 0)) %>%
  group_by(date) %>%
  mutate(num_label = sum(round(100*num_value, 2))) %>% mutate(num_label = num_label/100) %>%
  ungroup() %>%
  mutate(in_range = ifelse(date > "2021-10-01", trend, NA), trend = mean(in_range, na.rm=TRUE),
         trend = ifelse(date > "2021-10-01", trend, NA)) %>%
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label=scales::percent (num_label,accuracy=0.1))) +
  geom_bar(stat = 'identity') + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "Core Inflation, Consistent Since October 2021, Slows Last Month",
       subtitle = paste("Monthly, Annualized. Dotted line reflects an average of ", round(trend,3)*100, "% in 2022.", sep= ""),
       caption ="BLS, CPI, 2022 Weights, Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  geom_line(aes(x=date, y=trend), linetype=2, lineend="square", size=1, color = "black") +
  geom_text(aes(x=date, y=num_label+0.0018), size=3) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b %Y", breaks = "3 month")

ggsave("graphics/g2_services_goodsTT.png", dpi="retina", width = 12, height=6.75, units = "in")


#### Core versus Food/Energy

FoodEnergy <- cpi %>% filter(item_name %in% c("Energy", "Food")) %>% group_by(date) %>%
  mutate(Wchange1a = sum(Wchange1a)) %>% ungroup() %>%
  filter(item_name == "Energy") %>%
  mutate(item_name = "Food and Energy")

cpi %>% filter(item_name == "All items less food and energy") %>%
  mutate(item_name = "Inflation, Core") %>%
  rbind(FoodEnergy) %>% filter(date > "2019-01-01") %>%
  ggplot(aes(x = date, y = Wchange1a, fill = item_name)) +
  geom_bar(stat = 'identity') + theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "Core Inflation, Consistent Since October 2021, Slows Last Month",
       subtitle = paste("Monthly, Annualized. Dotted line reflects an average of ", round(trend,3)*100, "% in 2022.", sep= ""),
       caption ="BLS, CPI, 2022 Weights, Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b %Y", breaks = "3 month")


##### 3 and 6 month core ####
core <- cpi %>% filter(item_name == "All items less food and energy") %>%
  mutate(change = value - lag(value,1), change = change/lag(value,1)) %>%
  mutate(ThreeMonth = change + lag(change,1) + lag(change,2)) %>%
  mutate(SixMonth = ThreeMonth + lag(change,3) + lag(change,4) + lag(change,5)) %>%
  mutate(ThreeMonth = ThreeMonth/3, SixMonth = SixMonth/6)

tester <- core %>% filter(year == 2022) %>%
  summarize(avg = mean(change))
tester <- as.numeric(tester)

core %>% filter(date > "2020-12-01") %>%
  ggplot(aes(date, ThreeMonth)) + geom_line(size=1.2, color="#0072B2") + theme_classic() +
  geom_hline(yintercept = tester, color="#D55E00", size=0.9, linetype="dashed") +
  geom_line(aes(date, SixMonth), color="#56B4E9", size=1, linetype="dashed") +
  annotate(
    "text", label = "Average 2022",
    x=as.Date("2021-02-01"), y = 0.00555, size = 5, colour = "#D55E00") +
  annotate(
    "text", label = "3-Month Average",
    x=as.Date("2021-03-15"), y = 0.0075, size = 5, colour = "#0072B2") +
  annotate(
    "text", label = "6-Month Average",
    x=as.Date("2021-09-15"), y = 0.00555, size = 5, colour = "#56B4E9") +
  labs(x="", y="",
       title="Remarkable Convergence Across 3-, 6-, and 9- Month Core Inflation",
       subtitle = "Core inflation, monthly change. 3- and 6- month inflation have been similar to 2022 value recently",
       caption = "All items less food and energy, monthly percent change, BLS, Author's calculations. Mike Konczal, Roosevelt Institute") +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20, face="bold"),
        plot.subtitle = element_text(size=15),
        plot.caption = element_text(size=10),
        plot.title.position = "plot",
        axis.text=element_text(size=18))

ggsave("graphics/Three_Six_month_average.png", width = 9.5, height=5.34, dpi="retina")


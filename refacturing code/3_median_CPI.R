###############################################################
# Graphics that I'm watching, Summer 2022, inflation
# Mike Konczal
# Last updated 11/07/2022

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/")
library(ggridges)
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


#### PART ONE - A MESS OF SEASONAL ADJUSTMENTS ####
median <- read_csv("weights/mediancpi_component_table.csv") %>% mutate(item_name = Component)
weights <- read_csv("weights/inflation_weights.csv")

# Check me LEFT OFF TKTKTKTKT - FIX LAST ON THE MEDIAN SIDE
median$item_name[!(median$item_name %in% cpi$item_name)]
median$item_name[!(median$item_name %in% weights$item_name)]

weights$item_name[!(weights$item_name %in% cpi_data$item_name)]

weight <- c(4.3,8.7,4.6,6.7)
series_title <- c("Owners' equivalent rent of residences in Midwest urban, all urban consumers, not seasonally adjusted",
                  "Owners' equivalent rent of residences in South urban, all urban consumers, not seasonally adjusted",
                  "Owners' equivalent rent of residences in Northeast urban, all urban consumers, not seasonally adjusted",
                  "Owners' equivalent rent of residences in West urban, all urban consumers, not seasonally adjusted")
median_housing <- as_tibble(cbind(series_title,weight))
median_housing$weight <- as.numeric(median_housing$weight)
rm(weight, series_title)

cpi_housing <- cpi_data %>%
  filter(series_title %in% median_housing$series_title, periodicity_code ==) %>%
  select(-weight) %>%
  left_join(median_housing, by="series_title") %>%
  mutate(item_name = series_title) %>%
  mutate(item_name = str_remove_all(item_name, " urban, all urban consumers, not seasonally adjusted")) %>%
  select(item_name, date, value, weight)

NSA_items <- c("Motor vehicle fees","Tenants' and household insurance", "Personal care products","Personal care services")

cpi_NSA_rest <- cpi_data %>%
  filter(item_name %in% NSA_items) %>%
  select(item_name, date, value, weight)

median <- cpi %>%
  filter(item_name %in% median$item_name) %>%
  select(item_name, date, value, weight) %>%
  rbind(cpi_housing) %>%
  rbind(cpi_NSA_rest) %>%
  filter(!is.na(date)) %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Pchange1a = (1 + Pchange1)^12 - 1) %>%
  ungroup() %>%
  
  group_by(date) %>%
  mutate(normalized = sum(weight)) %>%
  mutate(weightN = weight/normalized) %>%
  arrange(Pchange1a) %>%
  mutate(cumsum = cumsum(weight)/100) %>%
  mutate(cumsumN = cumsum(weightN)) %>%
  mutate(p20 = Pchange1a[detect_index(cumsumN, function(x) x > 0.2)]) %>%
  mutate(p30 = Pchange1a[detect_index(cumsumN, function(x) x > 0.3)]) %>%
  mutate(p40 = Pchange1a[detect_index(cumsumN, function(x) x > 0.4)]) %>%
  mutate(p50 = Pchange1a[detect_index(cumsumN, function(x) x > 0.5)]) %>%
  mutate(p60 = Pchange1a[detect_index(cumsumN, function(x) x > 0.6)]) %>%
  mutate(p70 = Pchange1a[detect_index(cumsumN, function(x) x > 0.7)]) %>%
  mutate(p80 = Pchange1a[detect_index(cumsumN, function(x) x > 0.8)]) %>%
  ungroup()

median %>% arrange(date) %>% filter(date != lag(date)) %>%
  filter(date > "2011-01-01") %>%
  pivot_longer(p40:p60, names_to="p_name", values_to="p_value") %>%
  ggplot(aes(date,p_value, color=p_name)) + geom_line() + theme_classic() +
  theme(legend.position = "bottom")

s_trends <- median %>% group_by(date) %>% arrange(Pchange1a) %>%
  group_by(date) %>%
  mutate(term_80 = item_name[detect_index(cumsumN, function(x) x > 0.8)]) %>%
  mutate(term_20 = item_name[detect_index(cumsumN, function(x) x > 0.2)]) %>%
  ungroup() %>%
  select(date, term_20, p20, term_80, p80) %>%
  arrange(date) %>% filter(date != lag(date))
    

######### PART TWO THIS BELOW TAKES ONLY SEASONALLY ADJUSTED VALUES, NO GRANUAARITY ON OER THOUGH #######

median_terms <- read_csv("weights/mediancpi_component_table.csv") %>% mutate(item_name = Component)
weights <- read_csv("weights/inflation_weights.csv")

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
  mutate(p20 = Pchange1a[detect_index(cumsumN, function(x) x > 0.2)]) %>%
  mutate(p30 = Pchange1a[detect_index(cumsumN, function(x) x > 0.3)]) %>%
  mutate(p40 = Pchange1a[detect_index(cumsumN, function(x) x > 0.4)]) %>%
  mutate(p50 = Pchange1a[detect_index(cumsumN, function(x) x > 0.5)]) %>%
  mutate(p60 = Pchange1a[detect_index(cumsumN, function(x) x > 0.6)]) %>%
  mutate(p70 = Pchange1a[detect_index(cumsumN, function(x) x > 0.7)]) %>%
  mutate(p80 = Pchange1a[detect_index(cumsumN, function(x) x > 0.8)]) %>%
  ungroup()


median %>% arrange(date) %>% filter(date != lag(date)) %>%
  filter(date > "2011-01-01") %>%
  filter(month(date) %in% c(11,5)) %>%
  pivot_longer(p40:p60, names_to="p_name", values_to="p_value") %>%
  mutate(p_valueA = (1+p_value)^4-1) %>%
  ggplot(aes(date ,p_valueA, color=p_name)) + geom_point() + theme_classic() + geom_line() +
  theme(legend.position = "bottom")

median %>% group_by(date) %>% arrange(Pchange1a) %>%
  group_by(date) %>%
  mutate(term_80 = item_name[detect_index(cumsumN, function(x) x > 0.84)]) %>%
  mutate(term_20 = item_name[detect_index(cumsumN, function(x) x > 0.16)]) %>%
  ungroup() %>%
  select(date, term_20, p20, term_80, p80) %>%
  arrange(date) %>% filter(date != lag(date))

#THIS IS THE GRAPHIC - 30 percent-trimmed distribution
median %>% mutate(dateF = as.factor(date)) %>%
  filter(cumsumN <= 0.85 & cumsum >= 0.15) %>%
  filter(cumsumN <= 0.92 & cumsum >= 0.08) %>%
  mutate(Pchange3a = (1+Pchange3)^4-1) %>%
  filter(date >= "2018-01-01") %>%
  filter(date != "2020-05-01") %>%
  filter(month(date) %in% c(11,8,5,2)) %>%
  mutate(monthC = format(date, "%B, %Y")) %>%
  mutate(monthC = fct_reorder(monthC,date)) %>%
  mutate(monthCR = fct_rev(monthC)) %>%
  ggplot(aes(x = Pchange3a, y = monthCR, fill = stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis(option = "H") +
  theme_ridges() + 
  theme(legend.position = "none") +
  scale_x_continuous(labels = percent) +
  labs(title="Distribution of CPI Price Increases Moved Out, Now Returning Back",
       subtitle="Distribution of the Cleveland Fed's Median/Trimmed-Mean CPI price basket, but 3-month change annualized, with components\nwhose expenditure weights fall above/below the 85/15th percentile of price changes removed.",
       x="Three Month Percent Change", y="", caption="For now, OER is treated as one value, instead of broken out by region and seasonally adjusted as Cleveland Fed does it. Mike Konczal, Roosevelt Institute") +
  theme(plot.title.position = "plot", legend.position = "none", legend.title = element_blank(),
  plot.title = element_text(size = 25),
  plot.subtitle = element_text(size=15),
  plot.caption = element_text(size=10, face="italic"),
  axis.text.y = element_text(size=12),
  axis.text.x = element_text(size=12)) + theme_lass

ggsave("graphics/trimmed_dist.png", dpi="retina", width = 12, height=6.75, units = "in", bg = "white")

median %>% mutate(dateF = as.factor(date)) %>%
  filter(cumsumN < 0.80 & cumsum > 0.20) %>%
  filter(date >= "2018-01-01") %>%
  filter(month(date) %in% c(11,8,5,2)) %>%
  mutate(monthC = format(date, "%B, %Y"))  %>%
  select(monthC) %>%
  mutate(monthCF = as.factor(monthC)) %>%
  mutate(date2 = my(monthCF)) %>%
  arrange(date2) %>%
  mutate(date3 = fct_reorder(monthCF,date2))
#####################


####### HERE WE ARE
######## THIS NEEDS A SEASONAL ADJUSTMENT
######## AND FOR THE NORMALIZATION OF WEIGHTS
### GET HOUSING

median_terms <- read_csv("weights/mediancpi_component_table.csv") %>% mutate(item_name = Component)
weights <- read_csv("weights/inflation_weights.csv")
weight <- c(4.3,8.7,4.6,6.7)
series_title <- c("Owners' equivalent rent of residences in Midwest urban, all urban consumers, not seasonally adjusted",
                  "Owners' equivalent rent of residences in South urban, all urban consumers, not seasonally adjusted",
                  "Owners' equivalent rent of residences in Northeast urban, all urban consumers, not seasonally adjusted",
                  "Owners' equivalent rent of residences in West urban, all urban consumers, not seasonally adjusted")
median_housing <- as_tibble(cbind(series_title,weight))
median_housing$weight <- as.numeric(median_housing$weight)
rm(weight, series_title)

cpi_housing <- cpi_data %>%
  filter(series_title %in% median_housing$series_title, periodicity_code =="R", !is.na(date)) %>%
  select(-weight) %>%
  left_join(median_housing, by="series_title") %>%
  mutate(item_name = series_title) %>%
  mutate(item_name = str_remove_all(item_name, " urban, all urban consumers, not seasonally adjusted")) %>%
  select(item_name, date, value, weight)


#######
#SET UP DATA:
median <- cpi_data %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  filter(item_name %in% median_terms$item_name) %>%
  select(item_name, date, value, weight) %>%
  rbind(cpi_housing) %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Pchange1a = (1 + Pchange1)^12 - 1) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
  mutate(Pchange3 = (value/lag(value, 3)-1)) %>%
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
  filter(cumsumN <= 0.80 & cumsumN >= 0.20) %>%
  filter(date >= "2018-06-01") %>%
  filter(date != "2020-06-01") %>%
  filter(month(date) %in% c(12,9,6,3)) %>%
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


median <- median %>% arrange(date, cumsumN)
write_csv(median, "inflation_distribution_3_month_v2.csv")

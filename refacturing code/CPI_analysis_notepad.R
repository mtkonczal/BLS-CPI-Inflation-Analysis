# This is a place to work on various analysis that don't fit into a specific project.
# Data here will come and go, like a notepad.
# Written by: Mike Konczal
# Last Updated: 3-12-2022

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/")
library(hrbrthemes)
library(janitor)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(huxtable)
library(scales)
library(tidytext)
library(lubridate)
library(x12)
library(ggridges)
##### SET UP SOME THINGS #####
source(file = "1_load_cpi_data.R")

#load("data/cpi_data.RData")

##### REPLICATION CODE ### STARTING HERE #### SPECIAL GRAPHIC
#######
#SET UP DATA:
cpi <- cpi_data %>%
  mutate(weight_nhs = weight) %>%
  mutate(weight_nhs = if_else(date >= "2023-07-01" & item_name == "Used cars and trucks", 2.766, weight_nhs)) %>%
  mutate(weight_nhs = if_else(date >= "2023-07-01" & item_name == "Commodities less food and energy commodities", 21.208, weight_nhs)) %>%
  mutate(weight = weight_nhs) %>%
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

### Other setup
cpi <- cpi_data %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  filter(area_code == "0000") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Pchange1a = (1 + Pchange1)^12 - 1) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
  mutate(AddedW3 = Wchange1 + lag(Wchange1,1) + lag(Wchange1,2)) %>%
  mutate(AddedW12 = AddedW3 + lag(Wchange1,3) + lag(Wchange1,4) + lag(Wchange1,5)) %>%
  mutate(AddedW12 = AddedW12 + lag(Wchange1,6) + lag(Wchange1,7) + lag(Wchange1,8)) %>%
  mutate(AddedW12 = AddedW12 + lag(Wchange1,9) + lag(Wchange1,10) + lag(Wchange1,11)) %>%
  mutate(Pchange3 = (value/lag(value, 3)-1)) %>%
  mutate(Pchange3a = (1 + Pchange3)^12 - 1) %>%
  mutate(Wchange3 = (Pchange3*weight)/100) %>%
  mutate(Wchange3a = (1 + Wchange3)^4 - 1) %>%
  mutate(Pchange6 = (value/lag(value, 6)-1)) %>%
  mutate(Wchange6 = (Pchange3*weight)/100) %>%
  mutate(Wchange6a = (1 + Wchange3)^2 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100) %>%
  mutate(Wchange1 = Wchange1*100) %>%
  ungroup()


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


item_used <- c("Used cars and trucks")
cpi %>% filter(item_name %in% item_used) %>% filter(year > 2020) %>% select(date, item_name, Wchange3a, Wchange1a)

item_test <- c("Commodities less food and energy commodities")
cpi %>% filter(item_name %in% item_test) %>% arrange(date) %>% filter(year == 2021) %>% summarize((Wchange12))

tester <- cpi %>% filter(item_name == "Energy")
item_basket <- item_basket_watch_categories

item_basket <- c("Food", "Energy", "All items less food and energy", "Goods", "Services", "Gasoline", "Housing", "Used cars and trucks", "New vehicles", "All items")
item_basket <- c("All items", "Energy", "Food", "Commodities less food and energy commodities", "Services less energy services", "All items less food and energy")

item_basket <- item_basket_core_goods
item_basket <- item_basket_topline
item_basket <- item_basket_core_services

gcpi <- cpi %>% filter(item_name %in% item_basket) %>% filter(date == max(date)) %>% mutate(Wchange3a = Wchange3a*100, Wchange12 = Wchange12*100)
gcpi <- gcpi %>% ungroup() %>% mutate(item_name = shorten_item_names(item_name)) %>% arrange(Wchange3a) %>% mutate(g2_item_name=factor(item_name, item_name))

gcpi <- gcpi %>%  mutate(Wchange3aL = ((abs(Wchange3a-Wchange12))>0.1) ) %>% mutate(Wchange12aL = Wchange3aL)
gcpi$Wchange3aL <- na_if(gcpi$Wchange3aL, 0)
gcpi$Wchange12aL <- na_if(gcpi$Wchange12aL, 0)

gcpi <-gcpi %>% mutate(Name3Label = NA)
gcpi <-gcpi %>% mutate(Name12Label = NA)
gcpi$Name3Label[gcpi$item_name=="Services less energy services"] <- "3-Month\nChange,\nAnnualized"
gcpi$Name12Label[gcpi$item_name=="Services less energy services"] <- "12-Month\nChange,\nAnnualized"



MaxMonth <- format(gcpi$date[1], "%B")
MaxYear <- format(gcpi$date[1], "%Y")
Sub_Title <- paste("Contributions to Overall Inflation, <span style = 'color:#6d0000;'>Last Three Months</span> and <span style = 'color:#b87e5e;'>Twelve Months</span> from ", MaxMonth, ", ", MaxYear, ", Annualized", sep="")
Full_Title <- "Inflation in Core Services Increases Across Most Categories"
font <- "Helvetica"

ggplot(gcpi) +
  geom_segment( aes(x=g2_item_name, xend=g2_item_name, y=Wchange3a, yend=Wchange12), color="grey") +
  geom_point( aes(x=g2_item_name, y=Wchange3a), color="#6d0000", size=3) +
  geom_point( aes(x=g2_item_name, y=Wchange12), color="#b87e5e", size=3) +
  coord_flip()+
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_textbox(color = "#1002a4",  family=font, size = 28.5,  hjust = 0.5, padding = margin(18, 14, 18, 14)),
        plot.subtitle =  element_markdown(size = 19, hjust = 0.5)) +
  ggtitle(Full_Title) +
  theme(
    plot.title.position = "plot",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 14, hjust = 0.5, color="#1002a4"),
    plot.caption = element_text(size = 12, lineheight = 0.7),
    panel.grid = element_blank()) +
  xlab("") +
  ylab("") +
  #    geom_hline(yintercept=0, linetype="solid", color = "black", alpha=0.5) +
  labs(subtitle = Sub_Title,
       caption ="BLS, CPI, Seasonally Adjusted. Author's Calculation. 2021 weights. Contributions may not add up to totals due to weights, seasonal adjustments and rounding. @rortybomb",
       x="", y="") +
  geom_text(aes(x=g2_item_name, y=Wchange3a, label=round(Wchange3aL*Wchange3a, 2)), nudge_x =.2, color = "#6d0000", size=5) +
  geom_text(aes(x=g2_item_name, y=Wchange12, label=(round(Wchange12aL*Wchange12, 2))), nudge_x =.2, color = "#b87e5e", size=5) +
  geom_text(aes(x=g2_item_name, y=Wchange3a, label=Name3Label), color = "#6d0000", nudge_x=-0.4, size=4) +
  geom_text(aes(x=g2_item_name, y=Wchange12, label=Name12Label), color = "#b87e5e", nudge_x=-0.4, size=4)          

#####

item_basket_LM <- c("All items less food and energy", "Shelter", "Transportation commodities less motor fuel", "Airline fares")
LM <- cpi %>% filter(item_name %in% item_basket_LM) %>%
  filter(date >= "2014-01-01")

LM1 <- LM %>% filter(item_name == "All items less food and energy")
LM2 <- LM %>% filter(item_name != "All items less food and energy")

LM2 <- LM2 %>% group_by(date) %>% mutate(totalW1 = sum(Wchange1)) %>% filter(item_name == "Shelter") %>% ungroup() %>% select(date, totalW1)
#LM2 %>% filter(date <= "2020-01-01") %>% summarize(mean(v))

LM2$v <- LM1$Wchange1
LM2$v <- LM2$v - LM2$totalW1
LM2$v <- (1+LM2$v/100)^12 - 1
LM2$v <- LM2$v*100
ggplot(LM2, aes(date, v)) + geom_line(color="darkred") + theme_minimal() +
  labs(title="Inflation: Core Minus Shelter and Autos",
       x="", y="")


item_basket_core_goods

item_basket_AFU2 <- c("Commodities less food and energy commodities", "Transportation commodities less motor fuel")
AFU2 <- cpi %>% filter(item_name %in% item_basket_AFU2) %>%
  filter(date >= "2020-10-01") %>% mutate(Wchange1a = Wchange1a*100)
ggplot(AFU2, aes(date,Wchange1a,color=item_name)) + geom_line(size=2) + theme_minimal() + theme(legend.position='none') 


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
       title = "Core Inflation, Consistent Since October 2021, Has a Large Upside This Month",
       subtitle = paste("Monthly Contribution to Inflation, Annualized. Dotted line reflects an average of ", round(trend,3)*100, "% from Oct 2021 to Jul 2022.", sep= ""),
       caption ="BLS, CPI, 2021 Weights, Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  geom_line(aes(x=date, y=trend), linetype=2, lineend="square", size=1, color = "black") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b %Y", breaks = "3 month") +
  geom_text(aes(x=date, y=place_y, label=num_label), nudge_y = 0.003, color = "#6d0000", size=4)


###### GRAPHIC 4 - HANDOFF TO SERVICES #####

item_basket_AFU <- c("Services less energy services", "Shelter", "Airline fares")
AFU <- cpi %>% filter(item_name %in% item_basket_AFU) %>%
  filter(date >= "2011-01-01")
AFU <- create_basket(cpi, item_basket_AFU, startDate = "2014-01-01", lagM = 1, annualize = TRUE) %>% mutate(values = Services_less_energy_services - Shelter - Airline_fares)

AFU %>% filter(date <= "2019-12-01") %>% summarize(mean(values, na.rm = TRUE), sd(values, na.rm=TRUE))
AFU %>% filter(date >= "2021-01-01") %>% summarize(mean(values), sd(values))

ggplot(AFU, aes(x=date, y=values)) +
  geom_line(size=1.25, color="darkred") + bbc_style() +
  labs(subtitle = "Inflation Rate for Core Services Ex Shelter Ex Airline Fares, Annualized",
       x="", y="", caption = "Values are percent, annualized. Data is BLS, CPI, seasonally adjusted. Author's calculation. @rortybomb\n
       2022 weights throughout. Line is weighted services less energy services minus weighted shelter minus weighted airline fares.") +
  geom_hline(yintercept=0, linetype="solid", color = "black", alpha=.8) +
  theme(plot.title = element_textbox(size=22), plot.title.position = "plot") +
  scale_y_continuous(labels = scales::percent)
#  geom_hline(yintercept=0.00576, linetype="dashed", color = "darkred", alpha=.8) +
#  geom_hline(yintercept=0.00903, linetype="dashed", color = "darkred", alpha=.8)
#  annotate(geom="text", x=as.Date("2016-12-01"), y=-1, label="Core Goods", size=8, color="steelblue") +
#  annotate(geom="text", x=as.Date("2016-12-01"), y=2.8, label="Core Services", size=8, color="darkred") +
#  annotate(geom="text", x=as.Date("2018-11-01"), y=2.8, label="CPI Inflation Target", size=4, color="grey45")
#ggsave("graphics/liftoff4.pdf")

AFU_chart <- AFU %>% select(date, values)

tail(A)
most_recent_AFU <- AFU_chart %>% filter(date == max(date)) %>%
  select(date, "This Month's Change" = values)

two_months_prior <- AFU_chart %>% mutate(Wtwo_monthsA = ) %>% filter(date == max(date)) %>%
  select(item_name, "Prior Two Months" = Wtwo_monthsA)

values_2021 <- cpi_with_minus %>% filter(item_name %in% item_basket) %>%
  filter(date == "2021-12-01") %>% select(item_name, "2021 Values" = Wchange12)

average_month_pre_pandemic <- cpi_with_minus %>% filter(item_name %in% item_basket) %>%
  group_by(item_name) %>% filter(date >= "2014-01-01", date <= "2019-12-01") %>%
  summarize("Average Monthly 2014-2019" = mean(Wchange1a))

handoff_chart <- average_month_pre_pandemic %>% left_join(values_2021, by="item_name") %>%
  left_join(two_months_prior, by="item_name") %>% left_join(most_recent, by="item_name") %>%
  select(-date)


#######

basket_broadening <- c("All items less food, shelter, energy, and used cars and trucks")
broadening <- cpi %>% filter(item_name %in% basket_broadening) %>%
  filter(date >= "2015-01-01") %>% mutate(lastValue = Wchange1a*(date==max(date)))
broadening$lastValue <- na_if(broadening$lastValue, 0)

ggplot(broadening, aes(x=date, y=Wchange1a)) + geom_line() + theme_classic()  + 
  labs(title="Inflation Rate, All Items Less Food, Shelter, Energy, and Used Cars and Trucks",
       caption="Values are percent, annualized. Data is BLS, CPI, seasonally adjusted. 2022 weights used throughout. Author's calculation. @rortybomb") +
  geom_point(aes(x=max(date), y=lastValue), size=3) +
  geom_label_repel(data=broadening, aes(x=max(date), y=lastValue, label=paste(100*round(lastValue,2),"%", sep="")), size=5, box.padding = unit(0.2,"in")) +
  scale_y_continuous(labels = scales::percent) + scale_x_date(date_labels = "%Y", date_breaks = "1 year")

##### BECOMING A BOXPLOT GUY
boxplot_cpi <- cpi %>% filter(date==max(date) | date=="2022-02-01" | date=="2021-06-01") %>% filter(display_level > 1) %>%
  select(series_id, item_name, Wchange1a, Pchange1, display_level, date)
boxplot_cpi$is_date <- format(boxplot_cpi$date, "%B %Y")
boxplot_cpi$is_date2 <- factor(boxplot_cpi$is_date , levels=c("March 2022","February 2022","June 2021"))
ggplot(boxplot_cpi, aes(x=is_date2, y=Pchange1)) + geom_boxplot(fill="skyblue") + theme_classic() + coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot", axis.text.y = element_text(size=15), plot.title = element_text(size=28), plot.subtitle = element_text(size=15), plot.caption = element_text(size=10)) +
  labs(x="", y="", subtitle="Though Price Increases Have Broadened Since Last Year, March's Increase Did Not, It Was Instead Driven by a Few Big Outliers",
       title="Boxplot of 233 CPI Price Increases", caption="CPI, Not annualized, not inflation weight-adjusted, seasonally adjusted. Prices with display level of 2 or greater, so much overlap in categories. @rortybomb")
#ggsave("boxplot1.png", dpi="retina")

source("../BLS-CPS-Jobs-Numbers/1_b_load_bls_ces_jobs_data.R")
ces_bp <- ces_data %>% filter(series_id == "CES0000000001") %>% mutate(job_growth = 1000*(value - lag(value,1))) %>%
  #mutate(Great_Recession = date > "2009-12-01" & date < "2016-01-01") %>% mutate(covid_recovery = date >= "2021-01-01") %>%
  filter(date > "2009-12-01", date < "2015-01-01" | date >="2021-01-01") %>% mutate(is_pandemic = (date > "2020-12-01"))

ces_bp$is_pandemicT <- as.character(ces_bp$is_pandemic)
ces_bp$is_pandemicT <- str_replace(ces_bp$is_pandemicT, "FALSE", "Great Recession,\n2010-2014")
ces_bp$is_pandemicT <- str_replace(ces_bp$is_pandemicT, "TRUE", "COVID Recovery,\nJan 2021-Now")
ces_bp$is_pandemicT <- factor(ces_bp$is_pandemicT, levels=c("Great Recession,\n2010-2014","COVID Recovery,\nJan 2021-Now"))

ggplot(ces_bp, aes(x=is_pandemicT, job_growth)) + geom_boxplot(fill="skyblue") + theme_classic() +
  theme(plot.title.position = "plot", axis.text.y = element_text(size=15), axis.text.x = element_text(size=15), plot.title = element_text(size=28),
        plot.subtitle = element_text(size=15), plot.caption = element_text(size=10)) +
  labs(x="", y="", subtitle="The Largest Jobs Number Outlier During the Great Recession's Recovery is Lower Than the Median Job Number Since Jan 2021",
       title="Boxplot of Monthly CES Job Number Increases", caption="CPS, CES, seasonally adjusted. @rortybomb") +
  scale_y_continuous(labels = comma)

  #  scale_y_continuous(labels = scales::comma()) +
ces_bp %>% filter(is_pandemic) %>% summarize(median(job_growth))
ces_bp %>% filter(!is_pandemic) %>% summarize(max(job_growth))

tail(ces_bp$job_growth)

##### ARIZONA ICE TEA POST ####


tester <- cpi_data %>% filter(item_name == "Other beverage materials including tea") %>%
  mutate(item_name = "Nonalcoholic Beverages and Beverage Materials, Other Beverage Materials Including Tea, Excluding Juices and Coffee") %>%
  filter(period != "M13", seasonal == "S") %>%
  mutate(value = 100*value/113.500) %>%
  select(date, item_name, value)

tester2 <- tester %>% mutate(value=100, item_name="AriZona Iced Tea")

tester3 <- rbind(tester, tester2)

ggplot(tester3, aes(x=date, y=value, color=item_name)) + geom_line(size=2) +
  theme(legend.position = "bottom", legend.text = element_text(size=13), axis.text = element_text(size=15), legend.title = element_blank(), plot.subtitle = element_text(size=30), plot.title.position = "plot") +
  labs(x="", y="", subtitle="Inflation for AriZona Iced Tea versus Competitors, 2003=100", caption="BLS, CPI, seasonally adjusted, @rortybomb") +
  scale_y_continuous(limits = c(80,130)) + theme_ipsum()

########## SERVICES EX SHELTER EX AIRLINE FARES
item_basket_airlines0 <- c("Services less energy services")
tester0 <- cpi %>% filter(item_name %in% item_basket_airlines0) %>%
  select(item_name, Wchange1a, weight, date) %>% mutate(Wchange1a = Wchange1a*100) %>% filter(date == max(date))

item_basket_airlines1 <- c("Shelter", "Airline fares")
tester1 <- cpi %>% filter(item_name %in% item_basket_airlines1) %>%
  select(item_name, Wchange1a, weight, date) %>% mutate(Wchange1a = Wchange1a*100) %>% filter(date == max(date))

# Instead of subtracting let's build it up
item_basket_airlines2 <- c("Car and truck rental", "Motor vehicle maintenance and repair", "Motor vehicle insurance", "Motor vehicle fees", "Other intercity transportation",
                           "Intracity transportation", "Recreation services", "Education and communication services", "Other personal services",
                           "Medical care services", "Water and sewer and trash collection services", "Household operations")
tester2 <- cpi %>% filter(item_name %in% item_basket_airlines2) %>%
  select(item_name, Wchange1a, weight, date) %>% mutate(Wchange1a = Wchange1a*100) %>% filter(date == max(date))

tester0$weight
sum(tester1$weight) + sum(tester2$weight)

tester0$Wchange1a
sum(tester1$Wchange1a) + sum(tester2$Wchange1a)


# Breadth of food price increases

food_index <- c("Rice, pasta, cornmeal","Breakfast cereal", "Flour and prepared flour mixes",
"Bread", "Fresh biscuits, rolls, muffins", "Cakes, cupcakes, and cookie",
"Beef and veal", "Pork", "Other meats", "Poultry", "Fish and seafood", "Eggs",
"Milk", "Fresh fruits", "Fresh vegetables", "Processed fruits and vegetables", "Juices and nonalcoholic drinks",
"Beverage materials including coffee and tea", "Fats and oils", "Sugar and sweets")

# total weights of food_index
cpi %>% filter(item_name %in% food_index, date == max(date)) %>% summarize( n = sum(weight))
cpi %>% filter(item_name == "Food at home", date == max(date)) %>% summarize( n = sum(weight))

cpi %>% filter(item_name %in% food_index) %>% filter(date == max(date)) %>%
  mutate(funct = fct_reorder(item_name, Pchange6)) %>%
  ggplot(aes(x=funct, y=Pchange6)) + geom_bar(stat = "identity", fill="skyblue") + coord_flip() + theme_classic()

cpi %>% filter(item_name %in% food_index) %>% filter(date == "2021-09-01") %>%
  mutate(funct = fct_reorder(item_name, Pchange6)) %>%
  ggplot(aes(x=funct, y=Pchange6)) + geom_bar(stat = "identity", fill="skyblue") + coord_flip() + theme_classic()


# Comparison point
cpi %>% filter(item_name %in% food_index) %>% filter(date == max(date) | date == max(date) %m-% months(7) | date == max(date) %m-% months(4)) %>%
  mutate(date = paste(as.character(month(date, label = TRUE, abbr = FALSE)), ", ", as.character(year(date)), sep = "")) %>%
  mutate(date = as.factor(date), name = reorder_within(item_name, Pchange3, date))  %>%
  ggplot(aes(name, Pchange3, fill = date)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~date, scales = "free_y", ncol=1) +
  coord_flip() +
  scale_x_reordered() +
  theme_classic() +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot") +
  labs(y = "Price Increase Over Previous Three Months",
       x = NULL,
       title = "Food Price Increases Have Broadened Over The Past Year",
       subtitle = "Price increase over previous three months of listed date. These categories represent 68 percent of all spending on food at home.",
       caption ="BLS, CPI, all subcategories of 'Food at home.' Seasonally Adjusted. Author's Calculation. @rortybomb")
  
#ggsave("food_prices.png", dpi="retina") 

# Total percent increase
cpi %>% filter(item_name %in% food_index) %>% filter(date == "2021-09-01" | date == "2022-02-01" | date == max(date)) %>%
  group_by(date) %>% summarize(total_increase = sum(Pchange3)) %>% arrange(desc(total_increase))



# NEW STUFF
cpi %>% filter(item_name == "All items less food, shelter, energy, and used cars and trucks") %>%
  filter(date == max(date)) %>% select(Wchange1a, Wchange3a, Wchange12)

cpi %>% filter(item_name %in% item_basket_topline) %>%
  group_by(item_name) %>%
  filter(date == max(date)) %>% select(Wchange1a, Wchange3a, Wchange12)



###### HOW MUCH WOULD GOODS COME DOWN TO BALANCE SERVICES

item_test <- c("Commodities less food and energy commodities", "Services less energy services")
cpi %>% filter(item_name %in% item_test) %>% arrange(date) %>% filter(year > 2020) %>% select(Wchange12, Wchange1a)

cpi %>% filter(item_name %in% item_test) %>% arrange(date) %>% filter(year > 2016) %>%
  ggplot(aes(date, value)) + geom_line() + theme_classic()


#####

#### FOOD GRAPHIC  ####

food_index <- c("Rice, pasta, cornmeal","Breakfast cereal", "Flour and prepared flour mixes",
                "Bread", "Fresh biscuits, rolls, muffins", "Cakes, cupcakes, and cookie",
                "Beef and veal", "Pork", "Other meats", "Poultry", "Fish and seafood", "Eggs",
                "Milk", "Fresh fruits", "Fresh vegetables", "Processed fruits and vegetables", "Juices and nonalcoholic drinks",
                "Beverage materials including coffee and tea", "Fats and oils", "Sugar and sweets")

# total weights of food_index - Comparison point
cpi %>% filter(item_name %in% food_index) %>% filter(date == "2019-09-01" | date == "2022-02-01" | date == max(date)) %>%
  mutate(date = paste(as.character(month(date, label = TRUE, abbr = FALSE)), ", ", as.character(year(date)), sep = "")) %>%
  mutate(date = factor(date, levels = c("May, 2022", "February, 2022", "September, 2021")), name = reorder_within(item_name, Pchange3, date))  %>%
  ggplot(aes(name, Pchange3, fill = date)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~date, scales = "free_y", ncol=1) +
  coord_flip() +
  scale_x_reordered() +
  theme_classic() +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.15, .03)) +
  theme(plot.title.position = "plot") +
  labs(y = "Price Increase Over Previous Three Months",
       x = NULL,
       title = "Food Price Increases Have Broadened Over The Past Year",
       subtitle = "Price increase over previous three months of listed date. These categories represent 68 percent of all spending on food at home.",
       caption ="BLS, CPI, all subcategories of 'Food at home.' Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute")

ggsave("graphics/food_prices.png", dpi="retina", width = 12, height=14, units = "in")

##### Graphic 2: Transitory #####

m_g2 <- c("Transportation commodities less motor fuel",
          "Airline fares", "Admissions", "Other lodging away from home including hotels and motels", "Car and truck rental")
# CHANGE BELOW
m_g2d <- "2019-01-01"
transitory_inflation <- cpi %>% filter(date > m_g2d, item_name %in% m_g2) %>%
  select(date, item_name, Wchange1a) %>%
  group_by(date) %>%
  summarize(transitory_inflation = sum(Wchange1a))

MI_GraphicT <- cpi %>% filter(date > m_g2d, item_name == "All items less food and energy") %>%
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
  scale_y_continuous(labels = percent)# +
#geom_text(size = 1, position = position_stack(vjust = 0.5)) +
#scale_x_date(date_labels = "%b %Y", breaks = "2 month")



#Dining Out! Matt Klein likes this one

cpi %>% filter(item_name == "Full service meals and snacks") %>%
  ggplot(aes(date, Wchange1a)) + geom_line() + theme_classic() +
  scale_y_continuous(labels = percent)

cpi %>% filter(item_name %in% c("Food", "Energy")) %>%
  ggplot(aes(date, Pchange1)) + geom_line() + theme_classic() +
  scale_y_continuous(labels = percent) + facet_wrap(~item_name,  scales = "free_y") + theme_ipsum()

cpi %>% filter(item_name %in% c("Food")) %>%
  ggplot(aes(date, Pchange1)) + geom_line() + theme_classic() +
  scale_y_continuous(labels = percent)


# Deflation calculation
auto_deflation <- c("All items less food and energy", "Used cars", "Transportation commodities less motor fuel")

trend_total <- cpi %>% filter(item_name == "Services less energy services", date == max(date)) %>%
  select(Wchange3a)
trend_total <- as.numeric(trend_total)

used_car_weights <- cpi %>% filter(item_name == "Transportation commodities less motor fuel", date == max(date)) %>%
  select(weight)
used_car_weights <- as.numeric(used_car_weights)

trend_total <- as_tibble(trend_total) %>% rename(trend_total = value)
trend_total %>% mutate(decline_5p = trend_total - 0.*used_car_weights/100)

months <- as_tibble(seq(ymd("2022/08/01"), ymd("2023/1/01"), by = "month"))
months %>% mutate(trend_total = 1 + trend_total) %>% mutate(trend_value = cumprod(trend_total)) %>%
                                                              rename(date = value)


cpi %>% filter(item_name == "Transportation commodities less motor fuel") %>%
  ggplot(aes(date,value)) + geom_line()

135/105
trend_total


cpi %>% filter(item_name %in% auto_deflation) %>% mutate(trend_total = trend_total) %>%
  
  
cpi %>% filter(item_name == "Food" | item_name == "Energy", date > "2014-01-01" & date < "2022-01-01") %>%
  ggplot(aes(date, value, color=item_name)) + geom_line() + theme_classic()


cpi %>% filter(item_name == "Medical care services") %>%
  filter(date > "2011-01-01") %>%
  ggplot(aes(date,Wchange1a)) + geom_line() + theme_classic()



########## HOW MANY INCREASES ARE BIGGER THAN BEFORE ############


baseline <- cpi %>% filter(date == max(date) | date == "2019-09-01") %>%
  select(date, item_name, Pchange12, weight) %>% mutate(year = as.numeric(year(date)))

baseline2019 <- baseline %>% filter(year == 2019) %>% rename(Pchange12_2019 = Pchange12) %>% select(-year, -weight)

baseline <- baseline %>% filter(year == 2022) %>% left_join(baseline2019, by="item_name") %>% rename(Pchange12_2022 = Pchange12)

baseline %>% mutate(bigger = Pchange12_2022 > Pchange12_2019) %>% filter(bigger == 1) %>%
  summarize(weights = sum())

baseline %>% ggplot(aes(Pchange12_2019, Pchange12_2022, size=weight)) + geom_point() + geom_abline(intercept = 0, slope = 1) +
  ylim(-1,1) + xlim(-1,1)


baseline <- cpi %>% filter(date == "2018-09-01" | date == "2019-09-01") %>%
  select(date, item_name, Pchange12, weight) %>% mutate(year = as.numeric(year(date)))

baseline2019 <- baseline %>% filter(year == 2019) %>% rename(Pchange12_2019 = Pchange12) %>% select(-year, -weight)

baseline <- baseline %>% filter(year == 2018) %>% left_join(baseline2019, by="item_name") %>% rename(Pchange12_2022 = Pchange12)

baseline %>% mutate(bigger = Pchange12_2022 > Pchange12_2019) %>% 
  baseline %>% ggplot(aes(Pchange12_2019, Pchange12_2022)) + geom_point() + geom_abline(intercept = 0, slope = 1) +
  ylim(-1,1) + xlim(-1,1)


############
#  less food and energy
exported_core <- cpi %>% filter(item_name == "All items less food and energy", date >= "2019-01-01") %>%
  select(date, item_name, Wchange1a, value) %>%
  mutate(Pchange3 = (value/lag(value, 3)-1))

GDP %>% filter(LineDescription == "Personal consumption expenditures", date >= "2019-01-01") %>%
  left_join(exported_core, by="date") %>%
  mutate(C_change = DataValue/lag(DataValue,1)-1) %>%
  ggplot(aes(date,C_change)) + geom_line() + theme_classic() +
  geom_line(aes(date,Pchange3, color="red")) +
  labs(x="",y="", title="Inflation takes off even as consumption remains well below trend",
       subtitle="3-month percent change, CPI core inflation (in red) versus consumption") +
  theme(panel.grid.major.y = element_line(size=0.5),
        plot.title = element_text(size = 15, face="bold"),
        plot.subtitle = element_text(size = 15),
        axis.text.x = element_text(size = 16)
  ) +
  scale_x_date(date_labels = "%b %y", breaks = "6 month") +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none")

ggsave("C_vs_inflation1.png", dpi="retina", width = 9, height=4.5, units = "in")

GDP %>% filter(LineDescription == "Personal consumption expenditures", date >= "2019-01-01") %>%
  left_join(exported_core, by="date") %>%
  mutate(DV_n = DataValue/DataValue[date=="2019-12-01"]) %>%
  mutate(value_n = value/value[date=="2019-12-01"]) %>%
  ggplot(aes(date,DV_n)) + geom_line() + theme_classic() +
  geom_line(aes(date,value_n, color="red")) +
  labs(x="",y="", title="Inflation takes off even as consumption remains well below trend",
       subtitle="Value (Dec 2019 = 1), CPI core inflation (in red) versus consumption") +
  theme(panel.grid.major.y = element_line(size=0.5),
        plot.title = element_text(size = 15, face="bold"),
        plot.subtitle = element_text(size = 15),
        axis.text.x = element_text(size = 16)
  ) +
  scale_x_date(date_labels = "%b %y", breaks = "6 month") +
  theme(legend.position = "none")

ggsave("C_vs_inflation2.png", dpi="retina", width = 9, height=4.5, units = "in")


cpi_data %>% filter(item_name == "Medical care", area_code == "0000", seasonal == "S") %>%
  mutate(diff = value/lag(value,1)-1) %>%
  ggplot(aes(date,diff)) + geom_line() + theme_classic()


a <- cpi_data %>% filter(item_name %in% c("Health insurance")) %>%
  filter(period != "M13", begin_period != "S01") %>% filter(area_code == "0000")

a <- a %>%
  group_by(item_name) %>%
  arrange(date) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Pchange12 = (value/lag(value,12)-1)) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
  ungroup()

a %>% ggplot(aes(date, Wchange1a)) + geom_bar(stat="identity") + theme_lass

a %>% ggplot(aes(date, Wchange1, color=item_name)) + geom_line() + theme_classic()

View(a %>% select(date, Wchange1a))


#### "full service meals and snacks" ####

MI_dates <- unique(MI_Graphic2a$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates = MI_dates[seq(1, length(MI_dates), 3)]

ggsave("graphics/g2_services_goods.png", dpi="retina", width = 12, height=6.75, units = "in")

cpi %>% filter(item_name == "Full service meals and snacks", year > 2019) %>%
  
  ggplot(aes(date,Pchange1a)) + geom_line() + theme_lass


##### MATTHEW KLEIN'S GO TO MEASURE
cpi %>% filter(date > "2020-12-01", item_name == "Full service meals and snacks") %>%
  mutate(num_label = round(100*Pchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  ggplot(aes(x = date, y = Pchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity', size=0) +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "Full service meals and snacks, monthly percent change, annualized",
       subtitle="Note: After the BLS seasonal adjustment for 2022.",
       caption ="BLS, CPI, 2022 weights, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  geom_text(size = 4, position = position_stack(vjust = 0.5), color="black") +
  scale_fill_brewer(palette="RdPu") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b %y") +
  theme(legend.text = element_text(size=18),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))

ggsave("graphics/full_meals.png", dpi="retina", width = 12, height=6.75, units = "in")


###### Let's go wide and deep with maximum prices #####

lowest <- read_csv("weights/most_prices.csv") %>% filter(category != "Meta", lowest == 1)

MI_dates <- cpi %>% filter(date > "2011-01-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates_three = MI_dates[seq(1, length(MI_dates), 6)]

cpi %>% filter(item_name %in% lowest$item_name, !is.na(Pchange1), date > "2014-01-01") %>%
  group_by(date) %>% summarize(n = n()) %>% ungroup() %>% summarize(min = min(n), max = max(n))


a <- cpi %>% filter(item_name %in% lowest$item_name, !is.na(Pchange1)) %>%
  group_by(date) %>% summarize(n = n()) %>% ungroup()


cpi %>% filter(item_name %in% lowest$item_name, !is.na(Pchange1), date >="2012-01-01") %>% mutate(p3 = (Pchange1a > 0.03)) %>%
  group_by(date) %>%
  summarize(total_3 = sum(p3)/n()) %>% ungroup() %>% ungroup() %>%
  ggplot(aes(date, total_3)) + geom_line(size=1) + theme_lass + scale_fill_brewer(palette="Paired") +
  labs(y = NULL,
       x = NULL,
       title = "Percent of Items With 3% Price Growth Has Decreased From High Levels",
       subtitle = "Percent of CPI items having at least 3 percent monthly price increases, annualized.",
       caption ="BLS, CPI, 2022 weights, only seasonally adjusted items included. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y")

#ggsave("graphics/three_percent_growth.png", dpi="retina", width = 12, height=6.75, units = "in")

dates_3_percent <- 
  cpi %>% filter(item_name %in% lowest$item_name, !is.na(Pchange1), date > "2011-01-01") %>% mutate(p3 = (Pchange1a > 0.03)) %>%
  group_by(date) %>%
  summarize(total_3 = sum(p3)/n()) %>% ungroup() %>% ungroup()

cpi %>% filter(item_name %in% lowest$item_name, !is.na(Pchange1), date >= "2020-01-01") %>%
  group_by(item_name) %>% mutate(nvalue = value/value[date=="2020-01-01"]) %>% ungroup() %>%
  ggplot(aes(date,nvalue,color=item_name)) + geom_line() + theme_classic() + theme(legend.position = "none")
  
cpi %>% filter(item_name %in% lowest$item_name, !is.na(Pchange1), date >= "2020-01-01") %>%
  group_by(item_name) %>% mutate(nvalue = value/value[date=="2020-01-01"]) %>% ungroup() %>%
  filter(date == max(date)) %>% select(item_name, nvalue) %>% arrange(desc(nvalue))

###### USED AUTOS ######


cpi %>% filter(date > "2018-12-01", item_name %in% c("All items less food and energy", "Used cars and trucks")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core goods and services` = `All items less food and energy` - `Used cars and trucks`) %>%
  select(-`All items less food and energy`) %>%
  rename(`Used autos` = `Used cars and trucks`) %>%
  pivot_longer(c(`Used autos`, `Rest of core goods and services`), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("Used autos", "Rest of core goods and services"))) %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity', size=0) +
  labs(y = NULL,
       x = NULL,
       title = "title4",
       subtitle = "Monthly core goods contribution to inflation, annualized.",
       caption ="Autos is New and Used Cars and Motor Parts. BLS, CPI, 2022 weights, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  geom_text(size = 4, position = position_stack(vjust = 0.5), color="black") +
  scale_fill_brewer(palette="Greens") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  theme(legend.position = c(0.60,0.90), legend.title = element_blank(), legend.text = element_text(size=16),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))


ggsave("graphics/g3_goods_used_autos.png", dpi="retina", width = 12, height=6.75, units = "in")


###### THAT INDEX ########


cpi %>% filter(date > "2018-12-01", item_name %in% c("All items less food and energy", "Used cars and trucks", "Shelter")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core goods and services` = `All items less food and energy` - `Used cars and trucks` - `Shelter`) %>%
  select(-`All items less food and energy`) %>%
  rename(`Used autos` = `Used cars and trucks`) %>%
  pivot_longer(c(`Used autos`, `Rest of core goods and services`,`Shelter`), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("Used autos", "Shelter", "Rest of core goods and services"))) %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity', size=0) +
  labs(y = NULL,
       x = NULL,
       title = "title4",
       subtitle = "Monthly core goods contribution to inflation, annualized.",
       caption ="Autos is New and Used Cars and Motor Parts. BLS, CPI, 2022 weights, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  geom_text(size = 4, position = position_stack(vjust = 0.5), color="black") +
  scale_fill_brewer(palette="Greens") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  theme(legend.position = c(0.60,0.90), legend.title = element_blank(), legend.text = element_text(size=16),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))


ggsave("graphics/g3_goods_used_autos.png", dpi="retina", width = 12, height=6.75, units = "in")



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
       title = "Recent Deflation in Goods is Driven by Used Cars",
       subtitle = "Monthly contribution to inflation, annualized.",
       caption ="BLS, CPI, 2022 weights, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  geom_text(size = 2, position = position_stack(vjust = 0.5), color="black") +
  scale_fill_brewer(palette="Greens") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=HI_dates) +
  theme(legend.position = c(0.30,0.75), legend.title = element_blank(), legend.text = element_text(size=16),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))


ggsave("graphics/g3_goods_just_used_autos.png", dpi="retina", width = 12, height=6.75, units = "in")

HI_dates <- cpi %>% filter(date >= "2000-01-01")
HI_dates <- unique(HI_dates$date)
HI_dates <- sort(HI_dates, decreasing = TRUE)
HI_dates = HI_dates[seq(1, length(HI_dates), 24)]

cpi %>% filter(date >= "2000-01-01", item_name %in% c("Commodities less food and energy commodities")) %>%
  mutate(item_name = "Core Goods") %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity', size=0) +
  labs(y = NULL,
       x = NULL,
       title = "Core Goods Inflation is Around Zero Percent Before the Pandemic",
       subtitle = "Monthly contribution to inflation, annualized.",
       caption ="BLS, CPI, 2022 weights, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  geom_text(size = 2, position = position_stack(vjust = 0.5), color="black") +
  scale_fill_brewer(palette="Greens") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=HI_dates) +
  theme(legend.position = c(0.60,0.55), legend.title = element_blank(), legend.text = element_text(size=16),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))

ggsave("graphics/g3_goods_just.png", dpi="retina", width = 12, height=6.75, units = "in")


aa <- cpi %>% filter(item_name == "All items less food and energy") %>% select(date, value, Pchange1a)
tail(aa)


#### REVISION ####

old_data <- read_csv("data/core_cpi_previous.csv") %>% mutate(pre_change = CPILFESL/lag(CPILFESL,1), pre_change = (pre_change)^12-1) %>%
  select(date = DATE, pre_change)

  
  
  ##### Graphic1: Core Inflation ####
MI_dates <- cpi %>% filter(date > "2021-12-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates_three = MI_dates[seq(1, length(MI_dates), 6)]

cpi %>% filter(date > "2021-12-01", item_name == "All items less food and energy") %>%
  select(date, item_name, Pchange1a) %>%
  left_join(old_data, by="date") %>%
  ggplot(aes(x = date, y = Pchange1a)) + theme_lass +
  geom_line() +
  geom_line(aes(date,pre_change), color="red", linetype="dashed") +
  labs(y = NULL,
       x = NULL,
       title = "New Revisions to CPI Seasonal Adjustment",
       subtitle = "Monthly percent increase in core goods and services, annualized. Dotted red line is previous value.",
       caption ="BLS, CPI, 2022 weights, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates_three)

ggsave("graphics/g3_goods_just.png", dpi="retina", width = 12, height=6.75, units = "in")


#####
item_basket_core_services <- c("Services less energy services",
                               "Shelter",
                               "Medical care services",
                               "Transportation services",
                               "Recreation services",
                               "Education and communication services",
                               "Other personal services")

item_basket_core_goods <- c("Commodities less food and energy commodities",
                            "Household furnishings and supplies",
                            "Apparel",
                            "Transportation commodities less motor fuel",
                            "Medical care commodities",
                            "Recreation commodities",
                            "Education and communication commodities",
                            "Alcoholic beverages",
                            "Other goods")



cpi %>% filter(item_name %in% item_basket_core_goods) %>%
  mutate(item_name = str_replace_all(item_name,"Commodities less food and energy commodities","All Core Goods")) %>%
  group_by(item_name) %>%
  mutate(trend = value[date=="2020-01-01"]/value[date=="2018-01-01"], trend = trend^(0.5)-1) %>%
  filter(year(date) > 2016) %>% ungroup() %>%
  mutate(item_name = factor(item_name)) %>%
  mutate(item_name = relevel(item_name, "All Core Goods")) %>%
  ggplot(aes(date,Pchange1a)) + geom_line() + theme_classic() + facet_wrap(~item_name, scales = "free_y") +
  scale_y_continuous(labels = percent) +
  geom_line(aes(date,trend),color="red") +
  labs(y="",
       x="",
       title="Many categories of goods remain above prepandemic trend, not just autos",
       subtitle="Monthly percentage change, annualized. Red line is 2018-2019 value.",
       caption="BLS, CPI, Seasonally Adjusted, author's calculations. Mike Konczal, Roosevelt Institute.")

ggsave("graphics/each_goods.png", dpi="retina", width = 12, height=6.75, units = "in")

cpi %>% filter(item_name %in% c("Owners' equivalent rent of residences","Rent of primary residence")) %>%
  ggplot(aes(date,Pchange1a, color=item_name)) + geom_line() + theme_classic() +
  theme(legend.position = c(0.4,0.8)) +
  scale_y_continuous(labels = percent)


###### THE FIRST MONTH - JANUARY TURNOVER GRAPHIC #####
lowest <- read_csv("weights/most_prices.csv") %>% filter(category != "Meta", lowest == 1)

cpi_NSA <- cpi_data %>%
  filter(period != "M13") %>%
  filter(seasonal == "U") %>%
  filter(item_name %in% lowest$item_name) %>%
  filter(area_code == "0000") %>%
  mutate(month = month(date))

cpi_NSA %>% group_by(item_name) %>%
  mutate(pchange = value/lag(value)-1) %>%
  filter(!is.na(pchange)) %>%
  ungroup() %>%
  group_by(month) %>%
  summarize(mean(pchange))

##### AEI CHART ####
lowest <- read_csv("weights/most_prices.csv") %>% filter(lowest == 1)

categories <- lowest %>% select(item_name, category)

AEI_chart <- cpi %>% filter(item_name %in% lowest$item_name) %>%
  left_join(categories, by=c("item_name")) %>% filter(category %in% c("Goods","Services"))

c2 <- AEI_chart %>% group_by(item_name) %>%
  filter(date == min(date)) %>%
  filter(date <= "2000-01-01") %>% ungroup()

c3 <- AEI_chart %>% filter(item_name %in% c2$item_name) %>%
 # filter(date >= "2000-01-01") %>%
  group_by(item_name, category) %>%
  summarize(change = value[date=="2023-01-01"]/value[date=="2000-01-01"]-1) %>%
  ungroup() %>%
  arrange(desc(change))

c3 <- c3 %>%
  mutate(item_name = str_replace_all(item_name, "Club membership for shopping clubs, fraternal, or other organizations, or participant sports fees","Club memberships")) %>%
  mutate(item_name = str_replace_all(item_name, "Other lodging away from home including hotels and motels","Other lodging"))

c3 %>% head(20) %>%
  mutate(category = as.factor(category), name = reorder_within(item_name, change, category))  %>%
  ggplot(aes(name, change, fill = category)) +
  geom_col(size=0) +
  #facet_wrap(~category, scales = "free_y", ncol=1) +
  coord_flip() +
  scale_x_reordered() +
  theme_lass +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot") +
  labs(y = NULL,
       x = NULL,
       title = "Food Price Increases Have Broadened Over The Past Year",
       subtitle = "Price increase over previous three months of listed date. These categories represent 68 percent of all spending on food at home.",
       caption ="BLS, CPI, all subcategories of 'Food at home.' Seasonally Adjusted. Author's Calculation. @rortybomb") +
  theme(axis.text.y = element_text(size=18, face="bold"))

#ggsave("food_prices.png", dpi="retina") 


##### Attempting to redo the disaggregation by categories ####


item_basket_core_services <- c("Services less energy services",
                               "Shelter",
                               "Medical care services",
                               "Transportation services",
                               "Recreation services",
                               "Education and communication services",
                               "Other personal services")

item_basket_core_goods <- c("Commodities less food and energy commodities",
                            "Household furnishings and supplies",
                            "Apparel",
                            "Transportation commodities less motor fuel",
                            "Medical care commodities",
                            "Recreation commodities",
                            "Education and communication commodities",
                            "Alcoholic beverages",
                            "Other goods")

max_date <- max(unique(cpi$date))
delay_date_6 <- max_date %m-% months(6)

df <- cpi %>% filter(item_name %in% c(item_basket_core_goods,item_basket_core_services, "All items less food and energy")) %>%
  select(date, item_name, value) %>%
  group_by(item_name) %>%
  summarize(rate_2022 = value[date=="2022-09-01"]/value[date=="2021-09-01"]-1,
            rate_2019 = value[date=="2019-09-01"]/value[date=="2018-09-01"]-1,
            rate_2023 = (value[date==max_date]/value[date==delay_date_6])^2-1) %>%
  ungroup() %>%
  pivot_longer(rate_2022:rate_2023, names_to = "time_period", values_to = "value")


df %>%
  ggplot(aes(time_period,value)) + geom_col() + coord_flip() + facet_wrap(~item_name)


cpi %>% filter(item_name %in% c(item_basket_core_services)) %>%
  filter(year(date) > 2016) %>%
  ggplot(aes(date,Pchange12)) + geom_line() +
  facet_wrap(~item_name)

# Not sure the right way to do this - are we normal at a higher level?
cpi %>%
  group_by(date) %>%
  filter(!is.na(Pchange3)) %>%
  summarize(std = sd(Pchange3)) %>%
  ggplot(aes(date,std)) + geom_line()
                 
lowest <- read_csv("weights/most_prices.csv") %>% filter(category != "Meta", lowest == 1)
core <- lowest %>% filter(category %in% c("Services","Goods"))

cpi %>%
  filter(item_name %in% core$item_name) %>%
  group_by(date) %>%
  filter(!is.na(Pchange3)) %>%
  summarize(std = sd(Pchange3)) %>%
  ggplot(aes(date,std)) + geom_line()


cpi %>% filter(item_name %in% lowest$item_name, !is.na(Pchange1), date > "2012-01-01") %>%
  group_by(date) %>%
  filter(!is.na(Pchange3)) %>%
  summarize(std = sd(Pchange3)) %>%
  ggplot(aes(date,std)) + geom_line()


#### Regional divergence? ####

a <- cpi_data %>% filter(item_name == "Services less energy services")
a <- a %>% filter(!is.na(date)) %>% filter(date == max(date))

unique(a$series_title)

item_basket_headline <- c("All items less food and energy","Services less energy services","Commodities less food and energy commodities")

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

regional_area_codes <- c("0100","0200","0300","0400")
area_names <- GET("https://download.bls.gov/pub/time.series/cu/cu.area", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()

a <- cpi_data %>% filter(item_name %in% c(item_basket_core_services,item_basket_core_goods), area_code %in% regional_area_codes, !is.na(date), periodicity_code == "R") %>%
  left_join(area_names, by="area_code") %>%
  filter(year(date) > 2013) %>%
  group_by(series_title, area_code) %>%
  arrange(date) %>%
  mutate(YoY = value/lag(value,12)-1) %>%
  ungroup() %>%
  group_by(date, item_name) %>%
  mutate(high_low = if_else(YoY == max(YoY) | YoY == min(YoY), TRUE, FALSE)) %>%
  ungroup()

list_keep <- a %>% filter(date == max(date), high_low) %>% select(item_name, area_code) %>% arrange(item_name)
list_keep[1,2]

for (i in 1:28) {
b <- a %>% filter(item_name == as.character(list_keep[i,1]) & area_code == as.character(list_keep[i,2]))
if(i == 1)
  c <- b
else
  c <- rbind(c,b)
}

c %>%
  ggplot(aes(date,YoY, color=area_names)) + geom_line(size=1.2) + theme_classic() + facet_wrap(~item_name, scales = "free") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=25),
        strip.text = element_text(size=)) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  labs(title="Growing Regional Differences Within Inflation Categories, Especially Services",
       subtitle="Year over Year inflation, seasonally unadjusted, by region for major categories, only showing highest and lowest lines for last month",
       caption="BLS, CPI. Preliminary, what does this mean? Mike Konczal, Roosevelt Institute")

ggsave("graphics/regionals.png", dpi="retina", width = 12, height=12, units = "in")

a %>%
  ggplot(aes(date,YoY, color=area_name)) + geom_line(size=1) + theme_classic() + facet_wrap(~item_name, scales = "free") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=25),
        strip.text = element_text(size=)) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  labs(title="Growing Regional Differences Within Inflation Categories, Especially Services",
       subtitle="Year over Year inflation, seasonally unadjusted, by region for major categories",
       caption="BLS, CPI. Preliminary, what does this mean? Mike Konczal, Roosevelt Institute")

ggsave("graphics/regionals_all.png", dpi="retina", width = 12, height=12, units = "in")


cpi_data %>% filter(item_name %in% item_basket_headline, area_code %in% regional_area_codes, !is.na(date)) %>%
  left_join(area_names, by="area_code") %>%
  filter(year(date) > 2011) %>%
  group_by(series_title, area_code) %>%
  arrange(date) %>%
  mutate(YoY = value/lag(value,12)-1) %>%
  ungroup() %>%
  ggplot(aes(date,YoY, color=area_name)) + geom_line() + theme_classic() + facet_wrap(~item_name) 



a <- cpi_data %>% filter(item_name %in% c(item_basket_core_services,item_basket_core_goods), area_code %in% regional_area_codes, !is.na(date), periodicity_code == "R") %>%
  left_join(area_names, by="area_code") %>%
  filter(year(date) > 2013) %>%
  group_by(series_title, area_code) %>%
  arrange(date) %>%
  mutate(YoY = value/lag(value,12)-1) %>%
  ungroup() %>%
  group_by(date, item_name) %>%
  mutate(high_low = if_else(YoY == max(YoY) | YoY == min(YoY), TRUE, FALSE)) %>%
  ungroup()


##### Supercore regional ####

cpi %>% filter(date > "2017-12-01", item_name %in% c("Services less energy services", "Shelter", "Commodities less food and energy commodities")) %>%
  mutate(item_name = str_replace_all(item_name, "Commodities less food and energy commodities","Core_goods")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core services` = `Services less energy services` - `Shelter`) %>%
  select(-`Services less energy services`) %>%
  pivot_longer(c(Shelter, `Rest of core services`,Core_goods), names_to = "item_name", values_to = "Wchange1a")


cpi_data %>% filter(item_name %in% c("Services less energy services", "Shelter", "Commodities less food and energy commodities"),
                    area_code %in% regional_area_codes, !is.na(date), periodicity_code == "R") %>%
  mutate(item_name = str_replace_all(item_name, "Commodities less food and energy commodities","Core_goods")) %>%
  group_by(series_title, area_name) %>%
  arrange(date) %>%
  mutate(YoY = value/lag(value,12)-1) %>%
  mutate(YoY_W = YoY*weight/100) %>%
  ungroup() %>%
  filter(year(date) >= 2015) %>%
  select(date, item_name, area_name, YoY_W) %>%
  pivot_wider(names_from = item_name, values_from = YoY_W) %>%
  mutate(`Rest of core services` = `Services less energy services` - `Shelter`) %>%
  select(-`Services less energy services`) %>%
  pivot_longer(c(Shelter, `Rest of core services`,Core_goods), names_to = "item_name", values_to = "YoY_W") %>%
  mutate(item_name = str_replace_all(item_name, "Core_goods","Core goods")) %>%
  mutate(item_name = factor(item_name, levels = c("Core goods", "Shelter", "Rest of core services"))) %>%
  mutate(num_label = round(100*YoY_W, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  
  ggplot(aes(x = date, y = YoY_W, color=area_name)) +
  geom_line(size=1) +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  facet_grid(~item_name) +
  labs(y = NULL,
       x = NULL,
       title = "Onion by region, why not?",
       subtitle = "Monthly contribution to inflation, year-over-year, by region. Preliminary. Beware, this uses national weights for subregions.",
       caption ="BLS, CPI, seasonally unadjusted, 2022 weights prior to 2023. Shelter category is 'shelter.' Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  scale_color_brewer(palette="Spectral", name = "item_name") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=19)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=25),
        strip.text = element_text(size=20))

ggsave("graphics/three_regional_categories.png", dpi="retina", width = 12, height=6.75, units = "in")
  




cpi_data %>% filter(item_name %in% c("Services less rent of shelter", "Shelter", "Commodities less food and energy commodities"),
                    area_code %in% regional_area_codes, !is.na(date), periodicity_code == "R") %>%
  mutate(item_name = str_replace_all(item_name, "Commodities less food and energy commodities","Core_goods")) %>%
  group_by(series_title, area_name) %>%
  arrange(date) %>%
  mutate(YoY = value/lag(value,12)-1) %>%
  mutate(YoY_W = YoY*weight/100) %>%
  ungroup() %>%
  filter(year(date) >= 2015) %>%
  select(date, item_name, area_name, YoY) %>%
  ggplot(aes(x = date, y = YoY, color=area_name)) +
  geom_line(size=1) +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  facet_grid(~item_name) +
  labs(y = NULL,
       x = NULL,
       title = "Onion by region, why not?",
       subtitle = "Monthly contribution to inflation, year-over-year, by region. Preliminary. Beware, this uses national weights for subregions.",
       caption ="BLS, CPI, seasonally unadjusted, 2022 weights prior to 2023. Shelter category is 'shelter.' Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  scale_color_brewer(palette="Spectral", name = "item_name") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=19)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=25),
        strip.text = element_text(size=20))

ggsave("graphics/three_regional_categories.png", dpi="retina", width = 12, height=6.75, units = "in")




lowest <- read_csv("weights/most_prices.csv") %>% filter(category != "Meta", lowest == 1)
core <- lowest %>% filter(category %in% c("Services","Goods"))

MI_dates <- cpi %>% filter(date > "2012-01-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates_three = MI_dates[seq(1, length(MI_dates), 12)]

cpi %>% filter(item_name %in% core$item_name, !is.na(Pchange1), date > "2012-01-01") %>% mutate(p3 = (Pchange1a > 0.03)) %>%
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

max_date <- max(cpi$date)

a <-
cpi %>% filter(item_name %in% core$item_name) %>%
  group_by(item_name) %>%
  summarize(change_2019 = value[date=="2020-01-01"]/value[date=="2019-01-01"]-1,
            change_2023 = value[date==max_date]/value[date == max_date %m-% months(12)]-1) %>%
  mutate(relative_diff = change_2023/change_2019,
         absolute_diff = change_2023-change_2019,
         time_one = 1,
         time_two = 2) %>%
  
  ggplot() + geom_segment(aes(x = time_one, y = change_2019, xend = time_two, yend = change_2023)) + theme_classic()


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

#### Unadjusted monthly ####

cpi_data %>% filter(seasonal == "U", item_name == "All items less food and energy", period != "M13", area_code == "0000", substr(period,1,1) == "M") %>%
  select(date, item_name, value) %>%
  mutate(change = (value/lag(value,1))^12-1, month = month(date), monthF = as.factor(month), year=year(date)) %>%
  ggplot(aes(monthF,change, color=as.factor(year))) + geom_line() + theme_classic()
  
substr(a$period,1,1)


unique(cpi$item_name) %>% filter(item_)

"Uncooked beef roasts" %in% cpi$item_name
"Men's underwear, hosiery, nightwear, and swimwear" %in% cpi$item_name
unique(cpi[grepl("wear", cpi$item_name),]$item_name)

View(cpi %>% filter(item_name %in% c("Commodities less food and energy commodities","Used cars and trucks")) %>% select(item_name, date, Pchange1, Wchange1, weight))

##### Core Goods Ex Used Car Index ####
cpi_goods_ex_autos_index <-
  cpi %>%
  select(date, item_name, Wchange1, weight_nhs) %>%
  mutate(weight_nhs = weight_nhs/100) %>%
  group_by(date) %>%
  summarize(goods_ex_auto_m1 = Wchange1[item_name == "Commodities less food and energy commodities"] - Wchange1[item_name == "Used cars and trucks"],
            goods_ex_auto_weight = weight_nhs[item_name == "Commodities less food and energy commodities"] - weight_nhs[item_name == "Used cars and trucks"],
  ) %>%
  ungroup() %>%
  mutate(goods_ex_auto_m1_W = goods_ex_auto_m1/goods_ex_auto_weight) %>%
  mutate(goods_ex_auto_m1_WA = (goods_ex_auto_m1_W+1)^12-1) %>%
  mutate(index = goods_ex_auto_m1_W+1) %>% filter(!is.na(index)) %>%
  mutate(index = cumprod(index))
#### Make difference ####
tail(cpi_goods_ex_autos_index)

cpi_goods_ex_autos_index %>% mutate(diff = (index/lag(index,3))^4-1) %>%
  ggplot(aes(date,diff)) + geom_line(size=1.2) + theme_lass +
  scale_color_manual(values=c("#F67280")) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  theme(legend.position = "none", legend.title = element_blank(), legend.text = element_text(size=16),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19)) +
  labs(title="Core Goods ex Used Autos, CPI, 3-month Percent Change, Annualized",
       caption="BLS, CPI, Seasonally-Adjusted. Author's calculations. Mike Konczal, Roosevelt Institute.") +
  geom_hline(yintercept = 0, color="white")

ggsave("graphics/goods_ex_autos.png", dpi="retina", width = 12, height=6.75, units = "in")




#### Single Food Chart ####


##### FOOD #####


food_index <- c("Rice, pasta, cornmeal","Breakfast cereal", "Flour and prepared flour mixes",
                "Bread", "Fresh biscuits, rolls, muffins", "Cakes, cupcakes, and cookie",
                "Beef and veal", "Pork", "Other meats", "Poultry", "Fish and seafood", "Eggs",
                "Milk", "Fresh fruits", "Fresh vegetables", "Processed fruits and vegetables", "Juices and nonalcoholic drinks",
                "Beverage materials including coffee and tea", "Fats and oils", "Sugar and sweets")

# total weights of food_index
cpi %>% filter(item_name %in% food_index, date == max(date)) %>% summarize( n = sum(weight))
cpi %>% filter(item_name == "Food at home", date == max(date)) %>% summarize( n = sum(weight))

cpi %>% filter(item_name %in% food_index, date == max(date), Pchange3 < 0) %>%  summarize( n = sum(weight))
#### GRAPHIC FOOD ####
# Comparison point
#food_dates <- cpi %>% filter(item_name %in% food_index) %>% filter(date == max(date) | date == max(date) %m-% months(7) | date == max(date) %m-% months(4)) %>%
food_dates <- cpi %>% filter(item_name %in% food_index) %>%
  group_by(item_name) %>%
  mutate(Pchange6 = (value/lag(value, 6)-1)) %>%
  ungroup() %>%
  filter(date == max(date) | date == "2022-06-01") %>%
  mutate(date = paste(as.character(lubridate::month(date, label = TRUE, abbr = FALSE)), ", ", as.character(year(date)), sep = "")) %>%
  mutate(date = factor(date,levels=c("June, 2023","June, 2022")), name = reorder_within(item_name, Pchange6, date))

ggplot(food_dates, aes(name, Pchange6, fill = date)) +
  geom_col(show.legend = FALSE, size=0) +
  facet_wrap(~date, scales = "free_y", ncol=1) + theme_lass +
  coord_flip() +
  scale_x_reordered() +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot") +
  labs(y = "Price Increase Over Previous Three Months",
       x = NULL,
       title = "Food Price Increases Have Narrowed Recently, With Eggs in Freefall",
       subtitle = "Price increase over previous six months of date. Categories represent 68 percent of spending on food at home.",
       caption ="BLS, CPI, all subcategories of 'Food at home.' Seasonally Adjusted. Author's Calculation. @rortybomb") +
  theme(axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=22, face="bold"), strip.text = element_text(color="white", size = 14)) +
  theme(panel.spacing.y=unit(0, "lines"), panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size=12, color="white"))

ggsave("graphics/food_chart.png", dpi="retina", width = 12, height=12, units = "in")


ahe <- read_csv("data/ahe.csv") %>% rename(ahe = CES0500000003, ahepi = AHETPI, cpi = CPIAUCSL, date = DATE) %>%
  mutate(ahe = ahe/lag(ahe,1)-1, ahepi = ahepi/lag(ahepi,1)-1, cpi = cpi/lag(cpi,1)-1) %>%
  mutate(real_ahe = ahe - cpi, real_ahepi = ahepi - cpi)


ahe %>%
  ggplot() + geom_line(aes(date, real_ahe)) + geom_line(aes(date, real_ahepi),color="red")


# Version with all prices, and line
lowest <- read_csv("weights/most_prices.csv") %>% filter(category != "Meta", lowest == 1)

cpi_data %>% filter(seasonal == "S", item_name %in% lowest$item_name) %>%
  group_by(item_name) %>%
  summarize(c_2023 = value[date==max(date)]/value[date == max(date) %m-% months(12)]-1,
            c_2022 = value[date=="2022-12-01"]/value[date=="2021-12-01"]-1) %>%
  ungroup() %>%
  left_join(lowest, by="item_name") %>%
  ggplot(aes(c_2022, c_2023, size=weight, color=category)) + geom_point() + theme_classic() +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color = "red")


cpi_data %>% filter(series_id == "CUUR0000SA0L1E") %>%
  select(date, value) %>%
  mutate(month = month(date), year = as.factor(year(date))) %>%
  mutate(pchange1 = value/lag(value,1)-1) %>%
  filter(year(date) %in% c(2019,2022,2023)) %>%
  ggplot(aes(month, pchange1, color=year)) + geom_line(size=1.2) + theme_lass +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(labels = percent) +
  geom_text_repel(aes(label=year), size=7,
                  data = . %>% group_by(year) %>% filter(month == 4) %>% ungroup()) +
  labs(title="Unadjusted, sliding into prepandemic rates?",
       subtitle="Seasonally unadjusted values for core CPI inflation, 1-month percent change, not annualized.",
       caption="Inspired by Paul Romer's blog. Mike Konczal, Roosevelt Institute")


median_terms <- read_csv("weights/mediancpi_component_table.csv") %>% mutate(item_name = Component)

median_terms$item_name

density_test <-
  cpi %>% filter(year(date) == 2023) %>%
  filter(item_name %in% median_terms$item_name) %>%
  select(date, Pchange3a, Pchange1a) %>%
  pivot_longer(Pchange3a:Pchange1a, names_to = "length_type", values_to = "Pvalues")

write_csv(density_test, file = "../../../Desktop/shiny_density_test.csv")

thresholds <- density_test %>% 
  group_by(date) %>% 
  summarise(
    lower_thresh = quantile(Pchange3a, 0.03),
    upper_thresh = quantile(Pchange3a, 0.97)
  )

# Merge thresholds back into the original data
density_test <- merge(density_test, thresholds, by = "date")

# Filter data to exclude the tails
density_test_filtered <- density_test %>% 
  filter(Pchange3a > lower_thresh & Pchange3a < upper_thresh)



# Create a density plot
ggplot(density_test_filtered, aes(x = Pchange3a, fill = as.factor(date))) + 
  geom_density(alpha = 0.5) + 
  labs(
    title = "PRELIMINARY: Density Plot - 3 month change, 45 CPI items, top/bottom 3% excluded",
    x = "Value",
    y = "Density",
    caption = "Mike Konczal, Roosevelt Institute"
  ) + 
  theme_minimal() + 
  scale_fill_manual(values = c("skyblue", "pink"), name = "Date") +
  theme(legend.position = c(0.8,0.7))
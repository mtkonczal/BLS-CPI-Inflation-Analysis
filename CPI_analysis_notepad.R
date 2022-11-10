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
##### SET UP SOME THINGS #####
source(file = "1_load_cpi_data.R")

#load("data/cpi_data.RData")

##### REPLICATION CODE ### STARTING HERE #### SPECIAL GRAPHIC

cpi <- cpi_data %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  filter(area_code == "0000") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
  mutate(AddedW3 = Wchange1 + lag(Wchange1,1) + lag(Wchange1,2)) %>%
  mutate(AddedW12 = AddedW3 + lag(Wchange1,3) + lag(Wchange1,4) + lag(Wchange1,5)) %>%
  mutate(AddedW12 = AddedW12 + lag(Wchange1,6) + lag(Wchange1,7) + lag(Wchange1,8)) %>%
  mutate(AddedW12 = AddedW12 + lag(Wchange1,9) + lag(Wchange1,10) + lag(Wchange1,11)) %>%
  mutate(Pchange3 = (value/lag(value, 3)-1)) %>%
  mutate(Wchange3 = (Pchange3*weight)/100) %>%
  mutate(Wchange3a = (1 + Wchange3)^4 - 1) %>%
  mutate(Pchange6 = (value/lag(value, 6)-1)) %>%
  mutate(Wchange6 = (Pchange3*weight)/100) %>%
  mutate(Wchange6a = (1 + Wchange3)^2 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100) %>%
  mutate(Wchange1 = Wchange1*100) %>%
  ungroup()


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
cpi %>% filter(item_name %in% food_index) %>% filter(date == "2021-09-01" | date == "2022-01-01" | date == max(date)) %>%
  mutate(date = paste(as.character(month(date, label = TRUE, abbr = FALSE)), ", ", as.character(year(date)), sep = "")) %>%
  mutate(date = as.factor(date), name = reorder_within(item_name, Pchange3, date))  %>%
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
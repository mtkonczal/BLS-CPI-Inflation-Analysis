# This is a place to work on various analysis that don't fit into a specific project.
# Data here will come and go, like a notepad.
# Written by: Mike Konczal
# Last Updated: 3-12-2022

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/")
library(janitor)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(huxtable)
library(scales)
##### SET UP SOME THINGS #####
#source(file = "1_load_cpi_data.R")
source(file = "2_load_helper_functions.R")

load("data/cpi_data.RData")

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



MaxMonth <- format(gcpi$date[1], "%b")
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
        plot.title = element_textbox(color = "#1002a4", face = "italic",  family=font, size = 28.5,  hjust = 0.5, padding = margin(18, 14, 18, 14)),
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
  geom_text(aes(x=g2_item_name, y=Wchange3a, label=Name3Label), color = "#6d0000", nudge_x=-0.3, size=4) +
  geom_text(aes(x=g2_item_name, y=Wchange12, label=Name12Label), color = "#b87e5e", nudge_x=-0.3, size=4)          

#####

item_basket_LM <- c("All items", "Shelter", "Motor fuel", "Food")
LM <- cpi %>% filter(item_name %in% item_basket_LM) %>%
  filter(date >= "2014-01-01")

LM1 <- LM %>% filter(item_name == "All items")
LM2 <- LM %>% filter(item_name != "All items")

LM2 <- LM2 %>% group_by(date) %>% mutate(totalW1 = sum(Wchange1)) %>% filter(item_name == "Food") %>% ungroup() %>% select(date, totalW1)
LM2 %>% filter(date <= "2020-01-01") %>% summarize(mean(v))

LM2$v <- LM1$Wchange1
LM2$v <- LM2$v - LM2$totalW1
LM2$v <- (1+LM2$v/100)^12 - 1
LM2$v <- LM2$v*100
ggplot(LM2, aes(date, v)) + geom_line(color="darkred") + theme_minimal() +
  labs(title="Inflation: All Items Minus Shelter, Motor Fuel, and Food",
       x="", y="")


item_basket_core_goods

item_basket_AFU2 <- c("Commodities less food and energy commodities", "Transportation commodities less motor fuel")
AFU2 <- cpi %>% filter(item_name %in% item_basket_AFU2) %>%
  filter(date >= "2020-10-01") %>% mutate(Wchange1a = Wchange1a*100)
ggplot(AFU2, aes(date,Wchange1a,color=item_name)) + geom_line(size=2) + theme_minimal() + theme(legend.position='none') 




###### GRAPHIC 4 - HANDOFF TO SERVICES #####

item_basket_AFU <- c("Services less energy services", "Shelter")
AFU <- cpi %>% filter(item_name %in% item_basket_AFU) %>%
  filter(date >= "2011-01-01")
AFU <- create_basket(cpi, item_basket_AFU, startDate = "2014-01-01", lagM = 1, annualize = TRUE) %>% mutate(values = Services_less_energy_services - Shelter)

AFU %>% filter(date <= "2019-12-01") %>% summarize(mean(values, na.rm = TRUE), sd(values, na.rm=TRUE))
AFU %>% filter(date >= "2021-01-01") %>% summarize(mean(values), sd(values))

ggplot(AFU, aes(x=date, y=values)) +
  geom_line(size=1.25, color="darkred") + bbc_style() +
  labs(subtitle = "Inflation Rate for Core Services Minus Shelter",
       x="", y="", caption = "Values are percent, annualized. Data is BLS, CPI, seasonally adjusted. Author's calculation. @rortybomb\n
       2022 weights throughout. Line is weighted services less energy services minus weighted shelter.") +
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

ggplot(tester3, aes(x=date, y=value, color=item_name)) + geom_line(size=2) + theme_classic() +
  theme(legend.position = "bottom", legend.text = element_text(size=13), axis.text = element_text(size=15), legend.title = element_blank(), plot.subtitle = element_text(size=30), plot.title.position = "plot") +
  labs(x="", y="", subtitle="Inflation for AriZona Iced Tea versus Competitors, 2003=100", caption="BLS, CPI, seasonally adjusted, @rortybomb") +
  scale_y_continuous(limits = c(80,130))


#####


tester <- cpi %>% filter(item_name == "Transportation commodities less motor fuel")
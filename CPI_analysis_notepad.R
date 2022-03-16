# This is a place to work on various analysis that don't fit into a specific project.
# Data here will come and go, like a notepad.
# Written by: Mike Konczal
# Last Updated: 3-12-2022

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/")
library(janitor)
library(tidyverse)
library(ggtext)
library(huxtable)

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
AFU <- create_basket(cpi, item_basket_AFU, startDate = "2011-01-01", annualize = TRUE) %>% mutate(values = Services_less_energy_services - Shelter)


AFU %>% filter(date <= "2019-12-01") %>% summarize(mean(values, na.rm = TRUE), sd(values, na.rm=TRUE))
AFU %>% filter(date >= "2021-01-01") %>% summarize(mean(values), sd(values))

ggplot(AFU, aes(x=date, y=values)) +
  geom_line(size=1.25, color="darkred") + bbc_style() +
  labs(title="Since 2021, Core Services Minus Shelter Has Double the Level, Volatilty, of Previous Trend",
       x="", y="", caption = "Values are percent, annualized. Data is BLS, CPI, seasonally adjusted. Author's calculation. 2021 weights throughout. @rortybomb\n
       Line is weighted services less energy services minus weighted shelter.") +
  geom_hline(yintercept=0, linetype="solid", color = "black", alpha=.8) +
  theme(plot.title = element_textbox(size=22))+
  scale_y_continuous(labels = scales::percent)
#  geom_hline(yintercept=2.5, linetype="dashed", color = "grey45", alpha=.8) +
#  annotate(geom="text", x=as.Date("2016-12-01"), y=-1, label="Core Goods", size=8, color="steelblue") +
#  annotate(geom="text", x=as.Date("2016-12-01"), y=2.8, label="Core Services", size=8, color="darkred") +
#  annotate(geom="text", x=as.Date("2018-11-01"), y=2.8, label="CPI Inflation Target", size=4, color="grey45")
ggsave("graphics/liftoff4.pdf")

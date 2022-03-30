# This script makes four graphics tied to the arguments about the Fed raising rates ('liftoff')
# as debated in March, 2022. Used in the following twitter thread:
# https://twitter.com/rortybomb/status/1504103317127016461
# Written by: Mike Konczal
# Last Updated: 3-16-2022

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/")
library(janitor)
library(tidyverse)
library(ggtext)
library(huxtable)
library(ggrepel)
library(ggthemes)

#Uncomment File 1 to Download data fresh.
#source(file = "1_load_cpi_data.R")

# bbc_style() theme for graphics is in the helper file.
source(file = "2_load_helper_functions.R")

load("data/cpi_data.RData")


# Set up data for analysis:
cpi <- cpi_data %>%
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

# Using a predownloaded unemployment rate file. Just get from FRED if missing.
U <- read_csv("data/UNRATE.csv") %>% filter(DATE >= "2009-01-01") %>% mutate(UNRATE = UNRATE/100)

dartboard <- cpi %>% filter(item_name == "All items less food and energy", date >= "2015-01-01") %>%
  select(date, item_name, Wchange1a) %>% left_join(U, by=c("date"="DATE")) %>%
  filter(date <= "2015-12-01" | date >= "2021-04-01") %>%
  mutate(FL = (date>"2020-01-01")) %>% mutate(dateL = paste(format(date, "%b"), format(date, "%Y")))

###### GRAPHIC 1 - THE NUMBERS AS THEY LOOKED AT BOTH LIFTOFFS, OFF THE EVANS BULLSEYE CHART ######

ggplot(dartboard, aes(UNRATE, Wchange1a, label=dateL, fill=FL)) + geom_point(size=4, pch=21) + bbc_style() +
  geom_text_repel(box.padding = 0.5, segment.alpha = 0, size=4) + labs(title = "geom_text_repel()") +
  geom_hline(yintercept=.025, linetype="dotted", color = "grey44") +
  geom_vline(xintercept=.035, linetype="dotted", color = "grey44") +
  annotate(geom="text", x=.041, y=.028, label="2.5% CPI\nInflation Target", size=4, color="grey44") +
  annotate(geom="text", x=.036, y=.075, label="3.5%\nUnemployment\nRate", size=4, color="grey44") +
  theme(panel.grid.major.y = element_blank(), legend.position='none') +
  theme(axis.title.x = element_text(face="plain", size=18), axis.title.y = element_text(face="plain", size=18), plot.title.position = "plot") +
  labs(title="Compared to Dec 2015 Liftoff, There's Far Less Unemployment, More Inflation", x="Unemployment Rate", y="Core CPI Inflation",
       caption="Seasonally adjusted, inflation is monthly rate, annualized. Author's calculations. @rortybomb\nInspired by Charles Evans's dual mandate bullseye plots.",
       subtitle="") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent)

ggsave("graphics/liftoff1.pdf")


###### GRAPHIC 2: TRANSITORY AS TOLD THROUGH CAR MARKET AND INFLATION ######

item_basket_AFU <- c("All items less food and energy", "Transportation commodities less motor fuel")
AFU <- cpi %>% filter(item_name %in% item_basket_AFU) %>%
  filter(date >= "2020-10-01")

ggplot(AFU, aes(date,Wchange1a,color=item_name)) + geom_line(size=1.5) + bbc_style() + theme(legend.position='none') +
  scale_color_manual(values=c("steelblue","darkred")) +
  labs(title="CPI Core Inflation Remains High Even As Contribution From Autos Falls to Zero",
       x="", y="", caption = "Values are percent, annualized. Data is BLS, CPI, seasonally adjusted. Author's calculation. 2021 weights used throughout. @rortybomb\n
       Core inflation is 'All items less food and energy,' New/Used Cars, Motor Parts is 'Transportation commodities iess motor fuel.'") +
  theme(plot.caption = element_text(size = 12, lineheight = 0.7)) +
  geom_hline(yintercept=0, linetype="solid", color = "black", alpha=.8) +
  geom_hline(yintercept=.025, linetype="dashed", color = "grey45", alpha=.8) +
  annotate(geom="text", x=as.Date("2021-12-01"), y=.062, label="CPI Core Inflation", size=8, color="steelblue") +
  annotate(geom="text", x=as.Date("2021-12-01"), y=.031, label="New/Used Cars,\nMotor Parts", size=8, color="darkred") +
  annotate(geom="text", x=as.Date("2020-11-01"), y=.028, label="CPI Inflation Target", size=4, color="grey45") +
  scale_y_continuous(labels = scales::percent) + scale_x_date(date_labels = "%b %Y") +
  theme(plot.title = element_text(face = "plain"))
ggsave("graphics/liftoff2.pdf")


###### GRAPHIC 3: CORE GOODS AND SERVICES ######

item_basket_AFU <- c("Services less energy services", "Commodities less food and energy commodities")
AFU <- cpi %>% filter(item_name %in% item_basket_AFU) %>%
  filter(date >= "2011-01-01")

ggplot(AFU, aes(date,Wchange1a,color=item_name)) + geom_line(size=1.5) + bbc_style() + theme(legend.position='none') +
  scale_color_manual(values=c("steelblue","darkred")) +
  labs(title="Core Goods Are Still Elevated, While Core Services Are Increasing",
       x="", y="", caption = "Values are percent, annualized. Data is BLS, CPI, seasonally adjusted. Author's calculation. 2021 weights used throughout. @rortybomb\nCore Goods is Commodities less food and energy commodities, core services is Services Less Energy Services.") +
  geom_hline(yintercept=0, linetype="solid", color = "black", alpha=.8) +
  geom_hline(yintercept=.025, linetype="dashed", color = "grey45", alpha=.8) +
  annotate(geom="text", x=as.Date("2016-12-01"), y=-0.013, label="Core Goods", size=8, color="steelblue") +
  annotate(geom="text", x=as.Date("2016-12-01"), y=.028, label="Core Services", size=8, color="darkred") +
  annotate(geom="text", x=as.Date("2011-11-01"), y=.028, label="CPI Inflation Target", size=4, color="grey45") +
  scale_y_continuous(labels = scales::percent)
ggsave("graphics/liftoff3.pdf")

# Some summary statistics
AFU %>% mutate(post_2021 = (date >= "2021-01-01")) %>% group_by(post_2021, item_name) %>% summarize(mean(Wchange1a), sd(Wchange1a))

###### GRAPHIC 4: INFLATION RETURNING TO THE 2009 TREND?!? ######

long_plot <- cpi_data %>%
  filter(item_name == "All items less food and energy") %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  filter(date >= "2006-01-01") %>%
  # Probably an easier way to do the following projections past dates, may revisit someday
  mutate(logical2009 = (date >= "2008-01-01")) %>%
  mutate(value2009 = 1.025^(cumsum(logical2009)/12)) %>%
  mutate(value2009 = logical2009*value2009*value[date=="2008-01-01"]) %>%
  mutate(logical2020 = (date >= "2020-01-01")) %>%
  mutate(value2020 = 1.025^(cumsum(logical2020)/12)) %>%
  mutate(value2020 = logical2020*value2020*value[date=="2020-01-01"])

long_plot$final_value2 <- na_if(long_plot$value2020, 0)
long_plot$final_value <- na_if(long_plot$value2009, 0)

ggplot(long_plot, aes(x=date)) + 
  geom_line(aes(y = value), colour = "#007f7f", size = 1.5) + 
  geom_line(aes(y = final_value), color="steelblue", linetype="dashed", size=1) +
  geom_line(aes(y = final_value2), color="steelblue", linetype="dashed", size=1)  +
  bbc_style() +
  labs(title = "Wait, Did the Fed Clarify Which Trend the Target is Flexible Around?",
       subtitle = "If CPI core inflation continues at last three months' average, we'll cross 2009 CPI path in June 2023",
       caption = "Projected CPI inflation is 2.5% annual at a monthly rate. Seasonally adjusted. Author's calculation. @rortybomb\nCore inflation is 'All items less food and energy.'",
       x="", y="") +
  theme(axis.title.y = element_text(face="plain", size=18)) +
  expand_limits(y = 200)

ggsave("graphics/liftoff4.pdf")

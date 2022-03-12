# This script makes three graphics tied to the arguments about the Fed raising rates ('liftoff')
# as debated in March, 2022.
# Written by: Mike Konczal

setwd("/Users/mkonczal/Documents/GitHub/Inflation-Analysis/")
library(janitor)
library(tidyverse)
library(ggtext)
library(huxtable)
library(ggrepel)
library(ggthemes)

#Uncomment File 1 to Download data fresh.
#source(file = "1_load_cpi_data.R")
source(file = "2_load_helper_functions.R")

load("data/cpi_data.RData")


#SET UP DATA:
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

U <- read_csv("data/UNRATE.csv") %>% filter(DATE >= "2009-01-01")

dartboard <- cpi %>% filter(item_name == "All items less food and energy", date >= "2016-01-01") %>%
  select(date, item_name, Wchange1a) %>% left_join(U, by=c("date"="DATE")) %>%
  filter(date <= "2016-12-01" | date >= "2021-10-01") %>% mutate(Wchange1a = Wchange1a*100) %>%
  mutate(FL = (date>"2020-01-01")) %>% mutate(dateL = paste(format(date, "%b"), format(date, "%Y")))

###### NOT USED GRAPHIC - THE GRAPHIC WITHOUT LABELS, AS A DARTBOARD ###### 

ggplot(dartboard, aes(UNRATE, Wchange1a, label=dateL, fill=FL)) + geom_point(size=10, pch=21) + theme_void() +
  annotate("path",
           x=3.5+0.5*cos(seq(0,2*pi,length.out=200)),
           y=2.5+0.5*sin(seq(0,2*pi,length.out=200)),color="darkgreen", size=4) +
  annotate("path",
           x=3.5+0.1*cos(seq(0,2*pi,length.out=20)),
           y=2.5+0.1*sin(seq(0,2*pi,length.out=20)),color="darkred", size=4) +
  geom_hline(yintercept=2.5, linetype="solid", color = "black", alpha=.4) +
  geom_vline(xintercept=3.5, linetype="solid", color = "black", alpha=.4) +
  theme(panel.grid = element_blank(), legend.position='none', plot.title = element_textbox(color = "#1002a4", face = "italic",  size = 24, padding = margin(18, 14, 18, 14)) ) +
  xlim(-3,10) + ylim(-2.5, 6.5) + labs(title=" Any advice for these two darts players?", x="", y="")

###### GRAPHIC 1 - THE NUMBERS AS THEY LOOKED AT BOTH LIFTOFFS, OFF THE EVANS BULLSEYE CHART ######

ggplot(dartboard, aes(UNRATE, Wchange1a, label=dateL, fill=FL)) + geom_point(size=4, pch=21) + theme_minimal() +
  geom_text_repel(box.padding = 0.6, segment.alpha = 0) + labs(title = "geom_text_repel()") +
  annotate("path",
           x=3.5+0.1*cos(seq(0,2*pi,length.out=100)),
           y=2.5+0.1*sin(seq(0,2*pi,length.out=100)),color="darkgreen", size=1) +
  geom_hline(yintercept=2.5, linetype="solid", color = "black", alpha=.4) +
  geom_vline(xintercept=3.5, linetype="solid", color = "black", alpha=.4) +
  theme(panel.grid = element_blank(), legend.position='none') +
  labs(title="Core CPI Inflation versus Unemployment Rate", x="Unemployment Rate", y="", caption="Monthly, Seasonally-Adjusted, Annualized for Inflation. @rortybomb") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        plot.title = element_textbox(color = "#1002a4", face = "italic",  size = 26,  hjust = 0.5, padding = margin(18, 14, 18, 14)),
        axis.text.x = element_text(size = 12, color = "#1002a4"),
        axis.title.x = element_text(size = 16, color = "#1002a4"),
        axis.text.y = element_text(size = 14, color="#1002a4"),
        plot.caption = element_text(size = 12, face="italic", lineheight = 0.7))
ggsave("graphics/liftoff1.pdf")

###### NOT USED GRAPHIC - THE GRAPHIC IN 2021, WATCHING THE EVOLUTION OF THE TRANSITORY DEBATE ######

dartboard2 <- cpi %>% filter(item_name == "All items less food and energy", date >= "2021-04-01") %>%
  select(date, item_name, Wchange1a) %>% left_join(U, by=c("date"="DATE")) %>% mutate(Wchange1a = Wchange1a*100) %>%
  mutate(FL1 = 1*(date>"2021-06-01")) %>% mutate(FL2 = 2*(date>"2021-09-01")) %>% mutate(FL = FL1 + FL2) %>% mutate(FL = as.character(FL)) %>%
  mutate(dateL = paste(format(date, "%b"), format(date, "%Y")))

dartboard2$FL <- c(rep("Don't worry, it's transitory", 3), rep("See, told you so", 3), rep ("Well this sucks, what the f",5))
dartboard2$FL <- factor(dartboard2$FL, levels = c("Don't worry, it's transitory", "See, told you so", "Well this sucks, what the f"))

ggplot(dartboard2, aes(UNRATE, Wchange1a, label=dateL, fill=FL)) + geom_point(size=4, pch=21) + theme_minimal() +
  geom_text_repel(box.padding = 0.6, segment.alpha = 0) + labs(title = "geom_text_repel()") +
  annotate("path",
           x=3.5+0.4*cos(seq(0,2*pi,length.out=100)),
           y=2.5+0.4*sin(seq(0,2*pi,length.out=100)),color="darkgreen", size=4) +
  geom_hline(yintercept=2.5, linetype="solid", color = "black", alpha=.4) +
  geom_vline(xintercept=3.5, linetype="solid", color = "black", alpha=.4) +
  theme(panel.grid = element_blank()) +
  labs(title="Core CPI Inflation versus Unemployment Rate", x="Unemployment Rate", y="", caption="Monthly, Seasonally-Adjusted, Annualized for Inflation. @rortybomb") +
  labs(fill = "What I was thinking at each time") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        plot.title = element_textbox(color = "#1002a4", face = "italic",  size = 26,  hjust = 0.5, padding = margin(18, 14, 18, 14)),
        axis.text.x = element_text(size = 12, color = "#1002a4"),
        axis.title.x = element_text(size = 16, color = "#1002a4"),
        axis.text.y = element_text(size = 14, color="#1002a4"),
        plot.caption = element_text(size = 12, lineheight = 0.7, face="italic"),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))

###### TRANSITORY AS TOLD THROUGH CAR MARKET AND INFLATION ######

item_basket_AFU <- c("All items less food and energy", "Transportation commodities less motor fuel")
AFU <- cpi %>% filter(item_name %in% item_basket_AFU) %>%
  filter(date >= "2020-10-01") %>% mutate(Wchange1a = Wchange1a*100)

ggplot(AFU, aes(date,Wchange1a,color=item_name)) + geom_line(size=2) + theme_minimal() + theme(legend.position='none') +
  scale_color_manual(values=c("steelblue","darkred")) +
  labs(title="Core Inflation Remains High Even As Contribution From Autos Falls to Zero",
       x="", y="", caption = "Values are percent, annualized. Data is BLS, CPI, seasonally adjusted. Author's calculation. 2021 weights throughout. @rortybomb\n
       Core Goods is 'All items less food and energy,' New/Used Cars, Motor Parts is 'Transportation commodities iess motor fuel.'") +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_textbox(color = "#1002a4", face = "italic",  size = 26,  hjust = 0.5, padding = margin(18, 14, 18, 14)),
        plot.subtitle =  element_markdown(size = 19, hjust = 0.5)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        plot.title.position = "plot",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14, hjust = 0.5, color="#1002a4"),
        plot.caption = element_text(size = 12, lineheight = 0.7)) +
  geom_hline(yintercept=0, linetype="solid", color = "black", alpha=.8) +
  geom_hline(yintercept=2.5, linetype="dashed", color = "grey45", alpha=.8) +
  annotate(geom="text", x=as.Date("2021-12-01"), y=6.2, label="Core Inflation", size=8, color="steelblue") +
  annotate(geom="text", x=as.Date("2021-12-01"), y=3.1, label="New/Used Cars,\nMotor Parts", size=8, color="darkred") +
  annotate(geom="text", x=as.Date("2020-11-01"), y=2.8, label="CPI Inflation Target", size=4, color="grey45")
ggsave("graphics/liftoff2.pdf")

###### INFLATION RETURNING TO THE 2009 TREND?!? ######

long_plot <- cpi_data %>%
  filter(item_name == "All items") %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  filter(date >= "2003-01-01") %>%
  mutate(logical2009 = (date >= "2008-01-01")) %>%
  mutate(value2009 = 1.025^(cumsum(logical2009)/12)) %>%
  mutate(value2009 = logical2009*value2009*value[date=="2008-01-01"]) %>%
  mutate(logical2020 = (date >= "2020-01-01")) %>%
  mutate(value2020 = 1.025^(cumsum(logical2020)/12)) %>%
  mutate(value2020 = logical2020*value2020*value[date=="2020-01-01"])

long_plot$final_value2 <- na_if(long_plot$value2020, 0)
long_plot$final_value <- na_if(long_plot$value2009, 0)

ggplot(long_plot, aes(x=date)) + 
  geom_line(aes(y = value), color = "darkred") + 
  geom_line(aes(y = final_value), color="steelblue", linetype="dashed") +
  geom_line(aes(y = final_value2), color="steelblue", linetype="dashed")  +
  theme_minimal() +
  labs(title = "When Your Target is Very Flexible",
       subtitle = "Actual CPI Inflaton Versus Inflation Continued From Recent Recessions (2.5 percent rate)",
       caption = "Projected CPI inflation is 2.5% annual at a monthly rate. Seasonally-adjusted. Author's calculation. @rortybomb",
       x="", y="") +
  theme(
    plot.title.position = "plot",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 22, color="steelblue"),
    plot.subtitle = element_text(size = 18, colour = "darkred", face = "italic"),
    plot.caption = element_text(size = 12, lineheight = 1.2)) +
  expand_limits(y = 175)
ggsave("graphics/liftoff3.pdf")
###############################################################
# Code to read in inflation data from CPS website and begin analysis.
# Requires inflation_weights.csv file as weights aren't stored on download site.
# Mike Konczal
# Last updated 2/18/22
setwd("/Users/mkonczal/Documents/GitHub/Inflation-Analysis/")
library(janitor)
library(tidyverse)
library(ggtext)

############### SECTION 1: READ IN AND CLEAN UP DATA #####################

cpi_data <- read_delim(file = "https://download.bls.gov/pub/time.series/cu/cu.data.0.Current")
cpi_data <- cpi_data %>%
  clean_names()
cpi_data$value <- as.numeric(cpi_data$value)
cpi_data$series_id <- str_trim(cpi_data$series_id)
cpi_data$date <- paste(substr(cpi_data$period, 2,3), "01", substr(cpi_data$year, 3, 4), sep="/")
cpi_data$date <- as.Date(cpi_data$date, "%m/%d/%y")

series <- read_delim(file = "https://download.bls.gov/pub/time.series/cu/cu.series")
series <- series %>%
  clean_names()
series$series_id <- str_trim(series$series_id)

items <- read_delim(file = "https://download.bls.gov/pub/time.series/cu/cu.item")
series <- inner_join(series, items, by = c("item_code"))

cpi_data <- inner_join(cpi_data, series, by = c("series_id"))

# Remove columns we don't need - note may want in the future.
cpi_data <- select(cpi_data, -c("footnote_codes.x", "area_code", "periodicity_code", "base_period", "footnote_codes.y", "begin_year", "begin_period", "end_year", "end_period", "selectable", "sort_sequence", "base_code"))

# Add weight data from seperate csv file, as it's not on the download website.
# NOTE: 2021 weights are added to all years. Future TK to do year by year weighting.
cpi_weights <- read_csv(file = "inflation_weights.csv")
cpi_data <- inner_join(cpi_data, cpi_weights, by = c("item_name"))
rm(series, items, cpi_weights)

################# SECTION 2: CREATE HELPER FUNCTIONS FOR THE ANALYSIS ####################

# Here are some functions to help analyze the data.
# These are mostly necessary to create data frames to draw line graphs or other graphics where the x-axis is date.

# Returns monthly percent change and percent change of basket for item y in the cpi file x.
inflation_data <- function(x, item){
  inf_d <- filter(x, item_name == item)
  inf_d <- arrange(inf_d, date)
  inf_d$pct_change <- (inf_d$value/lag(inf_d$value) - 1) * 100
  inf_d$basket_change <- inf_d$pct_change*inf_d$weight/100
  return(inf_d)
  }

# Takes in a vector of item names (items), cpi_file (x), starting date (YYYY-MM-DD format), with optional end date and seasonality
# and returns each item's contribution to inflation for each month.
create_basket <- function(x, items, startDate, endDate = "current", season = "S", lagM = 1, annualize = FALSE){
  x <- filter(x, period != "M13")
  if(endDate == "current"){
    endDate <- max(x$date)
  }
  x <- x %>%
    filter(seasonal == season) %>%
    filter(date >= startDate) %>%
    filter(date <= endDate)
  x <- arrange(x, date)
  tmp <- filter(x, item_name == items[1])
  cb <- tibble(.rows = nrow(tmp))
  cb[,1] <- tmp$date
  for (i in seq_along(items)) {
    inf_d <- filter(x, item_name == items[i])
    inf_d$pct_change <- (inf_d$value/lag(inf_d$value, lagM) - 1)
    inf_d <- inf_d$pct_change*inf_d$weight/100
    if(annualize == TRUE){
      inf_d <- (1 + inf_d)^(12/lagM) - 1
    }
    cb[,i+1] <- inf_d
  }
  items <- str_replace_all(items, c(' '), c('_'))
  items <- str_replace_all(items, c(','), c(''))
  colnames(cb) <- c('date', items)
  cb %>% clean_names()
  return(cb)
  }

# Takes in a vector of item names and returns their weights.
create_weights <- function(x, items){
  for (i in seq_along(items)) {
    tmp_wght <- filter(x, item_name == items[i])
    if( i == 1)
      wght <- mean(tmp_wght$weight)
    else
      wght <- cbind(wght, mean(tmp_wght$weight))
  }
  if(length(items) == 1){
    names(wght) <- items
  }
  else{
    colnames(wght) <- items
  }
  return(wght)
}

################# SECTION 3: DATA ANALYSIS #####################

################# GRAPHIC 1: SERVICES AND GOODS OVER TIME #####################

item_basket <- c('All items less food and energy', 'Commodities less food and energy commodities', 'Services less energy services')
cpi_line <- create_basket(cpi_data, item_basket, "2015-01-01", annualize = TRUE)
b <- create_weights(cpi_data, item_basket)

cpi_line

# GRAPHIC ONE: # Plot
ggplot()+
  geom_line(data=cpi_line,aes(y=Commodities_less_food_and_energy_commodities, x=date, colour="Core Commodities"), size=1) +
  geom_line(data=cpi_line,aes(y=Services_less_energy_services, x=date, colour="Core Services"),size=1) +
  scale_color_manual(name = "Inflation", values = c("Core Commodities" = "darkgreen", "Core Services" = "red")) +
  geom_hline(yintercept=0) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_markdown()) +
  ggtitle("Monthly Contribution to Core CPI Inflation, <span style = 'color:red;'>Core Services</span> and <span style = 'color:darkgreen;'>Core Commodities</span>, Annualized") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    plot.title.position = "plot",
    axis.ticks.y = element_blank()
  ) +
  theme(legend.position = "none") +
  labs(x ="", y = "")
ggsave("inflation_graphic_1.png")

################# DATA FOR GRAPHIC 2-3 ###########################

item_basket <- cpi_data %>%
  filter(display_level == 0) %>%
  select(item_name)
item_basket <- unique(item_basket)
item_basket <- t(item_basket)

item_basket <- c("All items", "New and used motor vehicles", "Shelter", "Other services", "Medical care services", "Food", "Energy", "Commodities less food and energy commodities")

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
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100)

cpi <- cpi %>%
  filter(item_name %in% item_basket) %>%
  filter(date == max(date))

cpi <- cpi %>%
  ungroup() %>%
  arrange(Wchange1) %>%
  mutate(g2_item_name=factor(item_name, item_name))

MaxMonth <- format(cpi$date[1], "%B")
MaxYear <- format(cpi$date[1], "%Y")

################# GRAPHIC TWO: LAST MONTH'S CONTRIBUTORS ###########################
ggplot(cpi, aes(x=g2_item_name, y=Wchange1a)) +
  geom_segment( aes(x=g2_item_name, xend=g2_item_name, y=0, yend=Wchange1a), color="red") +
  geom_point( color="dark red", size=3) +
  theme_light() +
  coord_flip() +
  xlab("") +
  ylab("Percent") +
  ggtitle(paste("Contribution to CPI for ", MaxMonth, ", ", MaxYear, ", ", "Annualized", sep="")) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot",
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )
ggsave("inflation_graphic_2.png")

################# GRAPHIC THREE: OVER PAST YEAR ###########################
ggplot(cpi) +
  geom_segment( aes(x=g2_item_name, xend=g2_item_name, y=Wchange1a, yend=Wchange12), color="grey") +
  geom_segment( aes(x=g2_item_name, xend=g2_item_name, y=Wchange1a, yend=Wchange3a), color="grey") +  
  geom_point( aes(x=g2_item_name, y=Wchange1a), color="#8b0000", size=3 ) +
  geom_point( aes(x=g2_item_name, y=Wchange3a), color="#ff735b", size=3 ) +
  geom_point( aes(x=g2_item_name, y=Wchange12), color="#bc939c", size=3 ) +
  coord_flip()+
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_markdown()) +
  ggtitle("Contribution to CPI Inflation, <span style = 'color:#8b0000;'>Last Month</span>, <span style = 'color:#ff735b;'>Three Months</span>, and <span style = 'color:#bc939c;'>Twelve Months</span>, all Annualized") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    plot.title.position = "plot",
    axis.ticks.y = element_blank()
  ) +
  xlab("") +
  ylab("Percent") +
  geom_hline(yintercept=0, linetype="solid", color = "black", alpha=0.5)
ggsave("inflation_graphic_3.png")



###### OTHER STUFF

# Try another basket.
item_basket <- c('All items', 'Energy', 'Used cars and trucks', 'New vehicles', 'Motor vehicle parts and equipment')
a <- create_basket(cpi, item_basket, "2019-01-01")
create_weights(cpi, item_basket)

a <- filter(a, date > "2020-12-01")
a <- filter(a, date < "2021-11-01")
sum(a$Energy + a$Used_cars_and_trucks + a$New_vehicles + a$Motor_vehicle_parts_and_equipment) / sum(a$All_items)

b <- create_basket(cpi, item_basket, "2020-12-01", "2021-10-01")
b <- filter(b, !is.na(b[,2]))
sum(a$Energy + a$Used_cars_and_trucks + a$New_vehicles + a$Motor_vehicle_parts_and_equipment) / sum(a$All_items)
a
b

a <- filter(a, date > "2020-12-01")
a <- filter(a, date < "2021-11-01")
sum(a$Energy + a$Used_cars_and_trucks + a$New_vehicles + a$Motor_vehicle_parts_and_equipment) / sum(a$All_items)


# Can summary by 'display levels' - so grab all the highest level.
# Probably some tricks here in displaying that might be useful.
item_basket <- cpi_data %>%
  filter(display_level == 0) %>%
  select(item_name)
item_basket <- unique(item_basket)
item_basket <- t(item_basket)
item_basket
create_basket(cpi, item_basket, "2019-12-01")
create_weights(cpi, item_basket)


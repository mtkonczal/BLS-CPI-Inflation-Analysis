###############################################################
# Code to read in inflation data from CPS website and begin analysis.
# Requires inflation_weights.csv file as weights aren't stored on download site.
# Mike Konczal
# Last updated 2/18/22

library(janitor)
library(tidyverse)


############### SECTION 1: READ IN AND CLEAN UP DATA #####################

cpi <- read_delim(file = "https://download.bls.gov/pub/time.series/cu/cu.data.0.Current")
cpi <- cpi %>%
  clean_names()
cpi$value <- as.numeric(cpi$value)
cpi$series_id <- str_trim(cpi$series_id)
cpi$date <- paste(substr(cpi$period, 2,3), "01", substr(cpi$year, 3, 4), sep="/")
cpi$date <- as.Date(cpi$date, "%m/%d/%y")

series <- read_delim(file = "https://download.bls.gov/pub/time.series/cu/cu.series")
series <- series %>%
  clean_names()
series$series_id <- str_trim(series$series_id)

items <- read_delim(file = "https://download.bls.gov/pub/time.series/cu/cu.item")
series <- inner_join(series, items, by = c("item_code"))

cpi <- inner_join(cpi, series, by = c("series_id"))

# Remove columns we don't need - note may want in the future.
cpi <- select(cpi, -c("footnote_codes.x", "footnote_codes.y", "begin_year", "begin_period", "end_year", "end_period", "selectable", "sort_sequence", "base_code"))

# Add weight data from seperate csv file, as it's not on the download website.
# NOTE: 2021 weights are added to all years. Future TK to do year by year weighting.
cpi_weights <- read_csv(file = "inflation_weights.csv")
cpi <- inner_join(cpi, cpi_weights, by = c("item_name"))


################# SECTION 2: CREATE HELPER FUNCTIONS FOR THE ANALYSIS ####################

# Here are some functions to help analyze the data.

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
create_basket <- function(x, items, startDate, endDate = "current", season = "S"){
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
    inf_d$pct_change <- (inf_d$value/lag(inf_d$value) - 1) * 100
    inf_d <- inf_d$pct_change*inf_d$weight/100
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

# Pick starting year - should add up top! Also keep just with seasonal
cpi_analysis <- cpi

i_all <- inflation_data(cpi_analysis, 'All items')
i_meat <- inflation_data(cpi_analysis, 'Energy')
i_all <- inflation_data(cpi_analysis, 'All items')

item_basket <- c('All items', 'All items less food, shelter, energy, and used cars and trucks')
a <- create_basket(cpi_analysis, item_basket, "2019-01-01")
b <- create_weights(cpi_analysis, item_basket)

a$difference <- a$All_items - a$All_items_less_food_shelter_energy_and_used_cars_and_trucks

# Plot
ggplot()+
  geom_line(data=a,aes(y=All_items,x=date,colour="All items"),size=1 )+
  geom_line(data=a,aes(y=difference,x=date,colour="Food, shelter, energy, used cars"),size=1) +
  geom_line(data=a,aes(y=All_items_less_food_shelter_energy_and_used_cars_and_trucks,x=date,colour="All items minus food, shelter, energy, used cars"),size=1) +
  scale_color_manual(name = "Inflation", values = c("All items" = "darkblue", "Food, shelter, energy, used cars" = "dark green", "All items minus food, shelter, energy, used cars" = "red")) +
  geom_hline(yintercept=0) +
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = 'white', color = 'black')) +
  labs(x ="", y = "monthly inflation, CPI")

# Try another basket.
item_basket <- c('All items', 'Energy', 'Used cars and trucks', 'New vehicles', 'Motor vehicle parts and equipment')
a <- create_basket(cpi_analysis, item_basket, "2019-01-01")
create_weights(cpi_analysis, item_basket)

a <- filter(a, date > "2020-12-01")
a <- filter(a, date < "2021-11-01")
sum(a$Energy + a$Used_cars_and_trucks + a$New_vehicles + a$Motor_vehicle_parts_and_equipment) / sum(a$All_items)

b <- create_basket(cpi_analysis, item_basket, "2020-12-01", "2021-10-01")
b <- filter(b, !is.na(b[,2]))
sum(a$Energy + a$Used_cars_and_trucks + a$New_vehicles + a$Motor_vehicle_parts_and_equipment) / sum(a$All_items)
a
b

a <- filter(a, date > "2020-12-01")
a <- filter(a, date < "2021-11-01")
sum(a$Energy + a$Used_cars_and_trucks + a$New_vehicles + a$Motor_vehicle_parts_and_equipment) / sum(a$All_items)


# Can summary by 'display levels' - so grab all the highest level.
# Probably some tricks here in displaying that might be useful.
item_basket <- cpi_analysis %>%
  filter(display_level == 0) %>%
  select(item_name)
item_basket <- unique(item_basket)
item_basket <- t(item_basket)
item_basket
create_basket(cpi_analysis, item_basket, "2019-12-01")
create_weights(cpi_analysis, item_basket)

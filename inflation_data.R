###############################################################
# Code to read in inflation data from CPS website and begin analysis.
# Requires inflation_weights.csv file as weights aren't stored on download site.
# Mike Konczal
# Last updated 2/18/22
# This was uploaded to Github let's see if it happens.
# I want to see if it works backwards from online to desktop.

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

colnames(cpi)

# Remove columns we don't need - note may want in the future.
cpi <- select(cpi, -c("footnote_codes.x", "footnote_codes.y", "begin_year", "begin_period", "end_year", "end_period", "selectable", "sort_sequence", "base_code"))

# Add weight data from seperate csv file, as it's not on the download website.
# NOTE: 2021 weights are added to all years. Future TK to do year by year weighting.
cpi_weights <- read_csv(file = "inflation_weights.csv")
cpi <- inner_join(cpi, cpi_weights, by = c("item_name"))


################# SECTION 2: CREATE HELPER FUNCTIONS FOR THE ANALYSIS ####################

# Not sure best way to analyze data. Here are some functions to try and find best practices.

# Returns monthly percent change and percent change of basket for item y in the cpi file x.
inflation_data <- function(x, y){
  inf_d <- filter(x, item_name == y)
  inf_d <- arrange(inf_d, date)
  inf_d$pct_change <- (inf_d$value/lag(inf_d$value) - 1) * 100
  inf_d$basket_change <- inf_d$pct_change*inf_d$weight/100
  return(inf_d)
  }

basket_data <- function(x, y){
  inf_d <- filter(x, item_name == y)
  inf_d$pct_change <- (inf_d$value/lag(inf_d$value) - 1) * 100
  inf_d <- inf_d$pct_change*inf_d$weight/100
  return(inf_d)
  }

# Takes in a vector of item names and returns their contributions for inflation.
# Must have more than one variable. Code is messy but works.
create_basket <- function(x, y){
  x <- arrange(x, date)
  for (i in seq_along(y)) {
    if( i == 1){
      tmp <- filter(x, item_name == y[i])
      #Column bind in a for loop is bad practice, will fix TK
      cb <- tmp$date
      cb <- cbind(cb, basket_data(x, y[i]))
    }
    else
      cb <- cbind(cb, basket_data(x, y[i]))
  }
  # Not sure how to keep date formatting, so this is a little rough coding-wise
  cb <- as_tibble(cb)
  tmp <- y[1]
  y <- str_replace_all(y, c(' '), c('_'))
  colnames(cb) <- c('date', y)
  cb %>% clean_names()
  tmp2 <- filter(x, item_name == tmp)
  cb$date <- tmp2$date
  return(cb)
  }

# Takes in a vector of item names and returns their weights.
create_weights <- function(x,y){
  for (i in seq_along(y)) {
    tmp_wght <- filter(x, item_name == y[i])
    if( i == 1)
      wght <- mean(tmp_wght$weight)
    else
      wght <- cbind(wght, mean(tmp_wght$weight))
    }
  colnames(wght) <- y
  return(wght)
}

################# SECTION 3: DATA ANALYSIS #####################

# Pick starting year - should add up top! Also keep just with seasonal
cpi_analysis <- cpi %>%
  filter(year > 2019) %>%
  filter(seasonal == "S")

i_all <- inflation_data(cpi_analysis, 'All items')
i_all <- inflation_data(cpi_analysis, 'Meats')
i_all <- inflation_data(cpi_analysis, 'All items')


item_basket <- c('All items', 'All items less food, shelter, energy, and used cars and trucks')
a <- create_basket(cpi_analysis, item_basket)
b <- create_weights(cpi_analysis, item_basket)

a$difference <- a$All_items - a$`All_items_less_food,_shelter,_energy,_and_used_cars_and_trucks`

# Plot
ggplot()+
  geom_line(data=a,aes(y=All_items,x=date,colour="All items"),size=1 )+
  geom_line(data=a,aes(y=difference,x=date,colour="Food, shelter, energy, used cars"),size=1) +
  geom_line(data=a,aes(y=`All_items_less_food,_shelter,_energy,_and_used_cars_and_trucks`,x=date,colour="All items minus food, shelter, energy, used cars"),size=1) +
  scale_color_manual(name = "Inflation", values = c("All items" = "darkblue", "Food, shelter, energy, used cars" = "dark green", "All items minus food, shelter, energy, used cars" = "red")) +
  geom_hline(yintercept=0) +
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = 'white', color = 'black')) +
  labs(x ="", y = "monthly inflation, CPI")

# Try another basket.
item_basket <- c('All items', 'Energy', 'Food', 'Meats')
create_basket(cpi_analysis, item_basket)
create_weights(cpi_analysis, item_basket)

# Can summary by 'display levels' - so grab all the highest level.
# Probably some tricks here in displaying that might be useful.
item_basket <- cpi_analysis %>%
  filter(display_level == 0) %>%
  select(item_name)
item_basket <- unique(item_basket)
item_basket <- t(item_basket)
item_basket
create_basket(cpi_analysis, item_basket)
create_weights(cpi_analysis, item_basket)





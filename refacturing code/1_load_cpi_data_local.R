###############################################################
# Code to read in inflation data from BLS website and begin analysis.
# This file reads in and store the CPI data.
# Requires inflation_weights.csv file as weights aren't stored on download site.
# Mike Konczal
# Last updated 2/13/2023

# Takes in 2023 weights in a secondary folder, and then chooses which is present.
# Needs a more general solution with previous weights prior to 2022.
# For future update.

library(janitor)
library(tidyverse)
library(httr)
library(data.table)
library(magrittr)

############### SECTION 1: READ IN AND CLEAN UP DATA #####################

cpi_data <- read_delim("../../../Desktop/cu.data.0.Current")
cpi_data <- cpi_data %>%
  clean_names()
cpi_data$value <- as.numeric(cpi_data$value)
cpi_data$series_id <- str_trim(cpi_data$series_id)
cpi_data$date <- paste(substr(cpi_data$period, 2,3), "01", cpi_data$year, sep="/")
cpi_data$date <- as.Date(cpi_data$date, "%m/%d/%Y")

series <- GET("https://download.bls.gov/pub/time.series/cu/cu.series", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()
series <- series %>%
  clean_names()
series$series_id <- str_trim(series$series_id)

items <- GET("https://download.bls.gov/pub/time.series/cu/cu.item", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()
series <- inner_join(series, items, by = c("item_code"))

area_code <- GET("https://download.bls.gov/pub/time.series/cu/cu.area", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()
series <- inner_join(series, area_code, by = c("area_code"))


cpi_data <- inner_join(cpi_data, series, by = c("series_id"))

# Remove columns we don't need - note may want in the future.
#cpi_data <- select(cpi_data, -c("footnote_codes.x", "area_code", "periodicity_code", "base_period", "footnote_codes.y", "begin_year", "begin_period", "end_year", "end_period", "selectable", "sort_sequence", "base_code"))

# Add weight data from seperate csv file, as it's not on the download website.
# NOTE: 2021 weights are added to all years. Future TK to do year by year weighting.
cpi_weights <- read_csv(file = "weights/inflation_weights.csv") %>% select(-year_weight)

cpi_data <- inner_join(cpi_data, cpi_weights, by = c("item_name"))
cpi_weights <- read_csv(file = "weights/inflation_weights_2023.csv") %>% select(item_name, weight_2023 = weight, year = year_weight)
cpi_data <- left_join(cpi_data, cpi_weights, by = c("item_name", "year"))

cpi_data$weight <- ifelse(!is.na(cpi_data$weight_2023),cpi_data$weight_2023,cpi_data$weight)
rm(series, items, cpi_weights)

#save(cpi_data, file = "data/last_cpi_data.RData")

########
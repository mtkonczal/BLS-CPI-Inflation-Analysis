###############################################################
# Code to read in inflation data from BLS website and begin analysis.
# This file reads in and store the CPI data.
# Requires inflation_weights.csv file as weights aren't stored on download site.
# Mike Konczal
# Last updated 3/12/22

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/")
library(janitor)
library(tidyverse)

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
#cpi_data <- select(cpi_data, -c("footnote_codes.x", "area_code", "periodicity_code", "base_period", "footnote_codes.y", "begin_year", "begin_period", "end_year", "end_period", "selectable", "sort_sequence", "base_code"))

# Add weight data from seperate csv file, as it's not on the download website.
# NOTE: 2021 weights are added to all years. Future TK to do year by year weighting.
cpi_weights <- read_csv(file = "weights/inflation_weights.csv")
cpi_data <- inner_join(cpi_data, cpi_weights, by = c("item_name"))
rm(series, items, cpi_weights)

#save(cpi_data, file = "data/cpi_data.RData")

########
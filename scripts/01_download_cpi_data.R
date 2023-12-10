# This file downloads the CPI files from BLS.gov, formats them,
# and merges them.

# Written by: Mike Konczal
# Last updated: 10/10/2023

# Libraries
library(janitor)
library(tidyverse)
library(httr)
library(data.table)

# Configurations
config <- list(
  user_email = "rortybomb@gmail.com",
  weights_file_path = "weights/inflation_weights.csv",
  weights_2023_file_path = "weights/inflation_weights_2023.csv",
  cpi_data_url = "https://download.bls.gov/pub/time.series/cu/cu."
)

#' Get and Process Data
#' This function retrieves and processes data from a specified endpoint.
download_data <- function(endpoint, base_url, user_email) {
  response <- GET(paste0(base_url, endpoint), user_agent(user_email))
  
  if (http_error(response)) {
    stop("Data could not be downloaded from URL: ", paste0(base_url, endpoint))
  }
  
  data <- content(response, as = "text") %>%
    fread() %>%
    clean_names()
  
  return(data)
}

# SECTION 1: READ IN AND CLEAN UP DATA
main_data = "data.0.Current"
#main_data = "data.1.AllItems"
#main_data = "data.2.Summaries"
endpoints <- c(main_data,"series","item","area")

for(i in endpoints){
  assign(i,download_data(i,config$cpi_data_url,config$user_email))
}

cpi_data <- get(main_data) %>%
  mutate(
    value = as.numeric(value),
    series_id = str_trim(series_id),
    date = as.Date(paste(substr(period, 2,3), "01", year, sep="/"), "%m/%d/%Y")
  )

series <- series %>%
  mutate(series_id = str_trim(series_id))

series <- series %>%
  inner_join(item, by = c("item_code")) %>%
  inner_join(area, by = c("area_code"))

cpi_data <- inner_join(cpi_data, series, by = c("series_id"))

# Add weight data
cpi_weights <- read_csv(file = config$weights_file_path) %>% select(-year_weight)
cpi_data <- inner_join(cpi_data, cpi_weights, by = c("item_name"))

cpi_weights_2023 <- read_csv(file = config$weights_2023_file_path) %>% select(item_name, weight_2023 = weight, year = year_weight)
cpi_data <- left_join(cpi_data, cpi_weights_2023, by = c("item_name", "year"))

cpi_data$weight <- ifelse(!is.na(cpi_data$weight_2023), cpi_data$weight_2023, cpi_data$weight)

# Clean up the environment
rm(series,item,area,cpi_weights,cpi_weights_2023)
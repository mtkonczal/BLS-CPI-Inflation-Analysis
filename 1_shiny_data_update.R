# This updates the distributional data for the Shiny scripts.
# In this directory for now.

# Written by: Mike Konczal
# Created: 10-16-2023
# Last Updated: 10-16-2023
library(tidyverse)
library(lubridate)


#### Do CPI First ####
source("scripts/01_download_cpi_data.R")
source("scripts/02_graphic_scripts.R")

cpi <- create_cpi_changes(cpi_data)

most_cpi_prices <- read_csv("weights/most_prices.csv") %>% filter(lowest == 1)

cpi_density_values <-
  cpi %>% filter(year(date) >= 2011) %>%
  filter(item_name %in% most_cpi_prices$item_name) %>%
  select(date, Pchange1a, Pchange3a, Pchange6a, Pchange12) %>%
  pivot_longer(Pchange1a:Pchange12, names_to = "length_type", values_to = "Pvalues") %>%
  mutate(length_type = case_when(
    length_type == "Pchange1a" ~ "1-month",
    length_type == "Pchange3a" ~ "3-month",
    length_type == "Pchange6a" ~ "6-month",
    length_type == "Pchange12" ~ "12-month",
    TRUE ~ length_type
  )) %>%
  mutate(inflation_type = "CPI")


#### Do PCE Second ####

most_pce_prices <- read_csv("weights/pce_items_lowest.csv") %>%
  filter(level == "Level 4") %>%
  rename(series_code = LineDescription)

BEA_format_date <- function(x) {
  year <- substr(x, 1, 4)
  # Assumes there is only one type of data in the call.
  identifier <- substr(x[1], 5, 5)
  
  if(identifier == "Q") {
    quarter <- substr(x, 6, 6)
    month <- ifelse(quarter == "1", "03",
                    ifelse(quarter == "2", "06",
                           ifelse(quarter == "3", "09", "12")))
  }
  
  if(identifier == "M") {
    month <- substr(x, 6, 7)
  }
  
  FormattedDate <- as.Date(paste(year, month, "01", sep = "-"), "%Y-%m-%d")
  return(FormattedDate)
}

#### Download and format data. No analysis.
load_flat_files <- function(location = "https://apps.bea.gov/national/Release/TXT/", type = "Q"){
  print(Sys.time())
  series <- read_csv(paste(location,"SeriesRegister.txt", sep=""), show_col_types = FALSE) %>% clean_names() %>% rename(series_code = percent_series_code)
  tables <- read_csv(paste(location,"TablesRegister.txt", sep=""), show_col_types = FALSE) %>% clean_names()
  print(paste("Loading ", type," data"))
  data <- read_csv(paste(location,"nipadata",type,".txt", sep=""), show_col_types = FALSE)  %>% clean_names()  %>% rename(series_code = percent_series_code)
  print("Formatting date")
  data$date <- BEA_format_date(data$period)
  print("Formatting tables")
  final_data <- data %>% left_join(series, by="series_code") %>% separate_rows(table_id_line_no, sep = "\\|") %>%
    separate(table_id_line_no, into = c("table_id", "line_no"), sep = ":", remove=FALSE) %>% left_join(tables, by="table_id")
  print(Sys.time())
  return(final_data)
}

NIPA_monthly_data <- load_flat_files(type = "M")

pce_density_values <- NIPA_monthly_data %>%
  filter(table_id == "U20404") %>%
  filter(series_label %in% most_pce_prices$series_code) %>%
  select(date, series_code, series_label, value) %>%
  group_by(series_code) %>%
  mutate(Pchange1a = (value / lag(value,1))^12 - 1,
         Pchange3a = (value / lag(value,3))^4 - 1,
         Pchange6a = (value / lag(value,3))^2 - 1,
         Pchange12 = (value / lag(value,3)) - 1
         ) %>%
  ungroup() %>%
  pivot_longer(Pchange1a:Pchange12, names_to = "length_type", values_to = "Pvalues") %>%
  mutate(length_type = case_when(
    length_type == "Pchange1a" ~ "1-month",
    length_type == "Pchange3a" ~ "3-month",
    length_type == "Pchange6a" ~ "6-month",
    length_type == "Pchange12" ~ "12-month",
    TRUE ~ length_type
  )) %>%
  na.omit() %>%
  select(date, length_type, Pvalues) %>%
  filter(year(date) >= 1970) %>%
  mutate(inflation_type = "PCE")

write_csv(rbind(cpi_density_values,pce_density_values), file = "data/shiny_density_test.csv")

saveRDS(rbind(cpi_density_values,pce_density_values), "data/shiny_density_test.rds")
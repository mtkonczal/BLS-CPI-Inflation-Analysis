# This updates the distributional data for the Shiny scripts.
# In this directory for now.

# Written by: Mike Konczal
# Created: 10-16-2023
# Last Updated: 10-16-2023
library(tidyverse)
library(lubridate)

source("scripts/01_download_cpi_data.R")
source("scripts/02_graphic_scripts.R")

cpi <- create_cpi_changes(cpi_data)

most_prices <- read_csv("weights/most_prices.csv") %>% filter(lowest == 1)

density_test <-
  cpi %>% filter(year(date) >= 2018) %>%
  filter(item_name %in% most_prices$item_name) %>%
  select(date, Pchange3a, Pchange1a, Pchange12) %>%
  pivot_longer(Pchange3a:Pchange1a, names_to = "length_type", values_to = "Pvalues") %>%
  mutate(length_type = case_when(
    length_type == "Pchange1a" ~ "1m change",
    length_type == "Pchange3a" ~ "3m change",
    length_type == "Pchange12" ~ "12m change",
    TRUE ~ length_type
  ))



write_csv(density_test, file = "data/shiny_density_test.csv")


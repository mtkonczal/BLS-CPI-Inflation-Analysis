# This file downloads the CPI files from BLS.gov, formats them,
# and merges them.

# Written by: Mike Konczal
# Last updated: 10/10/2023

# Libraries
library(hrbrthemes)
library(ggrepel)
library(viridis)
library(ggridges)
library(gt)
library(quantmod)
library(broom)

# Download the data:
source("scripts/01_download_cpi_data.R")
# Load functions for drawing graphics:
source("scripts/02_general_graphic_scripts.R")
source("scripts/03_specific_graphic_scripts.R")

cpi <- cpi_data


cpi_items <- c("All items less food and energy", "Commodities less food and energy commodities", "Transportation commodities less motor fuel",
                              "Services less energy services", "Shelter", "Core Goods Minus Autos", "Core Services Minus Shelter",
               "Food", "Energy")


start_year <- 2016
end_year_trendline <- 2019

cpi %>%
  filter(seasonal == "S", year >= start_year, item_name %in% cpi_items) %>%
  group_by(series_id) %>%
  mutate(index = row_number()) %>%
  group_modify(~ {
    # log-linear regression
    model <- lm(log(value) ~ index, data = filter(.x, year <= end_year_trendline))
    augment(model, newdata = .x) %>% mutate(projection = exp(.fitted))
  }) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(date, value)) +
  geom_line(aes(date, projection), linetype = "dashed") +
  facet_wrap(~ item_name, scales = "free")





cpi %>% filter(seasonal == "S", year >= start_year, year <= 2019) %>%
  group_by(series_id) %>%
  mutate(index_projection = row_number()) %>%
  do(augment(lm(log(value) ~ index_projection, data = .), data = cpi %>% filter(seasonal == "S", year >= start_year) %>% group_by(series_id)) %>%
  mutate(projection = exp(.fitted)) %>%
  ungroup() %>%
  ggplot() + 
  geom_line(aes(date, value)) + 
  geom_line(aes(date, projection, linetype="dashed")) +
  facet_wrap(~industry_name, scales = "free")

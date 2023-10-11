# This file downloads the CPI files from BLS.gov, formats them,
# and merges them.

# Written by: Mike Konczal
# Last updated: 10/10/2023

# Libraries

source("scripts/01_download_cpi_data.R")
source("scripts/02_graphic_scripts.R")

cpi <- cpi_data %>%
  filter(period != "M13") %>%
  filter(seasonal == "S")

core_3_6_title <- "Hello I'm testing this 2"
three_six_graphic(cpi,"All items less food and energy","2018-01-01","2020-01-01", title = core_3_6_title, include_3_6 = TRUE, add_above_labels = TRUE, column_alpha = 0.2)

core_3_6_title <- "Hello I'm testing this 2"
three_six_graphic(cpi,"All items less food and energy","2018-01-01","2020-01-01", title = core_3_6_title, include_3_6 = TRUE, add_above_labels = TRUE, column_alpha = 0.2)
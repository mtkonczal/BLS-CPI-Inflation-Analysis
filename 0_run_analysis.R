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

# Download the data:
source("scripts/01_download_cpi_data.R")
# Load functions for drawing graphics:
source("scripts/02_graphic_scripts.R")

cpi <- create_cpi_changes(cpi_data)



# Define the start and end dates for the data range
start_date <- "2010-06-07"
end_date <- "2010-06-14"



View(make_three_six_data(cpi, "All items less food and energy", 6))
#Graphic 1: Overview
core_3_6_title <- "Core Inflation Continues to Fall"
three_six_graphic(cpi, "All items less food and energy", "2018-01-01", "2020-01-01", "2021-01-01",
                  title = core_3_6_title, include_3_6 = TRUE, column_alpha = 0.2,
                  colors = c("3-Month Change" = "#2D779C", "6-Month Change" = "#A4CCCC"))
ggsave("graphics/g1.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graphic 2: Onion Chart
onion_title = "All Signs Looking Great"
start_onion_date <- max(cpi$date) %m-% months(24)
onion_chart(cpi, start_onion_date, title=onion_title)
ggsave("graphics/g2.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graphic 3: Core Goods
goods_minus_used_autos <- subtract_cpi_items(cpi, "2018-01-01",
                                             "Commodities less food and energy commodities",
                                             "Used cars and trucks", rest_name_variable = "Core goods ex used autos")

stacked_graphic(goods_minus_used_autos, unique(goods_minus_used_autos$item_name),start_date = "2020-01-01",
                palette = "Greens", title = "Seven Months of Flat Core Goods Prices - We're Done Here, Right?", date_breaks_length = 12)
ggsave("graphics/g3.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graphic 4: Core Services
subtract_array <- c("Shelter", "Medical care services")
services_breakdown <- subtract_cpi_items(cpi, "2018-01-01", "Services less energy services",
                   subtract_array = subtract_array, add_on_array = "Food away from home")

stacked_graphic(services_breakdown, unique(services_breakdown$item_name),start_date = "2021-01-01",
                title = "No Troubling Trends in Services - Are We Done Here?", date_breaks_length = 12, add_labels = TRUE, palette= "RdPu", legend.position = c(0.25,0.9))
ggsave("graphics/g4.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graphic 5: Food and Energy
stacked_graphic(cpi, c("Energy","Food"),start_date = "2019-01-01",title = "Energy Starts to Fall", date_breaks_length = 12)
ggsave("graphics/energy_food.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graphic 6: Ridgeline Graphic
median_terms <- read_csv("weights/mediancpi_component_table.csv") %>% mutate(item_name = Component)
draw_ridgeline(cpi, median_terms$item_name, title="Price Distribution moved out, is now moving back.")
ggsave("graphics/g5.png", dpi="retina", width = 12, height=14, units = "in")

# Graphic 7: Seasonally Unadjusted
unadjusted_analysis(cpi_data, c(2019,2022,2023), title="Unadjusted sliding into prepandemic values?")
ggsave("graphics/g7.png", dpi="retina", width = 12, height=6.75, units = "in")

source("1940s_core_graphic.R")
source("core_1960s.R")

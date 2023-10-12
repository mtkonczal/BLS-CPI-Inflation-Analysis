# This file downloads the CPI files from BLS.gov, formats them,
# and merges them.

# Written by: Mike Konczal
# Last updated: 10/10/2023

# Libraries
library(hrbrthemes)
library(ggrepel)
library(viridis)
library(ggridges)

source("scripts/01_download_cpi_data.R")
source("scripts/02_graphic_scripts.R")

cpi <- create_cpi_changes(cpi_data)

#Graphic 1: Overview
core_3_6_title <- "Core Inflation Continues to Fall"
three_six_graphic(cpi, "All items less food and energy", "2018-01-01", "2020-01-01", "2019-01-01",
                  title = core_3_6_title, include_3_6 = TRUE, column_alpha = 0.2,
                  colors = c("3-Month Change" = "#2D779C", "6-Month Change" = "#A4CCCC"))

# Graphic 2: Onion Chart
onion_title = "Pop in Shelter Drove Higher Inflation"
start_onion_date <- max(cpi$date) %m-% months(24)
onion_chart(cpi, start_onion_date, title=onion_title)


# Graphic 3: Core Goods
goods_minus_used_autos <- subtract_cpi_items(cpi, "2018-01-01",
                                             "Commodities less food and energy commodities",
                                             "Used cars and trucks", rest_name_variable = "Core goods ex used autos")

stacked_graphic(goods_minus_used_autos, unique(goods_minus_used_autos$item_name),start_date = "2020-01-01",
                palette = "Greens", title = "Six Months of Flat Core Goods Prices", date_breaks_length = 12)

# Graphic 4: Core Services
subtract_array <- c("Shelter", "Medical care services")
services_breakdown <- subtract_cpi_items(cpi, "2018-01-01", "Services less energy services",
                   subtract_array = subtract_array, add_on_array = "Food away from home")

stacked_graphic(services_breakdown, unique(services_breakdown$item_name),start_date = "2020-01-01",
                title = "Testing", date_breaks_length = 12, add_labels = TRUE, palette= "RdPu", legend.position = c(0.25,0.9))

# Graphic 5: Food and Energy
stacked_graphic(cpi, c("Energy","Food"),start_date = "2019-01-01",title = "Energy Starts to Fall", date_breaks_length = 12)
ggsave("graphics/energy_food.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graphic 6: Ridgeline Graphic
median_terms <- read_csv("weights/mediancpi_component_table.csv") %>% mutate(item_name = Component)
draw_ridgeline(cpi, median_terms$item_name, title="Price Distribution moved out, is now moving back.")

# Graphic 7: Seasonally Unadjusted
unadjusted_analysis(cpi_data, c(2019,2022,2023), title="Unadjusted sliding into prepandemic values?")

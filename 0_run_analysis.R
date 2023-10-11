# This file downloads the CPI files from BLS.gov, formats them,
# and merges them.

# Written by: Mike Konczal
# Last updated: 10/10/2023

# Libraries

source("scripts/01_download_cpi_data.R")
source("scripts/02_graphic_scripts.R")

cpi <- create_cpi_changes(cpi_data)

core_3_6_title <- "Hello I'm testing this 2"
three_six_graphic(cpi,"All items less food and energy","2018-01-01","2020-01-01", title = core_3_6_title, include_3_6 = TRUE, add_above_labels = TRUE, column_alpha = 0.2)

core_3_6_title <- "Hello I'm testing this 2"
three_six_graphic(cpi,"All items less food, shelter, energy, and used cars and trucks","2018-01-01","2020-01-01", title = core_3_6_title, include_3_6 = TRUE, add_above_labels = TRUE, column_alpha = 0.2)


stacked_graphic(cpi, c("Energy","Food"),start_date = "2017-01-01",title = "Testing", date_breaks_length = 12)
ggsave("graphics/energy_food.png", dpi="retina", width = 12, height=6.75, units = "in")


onion_chart(cpi, "2018-01-01",title="Hello?")

ggsave("graphics/three_categories.png", dpi="retina", width = 12, height=6.75, units = "in")

  
services_breakdown <- subtract_cpi_items(cpi, "2018-01-01", "Services less energy services",
                   subtract_array = subtract_array, add_on_array = "Food away from home")


stacked_graphic(services_breakdown, unique(services_breakdown$item_name),start_date = "2020-01-01",title = "Testing", date_breaks_length = 12, add_labels = TRUE)

goods_minus_used_autos <- subtract_cpi_items(cpi, "2018-01-01",
                                             "Commodities less food and energy commodities",
                                             "Used cars and trucks")

stacked_graphic(goods_minus_used_autos, unique(goods_minus_used_autos$item_name),start_date = "2020-01-01",title = "Testing", date_breaks_length = 12)




median_terms <- read_csv("weights/mediancpi_component_table.csv") %>% mutate(item_name = Component)

draw_ridgeline(cpi, median_terms$item_name, title="Working? TK")

unadjusted_analysis(cpi_data, c(2019,2022,2023), title="Unadjusted sliding into prepandemic values?")


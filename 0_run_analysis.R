# CPI Analysis
# Written by: Mike Konczal


library(hrbrthemes)
library(ggrepel)
library(viridis)
library(ggridges)
library(gt)
library(govMacroTools)
library(tidyverse)
library(janitor)

# Download the data:
source("scripts/01_download_cpi_data.R")
# Load functions for drawing graphics:
source("scripts/02_general_graphic_scripts.R")
source("scripts/03_specific_graphic_scripts.R")

cpi <- create_cpi_changes(cpi_data)

#Get Core data results in a View Window
#View(
#  make_three_six_data(cpi, "All items less food and energy", 6) %>% select(-above_label, -value, -last_value) %>% arrange(desc(date))
#  )



#Graphic 1: Overview
core_3_6_title <- "Core Inflation Eases in March"
three_six_graphic(cpi, "All items less food and energy", "2018-01-01", "2020-01-01", "2021-01-01",
                  title = core_3_6_title, include_3_6 = TRUE, column_alpha = 0.2,
                  colors = c("3-Month Change" = "#2D779C", "6-Month Change" = "#A4CCCC"))
ggsave("graphics/g1.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graphic 2: Onion Chart
onion_title = "Inflation on Target Across Board"
start_onion_date <- max(cpi$date) %m-% months(30)
onion_chart(cpi, start_onion_date, title=onion_title)
ggsave("graphics/g2.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graphic 3: Core Goods
goods_minus_used_autos <- subtract_cpi_items(cpi, "2018-01-01",
                                             "Commodities less food and energy commodities",
                                             "Transportation commodities less motor fuel", rest_name_variable = "Core goods ex autos")

stacked_graphic(goods_minus_used_autos, unique(goods_minus_used_autos$item_name),start_date = "2020-01-01",
                palette = "Greens", title = "Auto Prices Taking Off Since Trump Took Office", date_breaks_length = 12, legend.position = c(0.7,0.85))
ggsave("graphics/g3.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graphic 4: Core Services
subtract_array <- c("Shelter", "Medical care services", "Transportation services")
services_breakdown <- subtract_cpi_items(cpi, "2018-01-01", "Services less energy services",
                   subtract_array = subtract_array, add_on_array = "Food away from home")

stacked_graphic(services_breakdown, unique(services_breakdown$item_name), start_date = "2022-01-01",
                title = "Transportation Services Drive Decline", date_breaks_length = 12, add_labels = TRUE, palette= "RdPu", legend.position = c(0.85,0.9))
ggsave("graphics/g4.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graphic 5: Food and Energy
stacked_graphic(cpi, c("Energy","Food"),start_date = "2019-01-01",title = "Energy Starts to Fall", date_breaks_length = 12)
ggsave("graphics/energy_food.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graphic 6: Ridgeline Graphic
median_terms <- read_csv("weights/mediancpi_component_table.csv") %>% mutate(item_name = Component)
draw_ridgeline(cpi, median_terms$item_name, title="Price Distribution moved out, is now moving back.")
ggsave("graphics/g5.png", dpi="retina", width = 12, height=14, units = "in")

## Graphic 7: Seasonally Unadjusted
#unadjusted_analysis(cpi_data, c(2019,2022,2023, 2024), title="Unadjusted sliding into prepandemic values?")
#ggsave("graphics/g7.png", dpi="retina", width = 12, height=6.75, units = "in")


# Graphic 8: Versus PCE
cpi_versus_pce(cpi, breaks_value = 24, start_graphic = "2011-01-01", title="The Fed's PCE Target is Historically Lower Than CPI")
ggsave("graphics/cpi_versus_pce.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graphic 9: Versus Housing PCE
housing_cpi_versus_pce(cpi, title="Housing's Weight Drives CPI and PCE Divergence")
ggsave("graphics/cpi_versus_pce_housing.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graphic 10: 9.1 Breakdown, Quarterly
#pce_cpi_divergence_contributions(title="Housing's Weight Driving CPI and PCE Divergence")
#ggsave("graphics/cpi_versus_pce_9.1.png", dpi="retina", width = 12, height=6.75, units = "in")

source("2_january_unadjusted.R")
source("3_trump_tariffs.R")



# transportation services breakdown
subtract_array <- c("Car and truck rental", "Motor vehicle insurance", "Motor vehicle fees", "Public transportation","Motor vehicle maintenance and repair")
services_breakdown <- subtract_cpi_items(cpi, "2018-01-01", "Transportation services",
                                         subtract_array = subtract_array)

stacked_graphic(services_breakdown, unique(services_breakdown$item_name), start_date = "2022-01-01",
                title = "What's Happening in Transportation Services?", date_breaks_length = 12, add_labels = TRUE, palette= "RdPu", legend.position = c(0.6,0.9))
ggsave("graphics/g_ts.png", dpi="retina", width = 12, height=6.75, units = "in")




# Real Wages ----
cpi_rate <- cpi %>% filter(series_id == "CUSR0000SA0") %>%
  select(date, cpi_rate = value)

ahe <- getFRED("CES0500000003", rename_variables = "ahe") %>%
  inner_join(cpi_rate, by="date") %>%
  mutate(real_wages = ahe/cpi_rate,
         real_wages3m = real_wages/lag(real_wages, 3)-1,
         real_wages1m = real_wages/lag(real_wages, 1)-1)


ahe %>%
  filter(year(date) >= 2021) %>%
  ggplot(aes(date, real_wages1m)) + geom_line() +
  theme_classic() +
  geom_hline(yintercept = 0)



cpi %>%
  filter(item_name %in% median_terms$item_name | item_name == "Owners' equivalent rent of residences",
         date %in% c(max(date), max(date) %m-% months(12))) %>%
  select(date, Pchange3, item_name, weight) %>%
  mutate(dateF = as.factor(date)) %>%
  ggplot(aes(Pchange3, fill=dateF)) + geom_density()
  


goods_level <- read_csv("data/core_goods_display_levels.csv") %>%
  filter(display_level == 1)

goods_level

cpi %>% filter(item_name %in% goods_level$subcategory) %>%
  filter(year(date) > 2022) %>%
  ggplot(aes(date, Pchange1, color=item_name)) +
  geom_line() +
  theme_classic() +
  facet_wrap(~item_name) +
  geom_vline(xintercept = as.Date("2024-11-01"))

cpi %>%
  filter(item_name %in% c("Other lodging away from home including hotels and motels", "Airline fares")) %>%
  filter(year(date) >= 2018) %>%
  ggplot(aes(date, Pchange1)) +
  geom_line() +
  facet_wrap(~item_name, scales = "free") +
  theme_classic(base_size = 18) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent) +
  labs(subtitle="1-month percent change.",
       x="",
       y="",
       caption="Mike Konczal")


cpi %>%
  filter(item_name == "Other lodging away from home including hotels and motels") %>%
  select(date, item_name, Pchange1) %>%
  arrange(Pchange1)
fill_single_na_geomean <- function(
  df,
  value_col = value,
  group_col = series_id,
  date_col = date
) {
  value_col <- dplyr::enquo(value_col)
  group_col <- dplyr::enquo(group_col)
  date_col <- dplyr::enquo(date_col)

  df %>%
    dplyr::arrange(!!group_col, !!date_col) %>%
    dplyr::group_by(!!group_col) %>%
    dplyr::mutate(
      .prev = dplyr::lag(!!value_col),
      .next = dplyr::lead(!!value_col),
      # Ensure we only fill if there is exactly 1 NA in the group
      .na_count = sum(is.na(!!value_col)),

      !!rlang::as_name(value_col) := dplyr::if_else(
        is.na(!!value_col) &
          .na_count == 1L &
          !is.na(.prev) &
          !is.na(.next) &
          .prev > 0 &
          .next > 0,
        sqrt(.prev * .next), # More efficient than exp(log()) for just 2 numbers
        as.numeric(!!value_col) # Ensures type consistency
      )
    ) %>%
    dplyr::select(-.prev, -.next, -.na_count) %>%
    dplyr::ungroup()
}

# CPI Analysis
# Written by: Mike Konczal

library(hrbrthemes)
library(ggrepel)
library(viridis)
library(ggridges)
library(gt)
library(tidyusmacro)
library(tidyverse)
library(janitor)
library(lubridate)

# Download the data:
source("scripts/01_download_cpi_data.R")
# Load functions for drawing graphics:
source("scripts/02_general_graphic_scripts.R")
source("scripts/03_specific_graphic_scripts.R")

cpi_backup <- cpi_data

cpi <- cpi_data %>% filter(period != "M13") %>% filter(seasonal == "S")
cpi <- fill_single_na_geomean(cpi)
cpi <- create_cpi_changes(cpi)

#Graphic 1: Overview
core_3_6_title <- "Big Services"
g <- three_six_graphic(
  cpi,
  "All items less food and energy",
  "2018-01-01",
  "2020-01-01",
  "2022-01-01",
  title = core_3_6_title,
  include_3_6 = TRUE,
  column_alpha = 0.2,
  subtitle = "All items less food and energy, seasonally adjusted, boxes are one-month change annualized.",
  colors = c("3-Month Change" = "#2c3254", "6-Month Change" = "#ff8361")
) +
  scale_fill_brewer(palette = "Paired") +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )
ggsave(
  "graphics/g1_core_inflation.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)


#Graphic 1: Overview
core_3_6_title <- "Supercore Low in Recent Months"
g <- three_six_graphic(
  cpi,
  "All items less food, shelter, energy, and used cars and trucks",
  "2018-01-01",
  "2020-01-01",
  "2023-01-01",
  title = core_3_6_title,
  subtitle = "Supercore: All items less food, shelter, energy, and used cars and trucks. Seasonally adjusted.",
  include_3_6 = TRUE,
  column_alpha = 0.2,
  colors = c("3-Month Change" = "#2c3254", "6-Month Change" = "#ff8361")
) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )
ggsave(
  "graphics/supercore.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

# Graphic 2: Onion Chart
onion_title = "CPI Services High in April"
start_onion_date <- "2024-01-01" #max(cpi$date) %m-% months(30)
onion_chart(cpi, start_onion_date, title = onion_title, breaks_length = 6) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )
ggsave(
  "graphics/g2_onion_chart.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

# Graphic 3: Core Goods
goods_minus_used_autos <- subtract_cpi_items(
  cpi,
  "2018-01-01",
  "Commodities less food and energy commodities",
  "Transportation commodities less motor fuel",
  rest_name_variable = "Core goods ex autos"
)

stacked_graphic(
  goods_minus_used_autos,
  unique(goods_minus_used_autos$item_name),
  start_date = "2020-01-01",
  palette = "Greens",
  title = "Auto Prices Taking Off Since Trump Took Office",
  date_breaks_length = 12,
  legend.position = c(0.7, 0.85)
) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )
ggsave(
  "graphics/g3_core_goods_breakdown.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

# Graphic 4: Core Services
subtract_array <- c(
  "Shelter",
  "Medical care services",
  "Transportation services"
)
services_breakdown <- subtract_cpi_items(
  cpi,
  "2018-01-01",
  "Services less energy services",
  subtract_array = subtract_array,
  add_on_array = "Food away from home"
)

stacked_graphic(
  services_breakdown,
  unique(services_breakdown$item_name),
  start_date = "2022-01-01",
  title = "Transportation Services Drive Increase in January",
  date_breaks_length = 12,
  add_labels = TRUE,
  palette = "RdPu",
  legend.position = c(0.85, 0.9)
) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )
ggsave(
  "graphics/g4_services_breakdown.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

# Graphic 4b: Core Services (split out auto insurance explicitly)
auto_insurance_item <- read_csv(
  "weights/inflation_weights.csv",
  show_col_types = FALSE
) %>%
  filter(str_detect(str_to_lower(item_name), "motor vehicle insurance")) %>%
  distinct(item_name) %>%
  pull(item_name)

if (length(auto_insurance_item) != 1) {
  stop(
    "Expected exactly one auto insurance item in weights/inflation_weights.csv"
  )
}
auto_insurance_item <- auto_insurance_item[[1]]

subtract_array_with_auto <- c(
  "Shelter",
  "Medical care services",
  auto_insurance_item
)

services_breakdown_with_auto_insurance <- subtract_cpi_items(
  cpi,
  "2018-01-01",
  "Services less energy services",
  subtract_array = subtract_array_with_auto,
  add_on_array = "Food away from home"
)

stacked_graphic(
  services_breakdown_with_auto_insurance,
  unique(services_breakdown_with_auto_insurance$item_name),
  start_date = "2022-01-01",
  title = paste0(auto_insurance_item, " Pops in Core Services Breakdown"),
  date_breaks_length = 12,
  add_labels = TRUE,
  palette = "RdPu",
  legend.position = c(0.85, 0.9)
) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )
ggsave(
  "graphics/g4_services_breakdown_with_auto_insurance.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

# Graphic 5: Food and Energy
stacked_graphic(
  cpi,
  c("Energy", "Food"),
  start_date = "2019-01-01",
  title = "Energy Starts to Fall",
  date_breaks_length = 12
) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )
ggsave(
  "graphics/energy_food.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

# Graphic 5b: Energy Contribution to Headline Inflation
energy_headline <- subtract_cpi_items(
  cpi,
  "2022-01-01",
  "All items",
  subtract_array = c("Energy"),
  rest_name_variable = "All items less energy"
)

energy_colors <- c("Energy" = "#E15759", "All items less energy" = "#4E79A7")

stacked_graphic(
  energy_headline,
  c("Energy", "All items less energy"),
  start_date = "2022-01-01",
  title = "Energy On Par With March 2022 Increases",
  date_breaks_length = 6,
  add_labels = TRUE,
  legend.position = c(0.85, 0.9)
) +
  scale_fill_manual(values = energy_colors) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )
ggsave(
  "graphics/energy_contribution_headline.png",
  dpi = "retina",
  width = 16,
  height = 9,
  units = "in"
)

# T: Transportation Services Monthly Change
T <- three_six_graphic(
  cpi,
  "Transportation services",
  "2018-01-01",
  "2020-01-01",
  "2022-01-01",
  title = "Transportation Services Monthly Change",
  subtitle = "Transportation services, seasonally adjusted, boxes are one-month change annualized.",
  include_3_6 = TRUE,
  column_alpha = 0.2,
  colors = c("3-Month Change" = "#2c3254", "6-Month Change" = "#ff8361")
) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )
ggsave(
  "graphics/T.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

# Graphic 6: Ridgeline Graphic
median_terms <- read_csv("weights/mediancpi_component_table.csv") %>%
  mutate(item_name = Component)
draw_ridgeline(
  cpi,
  median_terms$item_name,
  title = "Price Distribution moved out, is now moving back."
)
ggsave(
  "graphics/g5_ridgeline.png",
  dpi = "retina",
  width = 12,
  height = 14,
  units = "in"
)

## Graphic 7: Seasonally Unadjusted
#unadjusted_analysis(cpi_data, c(2019,2022,2023, 2024), title="Unadjusted sliding into prepandemic values?")
#ggsave("graphics/g7.png", dpi="retina", width = 12, height=6.75, units = "in")

# Graphic 10: 9.1 Breakdown, Quarterly
#pce_cpi_divergence_contributions(title="Housing's Weight Driving CPI and PCE Divergence")
#ggsave("graphics/cpi_versus_pce_9.1.png", dpi="retina", width = 12, height=6.75, units = "in")

source("2_january_unadjusted.R")
source("3_trump_tariffs.R")
source("5_percent_3p_growth.R")


# transportation services breakdown
subtract_array <- c(
  "Car and truck rental",
  "Motor vehicle insurance",
  "Motor vehicle fees",
  "Public transportation",
  "Motor vehicle maintenance and repair"
)
services_breakdown <- subtract_cpi_items(
  cpi,
  "2018-01-01",
  "Transportation services",
  subtract_array = subtract_array
)

stacked_graphic(
  services_breakdown,
  unique(services_breakdown$item_name),
  start_date = "2022-01-01",
  title = "What's Happening in Transportation Services?",
  date_breaks_length = 12,
  add_labels = TRUE,
  palette = "RdPu",
  legend.position = c(0.6, 0.9)
)
ggsave(
  "graphics/g_ts.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)


# Real Wages ----
cpi_rate <- cpi %>%
  filter(series_id == "CUSR0000SA0") %>%
  select(date, cpi_rate = value)

ahe_all <- getFRED("CES0500000003", rename_variables = "ahe") %>%
  mutate(series = "All Employees")

ahe_prod <- getFRED("AHETPI", rename_variables = "ahe") %>%
  mutate(series = "Production and Nonsupervisory Workers")

ahe <- bind_rows(ahe_all, ahe_prod) %>%
  inner_join(cpi_rate, by = "date") %>%
  group_by(series) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    real_wages = ahe / cpi_rate,
    real_wages4m = real_wages / lag(real_wages, 4) - 1,
    real_wages1m = real_wages / lag(real_wages, 1) - 1,
    real_wages_Trump = real_wages / real_wages[date == "2025-01-01"] - 1
  ) %>%
  ungroup()


ahe_plot <- ahe %>% filter(year(date) >= 2023)
last_point <- ahe_plot %>%
  group_by(series) %>%
  filter(date == max(date)) %>%
  ungroup()

wage_colors <- c(
  "All Employees" = "#2c3254",
  "Production and Nonsupervisory Workers" = "#ff8361"
)

ahe_plot %>%
  ggplot(aes(date, real_wages_Trump, color = series)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_line(size = 1.2) +
  geom_point(data = last_point, size = 3) +
  geom_text(
    data = last_point,
    aes(label = percent(real_wages_Trump, accuracy = 0.1)),
    hjust = -0.2,
    vjust = 0.5,
    size = 4.5,
    show.legend = FALSE,
    fontface = "bold"
  ) +
  theme_esp() +
  scale_y_continuous(label = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = generate_dates(cpi$date, 6)) +
  scale_color_manual(values = wage_colors) +
  labs(
    title = "No Real Wage Gains For All Employees Under Trump",
    subtitle = "Change in Average Hourly Earnings Divided by Overall CPI, Since January 2025.",
    caption = "Mike Konczal",
    color = NULL
  ) +
  theme(legend.position = "top")
ggsave(
  "graphics/real_wages_Trump.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

# Nobody is Coming Here Portfolio ----
cpi %>%
  filter(
    item_name %in%
      c(
        "Other lodging away from home including hotels and motels",
        "Airline fares"
      )
  ) %>%
  group_by(item_name) %>%
  mutate(
    Pchange4 = value / lag(value, 4) - 1,
    indexN = 100 * value / value[date == "2024-12-01"]
  ) %>%
  ungroup() %>%
  filter(year(date) >= 2023) %>%
  ggplot(aes(date, indexN)) +
  theme_esp() +
  scale_x_date(date_labels = "%b\n%Y", breaks = generate_dates(cpi$date, 6)) +
  geom_line(size = 1.2, color = "navy") +
  facet_wrap(~item_name, scales = "free") +
  labs(
    subtitle = "Price level, December 2024 = 100",
    title = "Hotels and Airfare Prices are Falling",
    x = "",
    y = "",
    caption = "Mike Konczal"
  )
ggsave(
  "graphics/nobody_is_coming_here.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)


# Stop! -----

cpi %>%
  filter(
    item_name %in%
      median_terms$item_name |
      item_name == "Owners' equivalent rent of residences",
    date %in% c(max(date), max(date) %m-% months(12))
  ) %>%
  select(date, Pchange3, item_name, weight) %>%
  mutate(dateF = as.factor(date)) %>%
  ggplot(aes(Pchange3, fill = dateF)) +
  geom_density()


goods_level <- read_csv("data/core_goods_display_levels.csv") %>%
  filter(display_level == 1)

goods_level

cpi %>%
  filter(item_name %in% goods_level$subcategory) %>%
  filter(year(date) >= 2023) %>%
  group_by(item_name) %>%
  mutate(Trump_change = value / value[date == "2025-01-01"]) %>%
  ungroup() %>%
  ggplot(aes(date, Trump_change, color = item_name)) +
  geom_line(show.legend = FALSE) +
  theme_classic(base_size = 18) +
  facet_wrap(~item_name, scales = "free") +
  geom_vline(xintercept = as.Date("2025-01-01")) +
  scale_x_date(date_labels = "%b\n%Y") +
  labs(
    title = "Tariffs not in data yet.",
    subtitle = "Percent change since January 2025, January 2025 = 1. Major goods items.",
    x = "",
    y = "",
    caption = "Mike Konczal"
  ) +
  theme(plot.title.position = "plot")


unrate <- getFRED("unrate") %>% mutate(unrate = unrate / 100)

cpi %>%
  filter(item_name %in% c("All items less food and energy", "All items")) %>%
  group_by(item_name) %>%
  reframe(
    date = date,
    YoY = value / lag(value, 12) - 1,
    YoY_12 = lag(YoY, 12),
    YoY_24 = lag(YoY, 24),
    YoY_36 = lag(YoY, 36)
  ) %>%
  ungroup() %>%
  left_join(unrate, by = "date") %>%
  mutate(
    changeYoY = YoY - YoY_36,
    changeunrate = unrate - lag(unrate, 36),
    sacrifice_ratio_12m = changeunrate / changeYoY
  ) %>%
  filter(date == max(date))


cpi %>% filter(item_name == "Energy") %>% tail() %>% select(weight)

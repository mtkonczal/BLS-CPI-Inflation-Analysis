###############################################################
# Code to read in inflation data from BLS website and begin analysis.
# This file reads in and store the CPI data.
# Requires inflation_weights.csv file as weights aren't stored on download site.
# Mike Konczal
# Last updated 2/13/2023

# Takes in 2023 weights in a secondary folder, and then chooses which is present.
# Needs a more general solution with previous weights prior to 2022.
# For future update.
library(hrbrthemes)
library(janitor)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(huxtable)
library(scales)
library(tidytext)
library(lubridate)
library(x12)
library(purrr)

metros_codes <- c(
  "A104",
  "A210",
  "A212",
  "A213",
  "A214",
  "A311",
  "A421",
  "A425",
  "S11A",
  "S12A",
  "S12B",
  "S23A",
  "S23B",
  "S24A",
  "S24B",
  "S35A",
  "S35B",
  "S35C",
  "S35D",
  "S35E",
  "S37A",
  "S37B",
  "S48A",
  "S48B",
  "S49A",
  "S49B",
  "S49C",
  "S49D",
  "S49E",
  "S49F",
  "S49G")

source(file = "1_load_cpi_data.R")

unique(cpi_data$area_name)

a <- cpi_data %>% filter(area_name == "San Francisco-Oakland-Hayward, CA")
b <- as_tibble(unique(a$series_title))
c <- as_tibble(unique(a$item_name))


write_csv(area_code, "tester.csv")

area_code %>% filter(!substr(area_code,1,1) == 0)

max_date <- as.Date("2023-04-01")

a <- cpi_data %>% filter(area_code %in% metros_codes, periodicity_code == "R") %>%
  filter(!is.na(date)) %>%
  filter(date > "2022-11-01") %>%
  group_by(area_name, date) %>%
  summarize(n = n())


View(a)


min_cat <- cpi_data %>% filter(area_name == "Denver-Aurora-Lakewood, CO", date == "2023-03-01", periodicity_code == "R") %>% filter(!is.na(date))

a <- min_cat %>% group_by(series_title) %>%
  summarize(min_date = min(date), max_date = max(date))
unique(min_cat$item_code)


b <- cpi_data %>% filter(area_name == "Atlanta-Sandy Springs-Roswell, GA") %>% filter(!is.na(date)) %>%
  group_by(date) %>%
  summarize(n = n())


a <- cpi_data %>% filter(area_code %in% metros_codes, periodicity_code == "R", date == "2023-03-01") %>%
  group_by(item_name, seasonal) %>%
  summarize(n())

unique(a$item_name)

a <- cpi_data %>% filter(item_name == "All items less shelter", periodicity_code == "R", date=="2023-04-01") 


a <- cpi_data %>% filter(item_name == "Household furnishings and operations", periodicity_code == "R", seasonal == "U", date > "2022-01-01") %>%
  select(date, item_name, series_title, area_name)
  group_by(date) %>% summarize(n())

unique(a$area_name)

a <- cpi_data %>% filter(!is.na(date), area_code %in% metros_codes) %>%
  arrange(date) %>%
  filter(area_name != lag(area_name)) %>%
  group_by(date) %>%
  summarize(n())

a <- cpi_data %>% filter(area_code %in% metros_codes, periodicity_code == "R", seasonal == "U", date >= "2022-11-01") %>%
  group_by(date, item_name) %>% summarize(n())

a <- cpi_data %>% filter(item_name == "Household furnishings and operations", area_code %in% metros_codes, periodicity_code == "R", seasonal == "U", date > "2022-01-01") %>%
  group_by(area_name, date) %>% summarize(n()) %>%
  ungroup() %>% group_by(area_name) %>% mutate(diff = interval(lag(date), date) %/% months(1))


# So: some metro areas are monthly, others alternate months.
a <-
  cpi_data %>% filter(area_code %in% metros_codes, periodicity_code == "R", seasonal == "U", !is.na(date)) %>%
  group_by(series_title, area_name) %>%
  arrange(date) %>%
  mutate(YoY = value/lag(value,6)-1) %>%
  mutate(diff_date = interval(lag(date), date) %/% months(1)) %>%
#  mutate(YoY = value/value[date==date %m-% months(12)]) %>%
  ungroup() %>%
  group_by(area_name, item_name) %>%
  filter(date == max(date), year(date) == 2023)

a <- cpi_data %>% filter(area_code %in% metros_codes, periodicity_code == "R", seasonal == "U", !is.na(date)) %>%
  filter(date > "2017-01-01") %>%
  select(area_name, date, item_name, value) %>%
  group_by(area_name, item_name) %>%
  arrange(date, area_name, item_name) %>%
  filter(date != lag(date,1)) %>%
  mutate(diff = interval(lag(date), date) %/% months(1)) %>%
  filter(!is.na(diff), diff < 3) %>%
  ungroup() %>%
  group_by(area_name, item_name, diff) %>%
  # This is amazing!
  mutate(YoY = value/lag(value,12/diff[1])-1) %>%
  ungroup() %>%
  group_by(area_name, item_name) %>%
  filter(date == max(date), year(date) == 2023)

df <- a %>% select(YoY, item_name, area_name) %>%
  filter(item_name %in% c("Shelter", "Services less rent of shelter", "Food away from home")) %>%
  pivot_wider(names_from = "item_name",values_from = "YoY")

df_regression <- lm(`Services less rent of shelter` ~ Shelter, data=df)
summary(df_regression)

df_regression <- lm(`Food away from home` ~ Shelter, data=df)
summary(df_regression)

df %>%
  ggplot(aes(Shelter, `Services less rent of shelter`)) + geom_point(size=1.2) + theme_modern_rc(ticks = TRUE) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  labs(title="No statistically significant relationship between shelter inflation and services less rent of shelter",
       subtitle="Year over year change, last month available (Mar-Apr, 2023), seasonally unadjusted, 23 metro areas.",
       caption="Mike Konczal, Roosevelt Institute") +
  theme(plot.title.position = "plot", axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))

ggsave("graphics/metro_less_rent.png", dpi="retina", width = 12, height=12, units = "in")

df %>%
  ggplot(aes(Shelter, `Food away from home`)) + geom_point(size=1.2) + theme_modern_rc(ticks = TRUE) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  labs(title="No statistically significant relationship between shelter inflation and food away from home",
       subtitle="Year over year change, last month available (Mar-Apr, 2023), seasonally unadjusted, 23 metro areas.",
       caption="Mike Konczal, Roosevelt Institute") +
  theme(plot.title.position = "plot", axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))

ggsave("graphics/metro_food.png", dpi="retina", width = 12, height=12, units = "in")


metro_items <- cpi_data %>% filter(area_code %in% metros_codes, periodicity_code == "R", seasonal == "U", !is.na(date)) %>%
  filter(date > "2000-01-01") %>%
  filter(item_name == "All items less shelter" | item_name == "All items") %>%
  select(area_name, date, item_name, value) %>%
  group_by(area_name, item_name) %>%
  arrange(date, area_name, item_name) %>%
  filter(date != lag(date,1)) %>%
  mutate(diff = interval(lag(date), date) %/% months(1)) %>%
  filter(!is.na(diff)) %>%
  ungroup() %>%
  group_by(area_name, item_name, diff) %>%
  # This is amazing!
  mutate(YoY = value/lag(value,12/diff[1])) %>%
  ungroup()

metro_items %>%
  ggplot(aes(date,YoY, color=area_name)) + geom_line() + theme_classic() + facet_wrap(~item_name) +
  theme(legend.position = "none")
  

metro_items %>%
  group_by(date, item_name) %>%
  filter(!is.na(YoY)) %>%
  summarize(min = min(YoY), max = max(YoY)) %>%
  mutate(diff = max-min) %>%
  ggplot(aes(date,diff, color=item_name)) + geom_line() + theme_classic() + facet_wrap(~item_name) +
  theme(legend.position = "bottom")

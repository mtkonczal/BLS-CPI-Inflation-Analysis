# Assume you already ran prep start docs
# single graphic, merge into function calls later
# Konczal
####
graphic_title <- "2023 Disinflation is Like Late 1940s, Not the 1970s"
threshold_naming <- -0.02

#### Function ####
library(quantmod)
prep_FRED_data <- function(x) {
  getSymbols(x, src="FRED")
  df <- get(x)
  df <- as_tibble(data.frame(Date = index(df))) %>%
    bind_cols(setNames(list(as.numeric(df[, x])), x))
  colnames(df) <- tolower(colnames(df))
  return(df)
}

#### Prep U ####
unlevel <- prep_FRED_data("UNEMPLOY")
unrate <- prep_FRED_data("CLF16OV") %>% left_join(unlevel, by="date") %>%
  mutate(unrate = unemploy/clf16ov)
rm(unlevel)

unrate_annual <- unrate %>%
  mutate(year = year(date)) %>%
  filter(year != 2023) %>%
  group_by(year) %>%
  summarize(unrate = mean(unrate))
####

#### food annual series
less_food <- cpi_data %>%
  filter(series_id == "CUUR0000SA0L1") %>%
  filter(period == "M13") %>%
  select(year, value)

last_year_food <- cpi %>%
  filter(series_id == "CUSR0000SA0L1") %>%
  mutate(YoY = value / lag(value, 12) - 1) %>%
  mutate(YoYD = YoY - lag(YoY, 12)) %>%
  filter(year == 2023) %>%
  select(date, value, YoY, YoYD) %>%
  left_join(unrate, by="date") %>%
  mutate(name_value = if_else(YoYD < threshold_naming | date > max(date) %m-% months(3), as.character(format(date, '%b\n%Y')), as.character(NA))) %>%
  mutate(year = year(date)) %>%
  select(year, value, YoY, YoYD, unrate, name_value)


old_food <- read_csv("data/all_items_less_food_1930s_1997.csv") %>%
  filter(period == "M13") %>%
  select(year, value)

food_series <- rbind(old_food, less_food) %>%
  mutate(YoY = value / lag(value, 1) - 1) %>%
  mutate(YoYD = YoY - lag(YoY)) %>%
  left_join(unrate_annual, by="year") %>%
  na.omit()
####

#### Graphic 1 ####
food_series_graphic <- food_series %>%
  mutate(name_value = if_else(YoYD < threshold_naming, as.character(year), as.character(NA)))

food_series_graphic <- rbind(food_series_graphic, last_year_food) %>%
  mutate(max_value = year == 2023,
  max_value = as.factor(max_value))

subtitle <- "Change in year-over-year 'All items less food' CPI inflation from 1 year ago, vs unemployment rate, change < 0. 1948-2023."

food_series_graphic %>%
  filter(YoYD <= 0) %>%
  ggplot(aes(unrate, YoYD, color=max_value, label=name_value)) +
  theme_lass +
  geom_point() +
  geom_text_repel() +
  #geom_line(aes(unrate, predicted), color="#FC8D62") +
  labs(title = graphic_title,
       subtitle = subtitle,
       caption = "BLS, seasonally adjusted. Author's Calculations. Mike Konczal, Roosevelt Institute.",
       x = "Unemployment Rate",
       y = "Change in year-over-year core cpi growth from one year ago") +
  theme(axis.text.x = element_text(size=15, face="bold"),
        axis.text.y = element_text(size=15, face="bold"),
        axis.title.x = element_text(size=15, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size=14, angle = 90, color="white", vjust = 3)) +
  theme(plot.title = element_text(size=16)) +
  scale_x_continuous(label=percent) +
  scale_y_continuous(label=percent) +
  scale_color_brewer(palette="Set2") +
  theme(legend.position = "none")

ggsave("graphics/core_1940s.png", dpi="retina", width = 12, height=6.75, units = "in")
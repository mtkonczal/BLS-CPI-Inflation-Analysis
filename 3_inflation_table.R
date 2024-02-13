library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)
library(viridis)
library(hrbrthemes)
library(janitor)
library(ggrepel)
library(quantmod)
library(gt)

check_items <- c("Services less energy services", "Food", "Energy", "Commodities less food and energy commodities", "All items", "Housing")

a <-
cpi %>% 
  select(date, DataValue = value, PCEweight = weight, LineDescription = item_name, WDataValue_P3 = Wchange3, WDataValue_P6 = Wchange6, WDataValue_P3a = Wchange3a, WDataValue_P6a = Wchange6a) %>%
  filter(LineDescription %in% check_items) %>%
  group_by(LineDescription) %>%
  mutate(before = (DataValue[date == "2019-12-01"]/DataValue[date == "2017-12-01"]) - 1,
         Wbefore = before*PCEweight[date=="2019-11-01"],
         Wbeforea = (1+Wbefore)^(0.5) - 1,
         y2022 = (DataValue[date == "2022-12-01"]/DataValue[date == "2021-12-01"]) - 1,
         Wy2022a = y2022*PCEweight[date=="2022-11-01"]) %>%
  filter(date == max(date))
    
b <- a %>% filter(LineDescription %in% c("Services less energy services", "Housing")) %>%
  select(WDataValue_P3, WDataValue_P6, Wbefore, Wy2022a) %>%
  pivot_wider(names_from = "LineDescription", values_from = WDataValue_P3:Wy2022a) %>%
  clean_names() %>%
  summarize(WDataValue_P3 = w_data_value_p3_services_less_energy_services - w_data_value_p3_housing,
            WDataValue_P6 = w_data_value_p6_services_less_energy_services - w_data_value_p6_housing,
            Wbefore = wbefore_services_less_energy_services - wbefore_housing,
            Wy2022a = wy2022a_services_less_energy_services - wy2022a_housing) %>%
  mutate(WDataValue_P3a = (1+WDataValue_P3)^4-1,
         WDataValue_P6a = (1+WDataValue_P6)^2-1,
         Wbeforea = (1+Wbefore)^(0.5) - 1,
         LineDescription = "5 - Core Non-Housing Services") %>%
  relocate(LineDescription)

table <- a %>%
  select(LineDescription, WDataValue_P3, WDataValue_P6, Wbefore, Wy2022a, WDataValue_P3a, WDataValue_P6a, Wbeforea) %>%
  rbind(b) %>%
  select(LineDescription, WDataValue_P6a, Wy2022a, Wbeforea) %>%
  filter(LineDescription != "Services less energy services") %>%
  mutate(LineDescription = case_when(
    LineDescription == "All items" ~ "1 - Total CPI Inflation",
    LineDescription == "Food" ~ "2 - Food",
    LineDescription == "Energy" ~ "3 - Energy",
    LineDescription == "Commodities less food and energy commodities" ~ "4 - Core Goods",
    LineDescription == "Housing" ~ "6 - Housing",
    .default = LineDescription
  )) %>%
  arrange(LineDescription) %>%
  mutate(LineDescription = substr(LineDescription, 5, nchar(LineDescription))) %>%
  ungroup()
  
chart_date <- format(max(pce$date, na.rm = TRUE), "%B %Y")

table %>%
  gt(rowname_col = "LineDescription") %>%
  tab_header(
    title = md(paste0("**Breakdown of Weighted Contribution to ", chart_date, " CPI Inflation**")),
    subtitle = "All data annualized"
  ) %>%
  cols_label(
    WDataValue_P6a = "Past 6 Months",
    Wy2022a = "2022",
    Wbeforea = "2018-2019"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  fmt_percent(
    columns = c(WDataValue_P6a, Wy2022a, Wbeforea)
  ) %>%
  tab_footnote(
    footnote = "Source: [your data source]",
    locations = cells_column_labels(columns = vars(LineDescription))
  ) %>%
  opt_stylize(style = 6, color = "blue") %>%
  tab_source_note(
    source_note = "Total +/- ~0.2% due to rounding. BEA, Author's Analysis. Mike Konczal, Roosevelt Institute."
  ) %>%
  tab_style(
    style = list(
      cell_borders(sides = "bottom", color = "black", weight = px(2))
    ),
    locations = cells_body(rows = 1)
  ) %>%
  gtsave(., filename="graphics/inflation_chart.png")


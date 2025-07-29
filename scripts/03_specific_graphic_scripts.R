# This file downloads the CPI files from BLS.gov, formats them,
# and merges them.

# Written by: Mike Konczal
# Last updated: 10/10/2023

# Libraries
library(bea.R)
library(lubridate)
library(hrbrthemes)
library(ggrepel)
library(viridis)
library(ggridges)
library(scales)
library(quantmod)
library(gt)


housing_cpi_versus_pce <- function(cpi_df, compare_length = 24, start_graphic = "2014-01-01", breaks_value = 24, title = "",
                                   legend.position.c = c(0.35, 0.65), colors = c("CPI Housing Contribution to Inflation" = "#BA68C8", "PCE Housing Contribution to Inflation" = "#4F97D7"),
                                   label_length = 6,
                                   column_alpha = 1) {
  cpi_housing <- cpi_df %>%
    filter(item_name == "Housing") %>%
    select(date, cpi_inflation = Wchange1a)

  pce_housing <- read_csv("data/housing_pce_Jan_2024.csv") %>% select(date, pce_inflation = WDataValue_P1a)

  cpi_df <- cpi_housing %>%
    inner_join(pce_housing, by = "date") %>%
    na.omit() %>%
    pivot_longer(cpi_inflation:pce_inflation, names_to = "time_length", values_to = "change") %>%
    mutate(
      time_length = case_when(
        time_length == "cpi_inflation" ~ "CPI Housing Contribution to Inflation",
        time_length == "pce_inflation" ~ "PCE Housing Contribution to Inflation",
        TRUE ~ time_length
      )
    )
  breaks_value <- generate_dates(cpi_housing$date, breaks_value)

  # Plot
  p <- cpi_df %>%
    filter(date >= start_graphic) %>%
    ggplot(aes(date, change, color = time_length)) +
    geom_line(size = 1.6) +
    labs(
      x = "", y = "",
      title = title,
      subtitle = "PRELIMINARY: Contribution to inflation, housing, one-month annualized, CPI versus PCE.",
      caption = "CPI: Housing. 2022 weights prior to 2023. PCE: Housing. NIPA Tables 2.4.4U and 2.4.5U, weights approximated as nominal consumption shares as a percent of the total.\nAuthor's calculations. Mike Konczal, Roosevelt Institute."
    ) +
    theme_esp() +
    scale_color_manual(values = colors) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(date_labels = "%b\n%Y", breaks = breaks_value) +
    theme(
      panel.grid.major.y = element_line(size = 0.5),
      legend.position = legend.position.c,
      legend.text = element_text(size = 15)
    )
  return(p)
}

get_NIPA_data <- function(beaKey, TableName, Frequency, Year, data_set_name = 'NIPA'){
  NIPA_request <- list(
    'UserID' = beaKey ,
    'Method' = 'GetData',
    'datasetname' = data_set_name,
    'TableName' = TableName,
    'Frequency' = Frequency,
    'Year' = Year,
    'ResultFormat' = 'json'
  );
  NIPA_data <- beaGet(NIPA_request, asWide = FALSE)
  return(NIPA_data)
}


BEA_date_quarterly <- function(x){
  x <- x %>%
    mutate(year = substr(TimePeriod, 1, 4)) %>%
    mutate(quarter = substr(TimePeriod, 5,6)) %>%
    mutate(month = case_when(
      quarter == "Q1" ~ 3,
      quarter == "Q2" ~ 6,
      quarter == "Q3" ~ 9,
      quarter == "Q4" ~ 12))
  x$date <- paste(x$month, "01", x$year, sep="/")
  x$date <- as.Date(x$date, "%m/%d/%Y")
  x <- x %>% select(-month, -quarter, -year)
  return(x)
}

pce_cpi_divergence_contributions <- function(title = "", start_date = "2010-01-01", date_breaks_length = 24, legend.position=c(0.50,0.85)){
  beaKey <- read_csv("/Users/mkonczal/Documents/data_folder/BEA_key/BEA_key.csv")
  beaKey <- as.character(beaKey)
  
  beaR_dates <- '2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024'

  reconcile_df <- get_NIPA_data(beaKey, 'U90100', 'Q', beaR_dates, data_set_name = 'NIUnderlyingDetail')
  reconcile_df <- BEA_date_quarterly(reconcile_df)
  
  reconcile <- reconcile_df %>%
    filter(LineNumber %in% c(2,13,14, 21,28,32)) %>%
    select(date, LineDescription, DataValue) %>%
    pivot_wider(names_from = LineDescription, values_from = DataValue) %>%
    clean_names() %>%
    mutate(less_formula_effect_percentage_points = -1*less_formula_effect_percentage_points,
           less_weight_effect_percentage_points = -1*less_weight_effect_percentage_points,
           rent_of_shelter = -1*rent_of_shelter,
           less_scope_effect_pce_price_index_items_out_of_scope_of_the_cpi_percentage_points = -1*less_scope_effect_pce_price_index_items_out_of_scope_of_the_cpi_percentage_points,
           less_other_effects_percentage_points = -1*less_other_effects_percentage_points,
           less_weight_outside_housing_effect_percentage_points = less_weight_effect_percentage_points - rent_of_shelter) %>%
    select(-less_weight_effect_percentage_points) %>%
    pivot_longer(-date, names_to = "type", values_to = "values") %>%
    mutate(
      type = case_when(
        type == "less_formula_effect_percentage_points" ~ "Formula effect",
        type == "rent_of_shelter" ~ "Weight effect: Shelter",
        type == "less_scope_effect_pce_price_index_items_out_of_scope_of_the_cpi_percentage_points" ~ "Scope effect: PCE not in CPI",
        type == "plus_scope_effect_cpi_items_out_of_scope_of_the_pce_price_index_percentage_points" ~ "Scope effect: CPI not in PCE",
        type == "less_other_effects_percentage_points" ~ "Other",
        type == "less_weight_outside_housing_effect_percentage_points" ~ "Weight effect: Non-shelter",
        TRUE ~ type
      )
    )
  
  calculated_total <- reconcile %>% group_by(date) %>%
    summarize(sum_tot = sum(values)/100)
  reconcile <- reconcile %>% mutate(values = values/100, typeF = as.factor(type))
  date_breaks <- generate_dates(reconcile$date, date_breaks_length)
  
 #### ADD A THING THAT SAYS "MAKES CPI HIGHER ABOVE LINE, AND MAKES CPI LOWER BELOW ZERO LINE ####    
  p <- ggplot() +
       geom_bar(data=reconcile, aes(date, values, fill=type), stat = 'identity', size=0) +
       geom_point(data=calculated_total, aes(date, sum_tot), color="darkred", size=3) +
       labs(title=title,
            subtitle="Table 9.1U. Reconciliation of Percent Change in the CPI with Percent Change in the PCE Price Index, quarterly value annualized.\nRed cirlces reflect total difference between headline CPI and PCE.",
            caption="Mike Konczal, Roosevelt Institute") +
    theme_esp() +
    scale_fill_brewer(palette="BrBG") +
    scale_y_continuous(labels = percent) +
    scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks) +
    theme(legend.position = "right", legend.title = element_blank(), legend.text = element_text(size=10),
          axis.text.x = element_text(size=14), axis.text.y = element_text(size=19)) +
    annotate("text", x=as.Date("2015-01-01"), y=0.025, label="Makes CPI Higher", color="white", size=8) +
    annotate("text", x=as.Date("2015-01-01"), y=-0.025, label="Makes CPI Lower", color="white", size=8)
  
  return(p)
     
}
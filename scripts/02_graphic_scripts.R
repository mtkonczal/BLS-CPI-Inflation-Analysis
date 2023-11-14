# This file downloads the CPI files from BLS.gov, formats them,
# and merges them.

# Written by: Mike Konczal
# Last updated: 10/10/2023

# Libraries
library(lubridate)
library(hrbrthemes)
library(ggrepel)
library(viridis)
library(ggridges)
library(scales)

create_cpi_changes <- function(cpi_data){
  cpi <- cpi_data %>%
    filter(period != "M13") %>%
    filter(seasonal == "S") %>%
    arrange(date) %>%
    group_by(item_name) %>%
    mutate(Pchange1 = (value/lag(value)-1)) %>%
    mutate(Pchange1a = (1 + Pchange1)^12 - 1) %>%
    mutate(Wchange1 = (Pchange1*weight)/100) %>%
    mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
    mutate(Pchange3 = (value/lag(value, 3)-1)) %>%
    mutate(Pchange3a = (1 + Pchange3)^4 - 1) %>%
    mutate(Wchange3 = (Pchange3*weight)/100) %>%
    mutate(Wchange3a = (1 + Wchange3)^4 - 1) %>%
    mutate(Pchange6 = (value/lag(value, 6)-1)) %>%
    mutate(Pchange6a = (1 + Pchange3)^2 - 1) %>%
    mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
    mutate(Wchange12 = (Pchange12*weight)/100) %>%
    ungroup()
  return(cpi)
}

calculate_trend <- function(data, variable_name, start_date, end_date) {
  trend <- data %>%
    filter(date == start_date | date == end_date, item_name == variable_name) %>%
    arrange(date) %>%
    mutate(
      num_months = interval(lag(date, 1), date) / months(1),
      trend = value / lag(value, 1)
    ) %>%
    summarize(trenda = trend^(12/num_months) - 1) %>%
    filter(!is.na(trenda)) %>%
    pull(trenda)
  
  return(as.numeric(trend))
}

library(lubridate)

# Generate 100 dates, starting from the maximum date in the input vector and going back X months at a time
generate_dates <- function(dates, X) {
  max_date <- max(dates)
  
  # Generate a sequence of 100 dates, each X months before the previous
  generated_dates <- seq(max_date, length.out = 100, by = paste0("-", X, " months"))
  
  return(generated_dates)
}

make_three_six_data <- function(cpi_df, variable_name, label_length) {
  cpi_df <- cpi_df %>% 
    filter(item_name == variable_name) %>%
    select(item_name, date, value) %>%
    mutate(
      OneMonth = (value/lag(value,1))^12-1,
      ThreeMonth = (value/lag(value,3))^4-1,
      SixMonth = (value/lag(value,6))^2-1,
      above_label = ifelse(date >= max(date) %m-% months(label_length), round(100*OneMonth, 1), NA)
    ) %>%
    pivot_longer(cols = c(ThreeMonth, SixMonth),
                 names_to = "time_length",
                 values_to = "change") %>%
    mutate(
      last_value = ifelse(date==max(date),change,NA),
      time_length = case_when(
        time_length == "ThreeMonth" ~ "3-Month Change",
        time_length == "SixMonth" ~ "6-Month Change",
        TRUE ~ time_length
      ),
      OneMonth = if_else(time_length == "3-Month Change", OneMonth, NA_real_)
    )
  return(cpi_df)
}

three_six_graphic <- function(cpi_df, variable_name, start_date, end_pre_date, start_graphic, breaks_value = 6, title="",
                              legend.position.c = c(0.90,0.85), colors = c("3-Month Change" = "#2D779C", "6-Month Change" = "#A4CCCC"),
                              add_above_labels = FALSE, label_length = 6, include_3_6 = TRUE,
                              column_alpha=1) {
  
  # Data manipulation
  pre_trend <- calculate_trend(cpi_df, variable_name, start_date, end_pre_date)
  cpi_df <- make_three_six_data(cpi_df, variable_name, label_length)
  breaks_value <- generate_dates(cpi_df$date, breaks_value)
  
  # Plot
  p <- cpi_df %>% 
    filter(date >= start_graphic) %>%
    ggplot(aes(date, change, color=time_length,label=label_percent()(round(last_value,3)))) +
    {if(include_3_6)geom_line(size=1.6)} +
    geom_hline(yintercept = pre_trend, linetype="dashed", color="#A4CCCC") +
    geom_col(aes(date, OneMonth), alpha=column_alpha, size=0, show.legend = FALSE) +
    geom_text_repel(
      show.legend=FALSE, nudge_x = 85, min.segment.length = Inf
    ) +
    labs(
      x="", y="",
      title=title,
      subtitle = paste0(
        "Core CPI inflation, monthly percentage change, annualized. ",
        "Dotted line represented 2017 to 2019 value of ", 
        scales::percent(round(pre_trend,3)), " annualized."
      ),
      caption = "All items less food and energy, monthly percent change, BLS, Author's calculations. Mike Konczal, Roosevelt Institute."
    ) +
    theme_lass +
    scale_color_manual(values=colors) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(date_labels = "%b\n%Y", breaks=breaks_value) +
    theme(
      panel.grid.major.y = element_line(size=0.5),
      legend.position = legend.position.c,
      legend.text = element_text(size=15)
    )
  
  if(add_above_labels){
    p <- p + geom_text(aes(x=date, y=OneMonth, label=above_label), nudge_y = 0.003, size=2.7, color="#A4CCCC")
  }
  return(p)
}

# A function to generate a stacked bar chart visualizing the contribution to inflation.
# 
# Args:
#   cpi_data: Data frame containing the CPI data.
#   item_array: Vector of item names to be included in the plot.
#   start_date: Date from which to start plotting.
#   title: Title of the plot (default is NA).
#   date_breaks_length: Length of date breaks (default is 12).
# 
# Returns:
#   A ggplot object.
stacked_graphic <- function(cpi_data, item_array, start_date, title = NA, palette = "RdYlGn", date_breaks_length = 12, add_labels = FALSE, legend.position = c(0.9,0.85)){
  
  date_breaks <- generate_dates(cpi_data$date, date_breaks_length)
  
  cpi_data %>% filter(item_name %in% item_array) %>%
    filter(date >= start_date) %>%
    mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
    ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
    geom_bar(stat = 'identity', size=0) + theme_lass +
    labs(y = NULL,
         x = NULL,
         title = title,
         subtitle = "Monthly Contribution to Inflation, Annualized.",
         caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
    scale_fill_brewer(palette=palette) +
    scale_y_continuous(labels = percent) +
    scale_x_date(date_labels = "%b\n%Y", breaks=date_breaks) +
    {if(add_labels)geom_text(size = 4, position = position_stack(vjust = 0.5), color="black")} +
    theme(legend.position = legend.position, legend.title = element_blank(), legend.text = element_text(size=18),
          axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))
}



onion_chart <- function(cpi_data, start_date, breaks_length = 12, title = NA){
  
  date_breaks <- generate_dates(cpi_data$date, breaks_length)
  
  three_categories <- cpi_data %>%
    filter(item_name %in% c("Services less energy services", "Shelter", "Commodities less food and energy commodities")) %>%
    mutate(item_name = str_replace_all(item_name, "Commodities less food and energy commodities", "Core_goods")) %>%
    select(date, item_name, Wchange1a) %>%
    pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
    mutate(`Rest of core services` = `Services less energy services` - `Shelter`) %>%
    select(-`Services less energy services`) %>%
    pivot_longer(c(Shelter, `Rest of core services`, Core_goods), names_to = "item_name", values_to = "Wchange1a") %>%
    mutate(item_name = str_replace_all(item_name, "Core_goods", "Core goods")) %>%
    mutate(item_name = factor(item_name, levels = c("Core goods", "Shelter", "Rest of core services"))) %>%
    mutate(num_label = round(100 * Wchange1a, 1)) %>%
    mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label))
  
  
  three_trend_2018_2020 <- three_categories %>%
    filter(date <= "2020-01-01" & date >= "2018-01-01") %>%
    select(item_name, Wchange1a) %>%
    group_by(item_name) %>%
    summarize(geometric_mean = (prod(1 + Wchange1a))^(1 / n()) - 1)
  
  three_categories %>%
    left_join(three_trend_2018_2020, by="item_name") %>%
    filter(date >= start_date) %>%
    ggplot(aes(x = date, y = Wchange1a, fill=item_name)) +
    geom_bar(stat = 'identity', size=0) +
    theme(legend.position = "bottom", legend.title = element_blank()) + 
    facet_grid(~item_name) +
    labs(y = NULL,
         x = NULL,
         title = title,
         subtitle = "Monthly contribution to inflation, annualized. Yellow line is 2018-19 geometric mean (which was below target).",
         caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
    theme_lass +
    scale_fill_brewer(palette="RdPu", name = "item_name") +
    scale_y_continuous(labels = percent) +
    scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks) +
    theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=19)) +
    geom_line(aes(date,geometric_mean), color="#E2E47E", size=1.4)
  
}

onion_chart2 <- function(cpi_data, start_date, title = NA){
  
  date_breaks <- generate_dates(cpi_data$date, date_breaks_length)

  three_categories <- cpi_data %>%
    filter(date > start_date, item_name %in% c("Services less energy services", "Shelter", "Commodities less food and energy commodities")) %>%
    mutate(item_name = str_replace_all(item_name, "Commodities less food and energy commodities", "Core_goods")) %>%
    select(date, item_name, Wchange1a) %>%
    pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
    mutate(`Rest of core services` = `Services less energy services` - `Shelter`) %>%
    select(-`Services less energy services`) %>%
    pivot_longer(c(Shelter, `Rest of core services`, Core_goods), names_to = "item_name", values_to = "Wchange1a") %>%
    mutate(item_name = str_replace_all(item_name, "Core_goods", "Core goods")) %>%
    mutate(item_name = factor(item_name, levels = c("Core goods", "Shelter", "Rest of core services"))) %>%
    mutate(num_label = round(100 * Wchange1a, 1)) %>%
    mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label))
  
  three_categories %>%
    ggplot(aes(x = date, y = Wchange1a, fill=item_name)) +
    geom_bar(stat = 'identity', size=0) +
    theme(legend.position = "bottom", legend.title = element_blank()) + 
    facet_grid(~item_name) +
    labs(y = NULL,
         x = NULL,
         title = title,
         subtitle = "Monthly contribution to inflation, annualized.",
         caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
    theme_lass +
    scale_fill_brewer(palette="RdPu", name = "item_name") +
    scale_y_continuous(labels = percent) +
    scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates_three_categories) +
    theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))

}

#####
subtract_cpi_items <- function(cpi, start_date, base_item, subtract_array,
                               add_on_array = NA, rest_name_variable = "Rest of Variable"){

  c1 <- cpi %>% filter(date >= start_date) %>%
    filter(item_name %in% subtract_array) %>%
    select(date, Wchange1a) %>%
    group_by(date) %>%
    summarize(total_rest = sum(Wchange1a))
  
  c2 <- cpi %>%
    filter(item_name == base_item) %>%
    select(date, Wchange1a) %>%
    left_join(c1, by="date") %>%
    mutate(Wchange1a = Wchange1a - total_rest) %>%
    mutate(item_name = rest_name_variable) %>%
    select(date, item_name, Wchange1a)
  
  if(!is.na(add_on_array))
    c3 <- cpi %>%
    filter(item_name %in% add_on_array) %>%
    select(date, item_name, Wchange1a)
  
  c <- cpi %>% filter(date >= start_date) %>%
    filter(item_name %in% subtract_array) %>%
    select(date, item_name, Wchange1a) %>%
    rbind(c2)
  
  if(!is.na(add_on_array))
    c <- c %>% rbind(c3)
  
  c <- c %>%
    arrange(date) %>%
    filter(date >= start_date) %>%
    mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label))
  
  return(c)
}  

draw_ridgeline <- function(cpi, item_list, top_cut = 0.85, bottom_cut = 0.15, title = NA) {

    median_data <- cpi %>%
      filter(item_name %in% item_list | item_name == "Owners' equivalent rent of residences") %>%
      filter(!is.na(date)) %>%
      arrange(date) %>%
      group_by(date) %>%
      mutate(normalized = sum(weight)) %>%
      mutate(weightN = weight / normalized) %>%
      arrange(Pchange3) %>%
      mutate(cumsum = cumsum(weight) / 100) %>%
      mutate(cumsumN = cumsum(weightN)) %>%
      ungroup() %>%
      mutate(Pchange3a = (1 + Pchange3)^4 - 1)
  
  quarters_backwards <- (month(max(cpi$date)) + c(0, 3, 6, 9) - 1) %% 12 + 1
  
    # THIS IS THE GRAPHIC - 30 percent-trimmed distribution
    median_data %>%
      mutate(dateF = as.factor(date)) %>%
      filter(cumsumN <= top_cut & cumsum >= bottom_cut) %>%
      filter(date >= "2017-06-01") %>%
      filter(date != "2020-06-01") %>%
      filter(month(date) %in% quarters_backwards) %>%
      mutate(monthC = format(date, "%B, %Y")) %>%
      mutate(monthC = fct_reorder(monthC, date)) %>%
      mutate(monthCR = fct_rev(monthC)) %>%
      ggplot(aes(x = Pchange3a, y = monthCR, fill = stat(x))) +
      geom_density_ridges_gradient() +
      scale_fill_viridis(option = "H") +
      theme_ridges() +
      theme_lass +
      theme(legend.position = "none") +
      scale_x_continuous(labels = percent) +
      labs(
        title = title,
        subtitle = "Distribution of the Cleveland Fed's Median/Trimmed-Mean CPI price basket, 3-month change annualized, with components\nwhose expenditure weights fall above/below the 85/15th percentile of price changes removed.",
        x = "Three Month Percent Change", y = "", caption = "OER is treated as one value, instead of broken out by region and manually seasonally adjusted as per Cleveland Fed's methodology.Some 2020s removed as negative outlier. Mike Konczal, Roosevelt Institute"
      ) +
      theme(
        plot.title.position = "plot", legend.position = "none", legend.title = element_blank(),
        plot.title = element_text(size = 25, margin = margin(0, 0, 5, 0)),
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(size = 10, face = "italic"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12)
      )
}

unadjusted_analysis <- function(cpi_data, years_array, title = NA) {
  
  cpi_data %>%
    filter(series_id == "CUUR0000SA0L1E") %>%
    select(date, value) %>%
    mutate(month = month(date), year = as.factor(year(date))) %>%
    mutate(pchange1 = value / lag(value, 1) - 1) %>%
    filter(year(date) %in% years_array) %>%
    ggplot(aes(month, pchange1, color = year)) +
    geom_line(size = 1.2) +
    theme_lass +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_y_continuous(labels = percent) +
    geom_text_repel(aes(label = year),
      size = 7,
      data = . %>% group_by(year) %>% filter(month == 4) %>% ungroup()
    ) +
    labs(
      title = title,
      subtitle = "Seasonally unadjusted values for core CPI inflation, 1-month percent change, not annualized.",
      caption = "Inspired by Paul Romer's blog. Mike Konczal, Roosevelt Institute"
    )
}


##### SET UP SOME THINGS #####
theme_lass <-   theme_modern_rc(ticks = TRUE) + theme(legend.position = "none", legend.title = element_blank(),
                                                      panel.grid.major.y = element_line(size=0.5),
                                                      panel.grid.minor.y = element_blank(),
                                                      plot.title.position = "plot",
                                                      axis.title.x = element_blank(),
                                                      axis.title.y = element_blank(),
                                                      plot.title = element_text(size = 25, face="bold"),
                                                      plot.subtitle = element_text(size=15, color="white"),
                                                      plot.caption = element_text(size=10, face="italic"),
                                                      legend.text = element_text(size=12),
                                                      axis.text.y = element_text(size=12, face="bold"),
                                                      axis.text.x = element_text(size=12, face="bold"),
                                                      strip.text = element_text(face = "bold", color="white", hjust = 0.5, size = 10),
                                                      panel.grid.major.x = element_blank(),
                                                      panel.grid.minor.x = element_blank(),
                                                      strip.background = element_blank()) +
  theme(text = element_text(family = "Larsseit"),
        plot.title = element_text(family = "Larsseit"),
        plot.subtitle = element_text(family = "Larsseit"),
        plot.caption = element_text(family="Larsseit"),
        strip.text = element_text(family="Larsseit"))
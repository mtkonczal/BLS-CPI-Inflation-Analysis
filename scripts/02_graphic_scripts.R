# This file downloads the CPI files from BLS.gov, formats them,
# and merges them.

# Written by: Mike Konczal
# Last updated: 10/10/2023

# Libraries
library(lubridate)


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
      last_value = ifelse(date==max(date),SixMonth,NA),
      above_label = ifelse(date >= max(date) %m-% months(label_length), round(100*OneMonth, 1), NA)
    ) %>%
    pivot_longer(cols = c(ThreeMonth, SixMonth),
                 names_to = "time_length",
                 values_to = "change") %>%
    mutate(
      time_length = case_when(
        time_length == "ThreeMonth" ~ "3-Month Change",
        time_length == "SixMonth" ~ "6-Month Change",
        TRUE ~ time_length
      ),
      OneMonth = if_else(time_length == "3-Month Change", OneMonth, NA_real_)
    )
  return(cpi_df)
}

three_six_graphic <- function(cpi_df, variable_name, start_date, end_pre_date, breaks_value = 6, title="",
                              legend.position.c = c(0.90,0.85), colors = c("3-Month Change" = "#2D779C", "6-Month Change" = "#A4CCCC"),
                              add_above_labels = FALSE, label_length = 6, include_3_6 = TRUE,
                              column_alpha=1) {
  
  # Data manipulation
  pre_trend <- calculate_trend(cpi_df, variable_name, start_date, end_pre_date)
  cpi_df <- make_three_six_data(cpi_df, variable_name, label_length)
  breaks_value <- generate_dates(cpi_df$date, breaks_value)
  
  # Plot
  p <- cpi_df %>% 
    filter(date >= start_date) %>%
    ggplot(aes(date, change, color=time_length)) +
    {if(include_3_6)geom_line(size=1.6)} +
    geom_hline(yintercept = pre_trend, linetype="dashed", color="#A4CCCC") +
    geom_col(aes(date, OneMonth), alpha=column_alpha, size=0, show.legend = FALSE) +
    geom_text_repel(
      aes(label=scales::label_percent()(round(last_value,3))),
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
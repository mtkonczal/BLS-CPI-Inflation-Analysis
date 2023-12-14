library(quantmod)
graphic_title="Accelerationist This! Current Disinflation Alongside Low Unemployment is Unprecedented in the Past 60 Years"
months_highlighted = 6

prep_FRED_data <- function(x) {
  getSymbols(x, src="FRED")
  df <- get(x)
  df <- as_tibble(data.frame(Date = index(df))) %>%
    bind_cols(setNames(list(as.numeric(df[, x])), x))
  colnames(df) <- tolower(colnames(df))
  return(df)
}

  unrate <- prep_FRED_data("UNRATE")  %>% mutate(unrate = unrate/100)
  long_cpi <- prep_FRED_data("CPILFESL") %>% mutate(value = cpilfesl)
  
  long_data <- long_cpi %>% #filter(series_label == "PCE excluding food and energy") %>%
    mutate(YoY = value/lag(value,12) - 1,
           YoYD = YoY - lag(YoY,12)) %>%
    select(date, YoY, YoYD) %>%
    na.omit() %>%
    left_join(unrate, by="date") %>%
    mutate(max_value = date < max(date) %m-% months(months_highlighted),
           max_value = as.factor(max_value),
           name_value = if_else(YoYD < YoYD[date == max(date)] | date > max(date) %m-% months(3), as.character(format(date, '%b\n%Y')), as.character(NA)))
  
  subtitle <- paste0("Change in year-over-year core CPI from 1 year ago, vs unemployment rate, change < 0, from ", format(min(long_data$date), "%B %Y"), " to ", format(max(long_data$date), "%B %Y"), ".")
  long_data %>%
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
         y = "Change in year-over-year core PCE growth from one year ago") +
    theme(axis.text.x = element_text(size=15, face="bold"),
          axis.text.y = element_text(size=15, face="bold"),
          axis.title.x = element_text(size=15, margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(size=14, angle = 90, color="white", vjust = 3)) +
    theme(plot.title = element_text(size=16)) +
    scale_x_continuous(label=percent) +
    scale_y_continuous(label=percent) +
    scale_color_brewer(palette="Set2")


ggsave("graphics/core_1960s.png", dpi="retina", width = 12, height=6.75, units = "in")
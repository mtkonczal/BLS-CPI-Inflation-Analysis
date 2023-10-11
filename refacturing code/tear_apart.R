
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

#######
#SET UP DATA:
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
  mutate(Pchange3a = (1 + Pchange3)^12 - 1) %>%
  mutate(Wchange3 = (Pchange3*weight)/100) %>%
  mutate(Wchange3a = (1 + Wchange3)^4 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100) %>%
  ungroup()



##### Graphic1: Core Inflation ####


#### DISTRIBUTION 3 PERCENT ####

lowest <- read_csv("weights/most_prices.csv") %>% filter(category != "Meta", lowest == 1)
core <- lowest %>% filter(category %in% c("Services","Goods"))

MI_dates <- cpi %>% filter(date > "2012-01-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates_three = MI_dates[seq(1, length(MI_dates), 12)]

all <- cpi %>% filter(item_name %in% lowest$item_name, !is.na(Pchange1), date > "2012-01-01") %>% mutate(p3 = (Pchange1a > 0.03)) %>%
  group_by(date) %>%
  summarize(total_3 = sum(p3)/n()) %>% ungroup() %>% ungroup() %>% mutate(type = "All items")


cpi %>% filter(item_name %in% core$item_name, !is.na(Pchange1), date > "2012-01-01") %>% mutate(p3 = (Pchange1a > 0.03)) %>%
  group_by(date) %>%
  summarize(total_3 = sum(p3)/n()) %>% ungroup() %>% ungroup() %>% mutate(type = "Core items") %>%
  rbind(all) %>%
  group_by(type) %>%
  mutate(last_value = ifelse(date==max(date),total_3,NA)) %>%
  ungroup() %>%
  ggplot(aes(date, total_3, color=type, label=label_percent()(last_value))) + geom_line(size=1) + theme_lass +
  labs(y = NULL,
       x = NULL,
       title = "Percent of Items With 3% Price Growth Has Decreased From High Levels",
       subtitle = "Percent of 140 CPI items (82 in core) having at least 3 percent monthly price increases, annualized.",
       caption ="BLS, CPI, only seasonally adjusted items included. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates_three) +
  geom_text(show.legend=FALSE, nudge_x = 125) +
  theme(legend.position = c(0.35,0.8))
  

ggsave("graphics/three_percent_growth.png", dpi="retina", width = 12, height=6.75, units = "in")

cpi %>% filter(item_name %in% lowest$item_name, !is.na(Pchange1), date == max(date) | date == "2019-12-01") %>%
  ggplot(aes(Pchange1a), color="date") + geom_density() + facet_wrap(~date)


##### Boxplots ####

boxplot_dates <- c(max(cpi$date), max(cpi$date) %m-% months(4), "2019-12-01")

df <-  cpi %>% filter(item_name %in% lowest$item_name, !is.na(Pchange1)) %>% filter(date %in% boxplot_dates) %>%
  left_join(lowest, by="item_name") %>% select(item_name, Pchange3, date, category)
# Convert dates to character strings with custom format
df$date_strings <- format(df$date, "%B, %Y")
# Create an ordered factor variable with custom levels
df$date_factor <- ordered(df$date_strings, levels = unique(df$date_strings))
# Print the factor variable

df %>%
  mutate(category = factor(category, levels=c("Goods", "Services", "Food", "Energy"))) %>%
  ggplot(aes(Pchange3, date_factor, fill=date_factor)) + geom_boxplot() + facet_wrap(~category, scales = "free") +
  labs(title="Three month percent change, 141 CPI items", caption="Mike Konczal, Roosevelt Institute, Boxplots Guy", x="", y="") + theme_classic() +
  scale_x_continuous(labels = percent) + theme(legend.position = "none")

ggsave("graphics/boxpots.png", dpi="retina", width = 12, height=6.75, units = "in")

rm(df)


#### Regional Onion ####
regional_area_codes <- c("0100","0200","0300","0400")

cpi_data %>% filter(item_name %in% c("Services less energy services", "Shelter", "Commodities less food and energy commodities"),
                    area_code %in% regional_area_codes, !is.na(date), periodicity_code == "R") %>%
  mutate(item_name = str_replace_all(item_name, "Commodities less food and energy commodities","Core_goods")) %>%
  group_by(series_title, area_name) %>%
  arrange(date) %>%
  mutate(YoY = value/lag(value,12)-1) %>%
  mutate(YoY_W = YoY*weight/100) %>%
  ungroup() %>%
  filter(year(date) >= 2015) %>%
  select(date, item_name, area_name, YoY_W) %>%
  pivot_wider(names_from = item_name, values_from = YoY_W) %>%
  mutate(`Rest of core services` = `Services less energy services` - `Shelter`) %>%
  select(-`Services less energy services`) %>%
  pivot_longer(c(Shelter, `Rest of core services`,Core_goods), names_to = "item_name", values_to = "YoY_W") %>%
  mutate(item_name = str_replace_all(item_name, "Core_goods","Core goods")) %>%
  mutate(item_name = factor(item_name, levels = c("Core goods", "Shelter", "Rest of core services"))) %>%
  mutate(num_label = round(100*YoY_W, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  
  ggplot(aes(x = date, y = YoY_W, color=area_name)) +
  geom_line(size=1) +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  facet_grid(~item_name) +
  labs(y = NULL,
       x = NULL,
       title = "Onion by region, why not?",
       subtitle = "Monthly contribution to inflation, seasonally-unadjusted, year-over-year, by region.",
       caption ="BLS, CPI, seasonally unadjusted, 2022 weights prior to 2023. Shelter category is 'shelter.'  National weights used for subregions. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  scale_color_brewer(palette="Spectral", name = "item_name") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=19)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=25),
        strip.text = element_text(size=20))

ggsave("graphics/three_regional_categories.png", dpi="retina", width = 12, height=6.75, units = "in")


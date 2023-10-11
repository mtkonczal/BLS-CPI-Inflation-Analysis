
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



##### Graphic 2: Goods Versus Services with labels ####
MI_Graphic2a <- cpi %>% filter(item_name %in% c("Services less energy services", "Commodities less food and energy commodities"), date > "2019-01-01") %>%
  mutate(in_range = ifelse(date >= "2021-10-01", value, NA), trend = mean(in_range, na.rm=TRUE),
         trend = ifelse(date >= "2021-10-01", trend, NA))

placement <- MI_Graphic2a %>% select(Wchange1a,date,item_name) %>%
  mutate(Wchange1a2 = if_else(Wchange1a < 0, 0, Wchange1a)) %>%
  group_by(date) %>% summarize(place_y = sum(Wchange1a2)) %>%
  ungroup() %>% mutate(item_name = "Services less energy services")

MI_Graphic2a <- cpi %>% filter(item_name == "All items less food and energy", date > "2021-12-01") %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  select(date, num_label) %>% full_join(MI_Graphic2a, by="date") %>%
  left_join(placement, by=c("date","item_name"))

MI_Graphic2a$item_name <- str_replace_all(MI_Graphic2a$item_name, "Services less energy services", "Core Services")
MI_Graphic2a$item_name <- str_replace_all(MI_Graphic2a$item_name, "Commodities less food and energy commodities", "Core Goods")

MI_dates <- unique(MI_Graphic2a$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates = MI_dates[seq(1, length(MI_dates), 3)]

MI_Graphic2a %>% ggplot(aes(x = date, y = Wchange1a, fill = item_name)) +
  geom_bar(stat = 'identity', size=0) + theme_lass +
  labs(y = NULL,
       x = NULL,
       title = title1,
       subtitle ="",
       caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  geom_text(aes(x=date, y=place_y, label=num_label), nudge_y = 0.003, size=4, color="pink") +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size=18),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))

ggsave("graphics/g2_services_goods.png", dpi="retina", width = 12, height=6.75, units = "in")


##### FOOD AND ENERGY #####
food_energy_dates <- cpi %>% filter(date >= "2017-01-01") %>% select(date)
food_energy_dates <- unique(food_energy_dates$date)
food_energy_dates <- sort(food_energy_dates, decreasing = TRUE)
food_energy_dates = food_energy_dates[seq(1, length(food_energy_dates), 6)]

cpi %>% filter(item_name %in% c("Food", "Energy")) %>%
  filter(date >= "2017-01-01") %>%
  ggplot(aes(x = date, y = Wchange1a, fill = item_name)) +
  geom_bar(stat = 'identity', size=0) + theme_lass +
  labs(y = NULL,
       x = NULL,
       title = title_energy_food,
       subtitle = "Monthly Contribution to Inflation, Annualized.",
       caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  scale_fill_brewer(palette="RdYlGn") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=food_energy_dates) +
  #geom_text(aes(x=date, y=place_y, label=num_label), nudge_y = 0.003, size=4, color="pink") +
  theme(legend.position = c(0.9,0.85), legend.title = element_blank(), legend.text = element_text(size=18),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))

ggsave("graphics/energy_food.png", dpi="retina", width = 12, height=6.75, units = "in")


###### Graphic 3: Services Breakdown #####
cpi %>% filter(date > "2020-12-01", item_name %in% c("Services less energy services", "Shelter")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core services` = `Services less energy services` - `Shelter`) %>%
  select(-`Services less energy services`) %>%
  pivot_longer(c(Shelter, `Rest of core services`), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("Shelter", "Rest of core services"))) %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity', size=0) +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = title2,
       subtitle = "Monthly core services contribution to inflation, annualized.",
       caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  geom_text(size = 4, position = position_stack(vjust = 0.5), color="black") +
  scale_fill_brewer(palette="RdPu") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  theme(legend.position = c(0.25,0.75), legend.title = element_blank(), legend.text = element_text(size=16),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))

ggsave("graphics/g3_services.png", dpi="retina", width = 12, height=6.75, units = "in")


#### THREE CATEGORIES ####

MI_dates_three_categories <- cpi %>% filter(date > "2017-12-01")
MI_dates_three_categories <- unique(MI_dates_three_categories$date)
MI_dates_three_categories <- sort(MI_dates_three_categories, decreasing = TRUE)
MI_dates_three_categories = MI_dates_three_categories[seq(1, length(MI_dates_three_categories), 12)]


three_categories <-
  cpi %>% filter(date > "2017-12-01", item_name %in% c("Services less energy services", "Shelter", "Commodities less food and energy commodities")) %>%
  mutate(item_name = str_replace_all(item_name, "Commodities less food and energy commodities","Core_goods")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core services` = `Services less energy services` - `Shelter`) %>%
  select(-`Services less energy services`) %>%
  pivot_longer(c(Shelter, `Rest of core services`,Core_goods), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = str_replace_all(item_name, "Core_goods","Core goods")) %>%
  mutate(item_name = factor(item_name, levels = c("Core goods", "Shelter", "Rest of core services"))) %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label))


three_trend_2018_2020 <- three_categories %>%
  filter(date <= "2020-01-01") %>%
  select(item_name, Wchange1a) %>%
  group_by(item_name) %>%
  summarize(avg_pre = mean(Wchange1a))

three_categories %>%
  ggplot(aes(x = date, y = Wchange1a, fill=item_name)) +
  geom_bar(stat = 'identity', size=0) +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  facet_grid(~item_name) +
  labs(y = NULL,
       x = NULL,
       title = title3,
       subtitle = "Monthly contribution to inflation, annualized.",
       caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  scale_fill_brewer(palette="RdPu", name = "item_name") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates_three_categories) +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))

ggsave("graphics/three_categories.png", dpi="retina", width = 12, height=6.75, units = "in")
#####


###### Graphic 3: Services Breakdown Medical #####
cpi %>% filter(date > "2020-12-01", item_name %in% c("Services less energy services", "Shelter", "Medical care services","Food away from home")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core services` = `Services less energy services` - `Medical care services` - `Shelter`) %>%
  select(-`Services less energy services`) %>%
  pivot_longer(c(Shelter, `Medical care services`, `Rest of core services`,`Food away from home`), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("Food away from home","Shelter", "Medical care services","Rest of core services"))) %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity', size=0) +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "Services Inflation Down Across Categories",
       subtitle = "Monthly core services contribution to Inflation, Annualized.",
       caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  geom_text(size = 4, position = position_stack(vjust = 0.5), color="black") +
  scale_fill_brewer(palette="RdPu") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  theme(legend.position = c(0.25,0.85), legend.title = element_blank(), legend.text = element_text(size=18),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))

ggsave("graphics/g3_services_medical.png", dpi="retina", width = 12, height=6.75, units = "in")

###### GRAPHIC 3a : Goods Breakdown - Autos ####

cpi %>% filter(date > "2020-12-01", item_name %in% c("Commodities less food and energy commodities", "Transportation commodities less motor fuel")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core goods` = `Commodities less food and energy commodities` - `Transportation commodities less motor fuel`) %>%
  select(-`Commodities less food and energy commodities`) %>%
  rename(`All autos` = `Transportation commodities less motor fuel`) %>%
  pivot_longer(c(`All autos`, `Rest of core goods`), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("All autos", "Rest of core goods"))) %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity', size=0) +
  labs(y = NULL,
       x = NULL,
       title = title4,
       subtitle = "Monthly core goods contribution to inflation, annualized.",
       caption ="Autos is New and Used Cars and Motor Parts. BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  geom_text(size = 4, position = position_stack(vjust = 0.5), color="black") +
  scale_fill_brewer(palette="Greens") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  theme(legend.position = c(0.85,0.65), legend.title = element_blank(), legend.text = element_text(size=16),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))


ggsave("graphics/g3_goods_autos.png", dpi="retina", width = 12, height=6.75, units = "in")


cpi %>% filter(item_name %in% c("Commodities less food and energy commodities", "Transportation commodities less motor fuel")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core goods` = `Commodities less food and energy commodities` - `Transportation commodities less motor fuel`) %>%
  select(-`Commodities less food and energy commodities`) %>%
  rename(`All autos` = `Transportation commodities less motor fuel`) %>%
  pivot_longer(c(`All autos`, `Rest of core goods`), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("All autos", "Rest of core goods"))) %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity', size=0) +
  labs(y = NULL,
       x = NULL,
       title = title4,
       subtitle = "Monthly core goods contribution to inflation, annualized.",
       caption ="Autos is New and Used Cars and Motor Parts. BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  geom_text(size = 4, position = position_stack(vjust = 0.5), color="black") +
  scale_fill_brewer(palette="Greens") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  theme(legend.position = c(0.85,0.65), legend.title = element_blank(), legend.text = element_text(size=16),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))


ggsave("graphics/g3_goods_autos.png", dpi="retina", width = 12, height=6.75, units = "in")



###### BETTER THREE SIX ####

#### RIDGELINE GRAPH ####

median_terms <- read_csv("weights/mediancpi_component_table.csv") %>% mutate(item_name = Component)
median <- cpi %>%
  filter(item_name %in% median_terms$item_name | item_name == "Owners' equivalent rent of residences") %>%
  filter(!is.na(date)) %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Pchange1a = (1 + Pchange1)^12 - 1) %>%
  ungroup() %>%
  
  group_by(date) %>%
  mutate(normalized = sum(weight)) %>%
  mutate(weightN = weight/normalized) %>%
  arrange(Pchange3) %>%
  mutate(cumsum = cumsum(weight)/100) %>%
  mutate(cumsumN = cumsum(weightN)) %>%
  ungroup() %>%
  mutate(Pchange3a = (1+Pchange3)^4-1)

#THIS IS THE GRAPHIC - 30 percent-trimmed distribution
median %>% mutate(dateF = as.factor(date)) %>%
  filter(cumsumN <= 0.85 & cumsum >= 0.15) %>%
  mutate(Pchange3a = (1+Pchange3)^4-1) %>%
  filter(date >= "2017-06-01") %>%
  filter(date != "2020-06-01") %>%
  filter(month(date) %in% c(8,11,2,5)) %>%
  mutate(monthC = format(date, "%B, %Y")) %>%
  mutate(monthC = fct_reorder(monthC,date)) %>%
  mutate(monthCR = fct_rev(monthC)) %>%
  ggplot(aes(x = Pchange3a, y = monthCR, fill = stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis(option = "H") +
  theme_ridges() + theme_lass +
  theme(legend.position = "none") +
  scale_x_continuous(labels = percent) +
  labs(title="Distribution of Price Increases Moved Out, But Now Returning Back",
       subtitle="Distribution of the Cleveland Fed's Median/Trimmed-Mean CPI price basket, 3-month change annualized, with components\nwhose expenditure weights fall above/below the 85/15th percentile of price changes removed.",
       x="Three Month Percent Change", y="", caption="OER is treated as one value, instead of broken out by region and manually seasonally adjusted as per Cleveland Fed's methodology.\nJune 2020 removed as negative outlier. Mike Konczal, Roosevelt Institute") +
  theme(plot.title.position = "plot", legend.position = "none", legend.title = element_blank(),
        plot.title = element_text(size = 25,margin=margin(0,0,5,0)),
        plot.subtitle = element_text(size=15),
        plot.caption = element_text(size=10, face="italic"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.text.x = element_text(size=12))

ggsave("graphics/trimmed_dist.png", dpi="retina", width = 12, height=12, units = "in", bg = "white")



##### Supercore Graphic ####
MI_dates <- cpi %>% filter(date > "2010-12-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates = MI_dates[seq(1, length(MI_dates), 12)]

supercore <- cpi %>% filter(item_name == "All items less food, shelter, energy, and used cars and trucks") %>%
  select(date, value) %>%
  mutate(ThreeMonth = (value/lag(value,3))^4-1) %>%
  mutate(SixMonth = (value/lag(value,6))^2-1) %>%
  mutate(YoY = (value/lag(value,12))-1) %>%
  select(-value, YoY) %>%
  pivot_longer(ThreeMonth:SixMonth, names_to = "time_length", values_to = "change") %>%
  mutate(time_length = str_replace_all(time_length,"SixMonth", "6-Month Change")) %>%
  mutate(time_length = str_replace_all(time_length,"ThreeMonth", "3-Month Change")) %>%
  mutate(last_value = ifelse(date==max(date),change,NA)) %>%
  mutate(type = "All items less food, shelter, energy, and used cars and trucks") #%>%
  #rbind(core_services)

date_start = "2017-01-01"
date_end = "2019-01-01"
date_period <- interval(date_start, date_end)
date_period = date_period %/% months(1)

pre_supercore <- cpi %>% filter(item_name == "All items less food, shelter, energy, and used cars and trucks",
                                date == date_start | date == date_end) %>% group_by(item_name) %>%
  mutate(change = value/lag(value,1)) %>% filter(!is.na(change)) %>% mutate(change = change^(12/date_period) - 1) %>% select(pre_values = change, type = item_name)

one_month <- cpi %>% filter(item_name == "All items less food, shelter, energy, and used cars and trucks") %>%
  select(date, Pchange1a) %>% mutate(time_length = "3-Month Change") %>% filter(Pchange1a > -0.05)

supercore %>% filter(date > "2016-12-01") %>%
  left_join(one_month, by=c("date","time_length")) %>%
  left_join(pre_supercore, by=c("type")) %>%
  mutate(type = str_replace_all(type, "Services less rent of shelter", "Non-housing services")) %>%
  mutate(type = str_replace_all(type, "All items less food, shelter, energy, and used cars and trucks", "Supercore: All items less food, shelter, energy, and used autos")) %>%
  ggplot(aes(date, change, color=time_length, label=label_percent(accuracy=0.1)(last_value))) + geom_line(size=1.2) + facet_wrap(~type) +
  geom_line(aes(date,pre_values), linetype="dashed", color="#FFD3B5") +
  labs(x="", y="",
       title="Supercore Has a Very Low Month",
       subtitle = "Monthly percent change, annualized. Line reflects 2017-2019 trend of 1.2 percent.",
       caption = "BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. 1-month for April 2020 not displayed. Author's calculations. Mike Konczal, Roosevelt Institute.") +
  theme_lass +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  theme(legend.position = c(0.85,0.35), legend.text = element_text(size=15)) +
  scale_color_manual(values=c("#FFD3B5", "#F67280")) +
  geom_text_repel(show.legend=FALSE, nudge_x = 55) +
  geom_col(aes(date, Pchange1a), alpha=0.1, size=0, fill="#FFD3B5",show.legend = FALSE)

ggsave("graphics/three_six_supercores.png", dpi="retina", width = 12, height=6.75, units = "in")


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


##### just used autos ####
###### Used Cars - Short and Long ########
HI_dates <- cpi %>% filter(date >= "2019-06-01")
HI_dates <- unique(HI_dates$date)
HI_dates <- sort(HI_dates, decreasing = TRUE)
HI_dates = HI_dates[seq(1, length(HI_dates), 6)]

cpi %>% filter(date >= "2021-01-01", item_name %in% c("Commodities less food and energy commodities", "Used cars and trucks")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core goods` = `Commodities less food and energy commodities` - `Used cars and trucks`) %>%
  select(-`Commodities less food and energy commodities`) %>%
  rename(`Used autos` = `Used cars and trucks`) %>%
  pivot_longer(c(`Used autos`, `Rest of core goods`,), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("Used autos", "Rest of core goods"))) %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity', size=0) +
  labs(y = NULL,
       x = NULL,
       title = "First Time Since 2021: 5 Months of Zero Goods ex Used Cars Inflation",
       subtitle = "Monthly contribution to inflation, annualized.",
       caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  geom_text(size = 4, position = position_stack(vjust = 0.5), color="black") +
  scale_fill_brewer(palette="Greens") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=HI_dates) +
  theme(legend.position = c(0.85,0.65), legend.title = element_blank(), legend.text = element_text(size=16),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))


ggsave("graphics/g3_goods_just_used_autos.png", dpi="retina", width = 12, height=6.75, units = "in")


cpi %>% filter(item_name %in% c("Commodities less food and energy commodities", "Used cars and trucks")) %>%
  select(date, item_name, Wchange1a) %>%
  pivot_wider(names_from = item_name, values_from = Wchange1a) %>%
  mutate(`Rest of core goods` = `Commodities less food and energy commodities` - `Used cars and trucks`) %>%
  select(-`Commodities less food and energy commodities`) %>%
  rename(`Used autos` = `Used cars and trucks`) %>%
  pivot_longer(c(`Used autos`, `Rest of core goods`,), names_to = "item_name", values_to = "Wchange1a") %>%
  mutate(item_name = factor(item_name, levels = c("Used autos", "Rest of core goods"))) %>%
  mutate(num_label = round(100*Wchange1a, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  filter(item_name == "Rest of core goods") %>% mutate(item_name = "Core goods ex used autos") %>%
  
  ggplot(aes(x = date, y = Wchange1a, fill = item_name, label = num_label)) +
  geom_line(size = 1.2) +
  labs(y = NULL,
       x = NULL,
       title = "First Time Since 2021: 5 Months of Zero Goods ex Used Cars Inflation",
       subtitle = "Monthly contribution to inflation, annualized.",
       caption ="BLS, CPI, 2022 weights prior to 2023, seasonally adjusted. Author's calculation. Mike Konczal, Roosevelt Institute") +
  theme_lass +
  scale_fill_brewer(palette="Greens") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=HI_dates) +
  theme(legend.position = c(0.85,0.65), legend.title = element_blank(), legend.text = element_text(size=16),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))


ggsave("graphics/g3_goods_just_used_autos_long.png", dpi="retina", width = 12, height=6.75, units = "in")


##### FOOD #####


food_index <- c("Rice, pasta, cornmeal","Breakfast cereal", "Flour and prepared flour mixes",
                "Bread", "Fresh biscuits, rolls, muffins", "Cakes, cupcakes, and cookie",
                "Beef and veal", "Pork", "Other meats", "Poultry", "Fish and seafood", "Eggs",
                "Milk", "Fresh fruits", "Fresh vegetables", "Processed fruits and vegetables", "Juices and nonalcoholic drinks",
                "Beverage materials including coffee and tea", "Fats and oils", "Sugar and sweets")

# total weights of food_index
cpi %>% filter(item_name %in% food_index, date == max(date)) %>% summarize( n = sum(weight))
cpi %>% filter(item_name == "Food at home", date == max(date)) %>% summarize( n = sum(weight))

cpi %>% filter(item_name %in% food_index, date == max(date), Pchange3 < 0) %>%  summarize( n = sum(weight))
#### GRAPHIC FOOD ####
# Comparison point
#food_dates <- cpi %>% filter(item_name %in% food_index) %>% filter(date == max(date) | date == max(date) %m-% months(7) | date == max(date) %m-% months(4)) %>%
food_dates <- cpi %>% filter(item_name %in% food_index) %>% filter(date == max(date) | date == "2022-12-01" | date == "2022-08-01") %>%
  mutate(date = paste(as.character(lubridate::month(date, label = TRUE, abbr = FALSE)), ", ", as.character(year(date)), sep = "")) %>%
  mutate(date = factor(date,levels=c("May, 2023","December, 2022","August, 2022")), name = reorder_within(item_name, Pchange3, date))

ggplot(food_dates, aes(name, Pchange3, fill = date)) +
  geom_col(show.legend = FALSE, size=0) +
  facet_wrap(~date, scales = "free_y", ncol=1) + theme_lass +
  coord_flip() +
  scale_x_reordered() +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot") +
  labs(y = "Price Increase Over Previous Three Months",
       x = NULL,
       title = "Food Price Increases Have Broadened Over The Past Year",
       subtitle = "Price increase over previous three months of listed date. These categories represent 68 percent of all spending on food at home.",
       caption ="BLS, CPI, all subcategories of 'Food at home.' Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute.") +

  theme(panel.grid.major.x = element_line(size=0.5)) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot") +
  labs(y = "Price Increase Over Previous Three Months",
       x = NULL,
       title = "Food Price Increases Have Narrowed Recently, With Eggs in Freefall",
       subtitle = "Price increase over previous three months of date. Categories represent 68 percent of spending on food at home.",
       caption ="BLS, CPI, all subcategories of 'Food at home.' Seasonally Adjusted. Author's Calculation. @rortybomb") +
  theme(axis.text.y = element_text(size=12),
  axis.text.x = element_text(size=12, face="bold"), strip.text = element_text(color="white", size = 14)) +
  theme(panel.spacing.y=unit(0, "lines"), panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size=12, color="white"))

ggsave("graphics/food_chart.png", dpi="retina", width = 12, height=12, units = "in")

cpi %>% filter(item_name == "Eggs") %>%
  ggplot(aes(date,value)) + geom_line(size = 1.2) + theme_lass +
  labs(title="We're Back - 30 Percent Decline in Egg Prices Since January", subtitle="Price level for Eggs in U.S. city average, all urban consumers, seasonally adjusted, 1982-84=100.")

ggsave("graphics/egg_chart.png", dpi="retina", width = 12, height=6.75, units = "in")
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



##### Phillips Curves #####

getSymbols("UNRATE", src="FRED")
unrate <- as_tibble(data.frame(date = index(UNRATE), unrate <- UNRATE[,1]))
colnames(unrate) <- tolower(colnames(unrate))

getSymbols("NROU", src="FRED")
nrou <- as_tibble(data.frame(date = index(NROU), NROU <- NROU[,1]))
colnames(nrou) <- tolower(colnames(nrou))

unrate <- unrate %>% left_join(nrou, by="date")
unrate$nrou <- na.locf(unrate$nrou, na.rm = FALSE)
unrate$u_gap <- unrate$unrate - unrate$nrou
unrate <- unrate %>% mutate(unrate = unrate/100, nrou = nrou/100, u_gap = u_gap/100)

cpi_analysis <- cpi %>% left_join(unrate, by="date")# %>% left_join(lowest, by="item_name") %>% filter(lowest == 1)

cpi_lm <- cpi_analysis %>% filter(date >= "2000-01-01") %>%
  # All items
  group_by(item_name) %>%
  # Regression: inflation = lagged inflation + unemployment gap
  do(tidy(lm(Pchange1a ~ lag(Pchange1a) + I(unrate - nrou), data = .))) %>%
  # Filter for statistically significant negative esimates on unemployment gap
  filter(term == "I(unrate - nrou)", statistic < -1.95) %>% arrange(statistic)
print(cpi_lm, n=Inf)

food_energy_dates <- cpi %>% filter(date >= "2014-01-01") %>% select(date)
food_energy_dates <- unique(food_energy_dates$date)
food_energy_dates <- sort(food_energy_dates, decreasing = TRUE)
food_energy_dates = food_energy_dates[seq(1, length(food_energy_dates), 12)]


housing <- c("Owners' equivalent rent of primary residence","Rent of primary residence")


with_housing <- cpi %>% filter(item_name %in% cpi_lm$item_name, year(date) > 2014) %>%
  mutate(weight = weight/100) %>%
  group_by(date) %>%
  summarize(change = sum(Wchange1),
            weight = sum(weight),
            changeW = change/weight,
            changeWA = (changeW+1)^12-1) %>%
  ungroup() %>% mutate(type = "All Phillips Curve items")

cpi %>% filter(item_name %in% cpi_lm$item_name, !(item_name %in% housing), year(date) > 2014) %>%
  mutate(weight = weight/100) %>%
  group_by(date) %>%
  summarize(change = sum(Wchange1),
            weight = sum(weight),
            changeW = change/weight,
            changeWA = (changeW+1)^12-1) %>%
  ungroup() %>% mutate(type = "All Phillips Curve items excluding housing") %>%
  rbind(with_housing) %>%
  ggplot(aes(date,changeWA, color=type)) + geom_line(size=1.1) + theme_classic() + theme_lass +
  labs(x="", y="",
       title="Demand Sensitive Items Also Reversing with Unemployment Low",
       subtitle = "Lowest level CPI items with a statistically significant negative relationship between\ninflation and unemployment gap + previous inflation, bundled. 1-month change annualized.",
       caption = "BLS, Author's calculations. Mike Konczal, Roosevelt Institute.") +
  theme_lass +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks = food_energy_dates) +
  scale_color_manual(values=c("#2D779C", "#A4CCCC")) + theme(legend.position = c(0.35,0.9))

ggsave("graphics/pc_curve.png", dpi="retina", width = 12, height=6.75, units = "in")


cpi_lm_NH <- cpi_lm %>% filter(!(item_name %in% c("Rent of primary residence","Owners' equivalent rent of primary residence","Electricity")))


##### Core Goods Ex Used Car Index ####
cpi_goods_ex_autos_index <-
  cpi %>%
  select(date, item_name, Wchange1, weight_nhs) %>%
  mutate(weight_nhs = weight_nhs/100) %>%
  group_by(date) %>%
  summarize(goods_ex_auto_m1 = Wchange1[item_name == "Commodities less food and energy commodities"] - Wchange1[item_name == "Used cars and trucks"],
            goods_ex_auto_weight = weight_nhs[item_name == "Commodities less food and energy commodities"] - weight_nhs[item_name == "Used cars and trucks"],
  ) %>%
  ungroup() %>%
  mutate(goods_ex_auto_m1_W = goods_ex_auto_m1/goods_ex_auto_weight) %>%
  mutate(goods_ex_auto_m1_WA = (goods_ex_auto_m1_W+1)^12-1) %>%
  mutate(index = goods_ex_auto_m1_W+1) %>% filter(!is.na(index)) %>%
  mutate(index = cumprod(index))

cpi_goods_ex_autos_index %>% mutate(diff = (index/lag(index,3))^4-1) %>%
  ggplot(aes(date,diff)) + geom_line(size=1.2) + theme_lass +
  scale_color_manual(values=c("#F67280")) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y") +
  theme(legend.position = "none", legend.title = element_blank(), legend.text = element_text(size=16),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19)) +
  labs(title="Core Goods ex Used Autos, CPI, 3-month Percent Change, Annualized",
       caption="BLS, CPI, Seasonally-Adjusted. Author's calculations. Mike Konczal, Roosevelt Institute.") +
  geom_hline(yintercept = 0, color="white")

ggsave("graphics/goods_ex_autos_long.png", dpi="retina", width = 12, height=6.75, units = "in")




#### Three month change ####
item_basket_core_services <- c("Services less energy services",
                               "Shelter",
                               "Medical care services",
                               "Transportation services",
                               "Recreation services",
                               "Education and communication services",
                               "Other personal services")

item_basket_core_goods <- c("Commodities less food and energy commodities",
                            "Household furnishings and supplies",
                            "Apparel",
                            "Transportation commodities less motor fuel",
                            "Medical care commodities",
                            "Recreation commodities",
                            "Education and communication commodities",
                            "Alcoholic beverages",
                            "Other goods")


item_basket_topline <- c("All items", "Energy", "Food", "Commodities less food and energy commodities", "Services less energy services")

item_basket_watch_categories <- c("All items", "New and used motor vehicles", "Shelter", "Other services",
                                  "Medical care services", "Food", "Energy", "Commodities less food and energy commodities")



cpi %>% 
  filter(item_name %in% median_terms$Component) %>%
  filter(date %in% c(max(date), max(date) %m-% months(12))) %>%
  select(date, Pchange1a) %>%
  group_by(date) %>%
  do({
    d <- density(.$Pchange1a, kernel = "epanechnikov")
    data.frame(x = d$x, y = d$y)
  }) %>%
  ggplot(aes(x = x, y = y, color = as.factor(date))) + 
  geom_line() +
  labs(color = "Date")

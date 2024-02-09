# First of the month!

cpi_data

median_terms <- read_csv("weights/mediancpi_component_table.csv") %>% mutate(item_name = Component)

df <- cpi_data %>%
  filter(seasonal == "U", !is.na(date), area_code == "0000", item_name %in% median_terms$item_name, substr(period, 1, 1) == "M") %>%
  mutate(month = as.factor(month(date))) %>%
  group_by(item_name) %>%
  mutate(change1 = value / lag(value, 1) - 1) %>%
  ungroup()

ggplot(df, aes(month, change1)) + geom_line() + theme_classic() + facet_wrap(~item_name)

df %>% select(item_name, month, year, change1) %>%
  filter(!is.na(change1), year != 1997) %>%
  group_by(item_name, year) %>%
  # THIS ISN'T CORRECT
  mutate(total_change = sum(change1)) %>%
  ungroup() %>%
  mutate(percent_change = change1/total_change) %>%
  group_by(month) %>%
  summarize(mean(percent_change))
  
  
df_all <- cpi_data %>% filter(series_id == "CUUR0000SA0L1E", substr(period, 1, 1) == "M", period != "M13") %>%
  mutate(change1 = value/lag(value,1) - 1,
         month = month(date),
         month2 = as.factor(month(date))) %>%
  filter(year >= 1998) %>%
  mutate(year = as.factor(year))


ggplot(df_all, aes(month, change1)) + geom_line() + geom_point() + facet_wrap(~year) + theme_classic()

df_all %>% select(year,date,month,change1) %>%
  group_by(year) %>%
  
  mutate(total_change = sum(change1),
         change_month = change1/total_change) %>%
  ungroup() %>%
  group_by(month) %>%
  summarize(avg = mean(change_month))
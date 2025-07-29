library(tidyverse)
library(lubridate)
library(govMacroTools)

# ESP color palette
esp_colors <- c(
  "Core CPI" = "#2c3254",
  "Core PCE" = "#ff8361"
)

core_cpi <- cpi %>% filter(series_id == "CUSR0000SA0L1E") %>%
  select(date, core_cpi = value)

# Get and reshape data
df <- getFRED(
  core_pce = "PCEPILFE"
) %>%
  left_join(core_cpi, by = "date") %>%
  arrange(date) %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  mutate(
    series_label = recode(series,
                          core_cpi = "Core CPI",
                          core_pce = "Core PCE")
  ) %>%
  group_by(series_label) %>%
  mutate(
    yoy = (value / lag(value, 12) - 1),
    q3m = ((valuedf_lo / lag(value, 6))^2 - 1)
  ) %>%
  ungroup() %>%
  filter(date >= as.Date("2023-01-01"))  # enough history for YoY

# Reshape to long format for faceting
df_long <- df %>%
  select(date, series_label, yoy, q3m) %>%
  pivot_longer(
    cols = c(yoy, q3m),
    names_to = "change_type",
    values_to = "value"
  ) %>%
  mutate(
    change_type = recode(change_type,
                         yoy = "Year-over-Year Change",
                         q3m = "6-Month Annualized Change")
  )

# Latest values for labels
last_vals <- df_long %>%
  group_by(series_label, change_type) %>%
  filter(date == max(date)) %>%
  ungroup()

# Plot
ggplot(df_long, aes(x = date, y = value, color = series_label)) +
  geom_line(size = 1.2) +
  geom_text(
    data = last_vals,
    aes(label = series_label),
    hjust = -0.1,
    vjust = 0.5,
    show.legend = FALSE,
    size = 5
  ) +
  facet_wrap(~change_type, scales = "free_y") +
  scale_color_manual(values = esp_colors) +
  scale_y_continuous(label = percent) +
  scale_x_date(
    date_labels = "%b\n%Y", breaks = generate_dates(df_long$date, 6),
    expand = expansion(mult = c(0.01, 0.15))
  ) +
  coord_cartesian(clip = "off") +
  theme_esp() +
  theme(strip.text = element_text(size = 16, face = "bold")) +
  labs(
    title = "Core PCE is Suprisingly Running Hotter Than Core CPI in 2025",
    subtitle = "Measured as Year-over-Year and 6-Month Annualized Inflation",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Mike Konczal"
  )

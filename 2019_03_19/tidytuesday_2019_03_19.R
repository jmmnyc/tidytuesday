library(tidyverse)
library(ggthemes)
library(scales)

combined_data <- readr::read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv")

View(combined_data)

my_font <- "Verdana"
base_color <- "#faf7ec"
font_color <- "#399694"
my_theme <- theme(text = element_text(family = my_font, face = "bold" ,color = font_color),
                  rect = element_rect(fill = base_color, color = NA), 
                  plot.background = element_rect(fill = base_color, color = NA), 
                  panel.background = element_rect(fill = base_color, color = NA), 
                  panel.border = element_blank(),
                  legend.background = element_rect(fill = base_color, color = NA),
                  legend.key = element_rect(fill = base_color),
                  plot.caption = element_text(size = 8))

theme_set(theme_light() + my_theme)

rates_by_states <- combined_data %>% 
  filter(!is.na(location) & state != "RI") %>%
  select(location, state, driver_race, stop_rate, stops_per_year) %>% 
  group_by(state) %>% 
  mutate(total_state_stops = sum(stops_per_year)) %>% 
  ungroup() %>% 
  mutate(weight = stops_per_year/total_state_stops,
         weighted_stop_rate = weight * stop_rate) %>% 
  select(location, state, driver_race, weighted_stop_rate) %>% 
  spread(driver_race, weighted_stop_rate) %>% 
  group_by(state) %>% 
  summarise(Minority_stop_rate = sum(Black, na.rm = TRUE) + sum(Hispanic, na.rm = TRUE),
            White_stop_rate = sum(White, na.rm = TRUE))
  


plot <- rates_by_states %>% 
  ggplot(aes(x = Minority_stop_rate, y = White_stop_rate, label = state)) + 
  geom_point(color = "#bc5652",alpha = .3, size = 8) +
  geom_text(aes(label = state)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  labs(title = "Search rate of traffic stops by State",
       subtitle = "Points on the line represent parity across driver race",
       caption = "Plot: @Joseph_Mike \nData: Stanford Open Policing Project") +
  scale_x_continuous('White search rate', limits=c(0, .35), labels = percent_format(accuracy = 1), expand=c(0,0)) + 
  scale_y_continuous('Minority search rate', limits=c(0, .35), labels = percent_format(accuracy = 1), expand=c(0,0)) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(margin = margin(15,0,0,0)),
        axis.title.y = element_text(margin = margin(0,15,0,0)),
        axis.title = element_text(margin = margin (0,0,15,0)))

plot

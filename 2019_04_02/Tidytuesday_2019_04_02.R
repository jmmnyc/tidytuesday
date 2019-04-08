library(tidyverse)
library(ggthemes)
library(lubridate)

bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")

#custom theme
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
                  plot.caption = element_text(size = 8),
                  axis.title.x = element_text(margin = margin(15,0,0,0)),
                  axis.title.y = element_text(margin = margin(0,15,0,0)),
                  axis.title = element_text(margin = margin (0,0,15,0))
)

theme_set(theme_light() + my_theme)



View(bike_traffic)


plot_data <- bike_traffic %>% 
  mutate(date_time = mdy_hms(date),
         date_only = date(date_time),
         year = year(date_time),
         weekday = wday(date_time, label = TRUE),
         weekday_ind = ifelse(weekday %in% c("Sat", "Sun"), "Weekend", "Weekdays"),
         hour = hour(date_time),
         bikes = sum(bike_count, na.rm = TRUE)) %>% 
  filter(year == '2018') %>% 
  group_by(crossing, weekday_ind, hour) %>% 
  summarise(total_bikes = sum(bike_count, na.rm = TRUE))

plot <- plot_data %>% 
  ggplot(aes(x = hour, y = total_bikes/1000)) +
  geom_line(color = "#bc5652", size = 1) +
  facet_grid(rows = vars(crossing),
             cols = vars(weekday_ind),
             scales = "free_y",
             labeller = labeller(crossing = label_wrap_gen(24))) +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, size = 10)) +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, size = 10)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  labs(title = "Hourly trends of Seatle bike usage",
       subtitle = "2018",
       y = "Bike usage (in thousands)",
       x = "Time of day",
       caption = "Plot: @Joseph_Mike \nData: Seattle Department of Transportation")
  

plot



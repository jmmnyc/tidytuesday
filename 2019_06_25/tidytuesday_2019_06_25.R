library(extrafont)

loadfonts(device = "win")

library(tidyverse)
library(ggthemes)
library(Cairo)
library(lubridate)

ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

# Set custom ggplot theme
my_font <- "Agency FB" 

base_color <- "#1E132C"

font_color <- "#70cd3e" # or #213549

my_theme <- theme(text = element_text(family = my_font, color = font_color, face = "bold"),
                  panel.border = element_blank(),
                  rect = element_rect(fill = base_color, color = NA), 
                  plot.background = element_rect(fill = base_color, color = NA), 
                  panel.background = element_rect(fill = base_color, color = NA), 
                  legend.background = element_rect(fill = base_color, color = NA),
                  legend.key = element_rect(fill = base_color),
                  plot.caption = element_text(size = 10, color = "#da7792"),
                  plot.subtitle = element_text(size = 10),
                  axis.title.x = element_text(margin = margin(20,0,0,0), size = 14),
                  axis.title.y = element_text(margin = margin(0,20,0,0), size = 14),
                  axis.text.y = element_text(color = "#f7f5f5", size = 10),
                  axis.text.x = element_text(color = "#f7f5f5", size = 10),
                  plot.title = element_text(margin = margin(0,0,20,0), size = 18))

theme_set(theme_dark() + my_theme)




plot_data <- ufo_sightings %>% 
  mutate(date_time = parse_date_time(date_time, 'mdy_HM'),
         month = as.factor(month(date_time, label = TRUE)),
         year = year(date_time)) %>% 
  filter(year > 1980,
         year < 2009) %>% 
  mutate(decade = year - year%%10) %>% 
  group_by(decade, month) %>% 
  summarise(count = n()) %>% 
  ungroup() 

plot_data %>% 
  ggplot(aes(month, count)) +
  geom_col(fill = "#eaa27c")  +
  facet_wrap(~decade) +
  theme(panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 14, color = "#f7f5f5"),
        strip.background.x = element_rect(fill = "#eaa27c")) +
  labs(title = "Monthly distribution of UFO sightings in last 3 decades",
        x = "Month",
        y = "UFOs sighted",
        caption = "Data from NUFORC\nVisualization by Jose M @Joseph_Mike")

ggsave("TidyTuesday_2019_06_25.png", width = 10, height = 6,device = "png", type = "cairo")



  
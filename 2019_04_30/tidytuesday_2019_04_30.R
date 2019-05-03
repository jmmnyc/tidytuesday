library(tidyverse)
library(lubridate)
library(ggthemes)
library(extrafont)

bird_col <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")

loadfonts(device = "win")

my_font <- "Century Gothic"
base_color <- "#f5f3dc"
font_color <- "#42ac96" # or #213549
my_theme <- theme(text = element_text(family = my_font, color = font_color),
                  rect = element_rect(fill = base_color, color = NA), 
                  plot.background = element_rect(fill = base_color, color = NA), 
                  panel.background = element_rect(fill = base_color, color = NA), 
                  panel.border = element_blank(),
                  legend.background = element_rect(fill = base_color, color = NA),
                  legend.key = element_rect(fill = base_color),
                  plot.caption = element_text(size = 8, color = "#223a4f"),
                  axis.title.x = element_text(margin = margin(20,0,0,0)),
                  axis.title.y = element_text(margin = margin(0,20,0,0)),
                  plot.title = element_text(margin = margin(0,0,20,0)))

theme_set(theme_light() + my_theme)

plot_data <- bird_col %>% 
  mutate(year = year(date)) %>% 
  group_by(year, habitat) %>% 
  summarise(collisions = n()) %>% 
  ungroup()

View(plot_data)

plot <- plot_data %>% 
  ggplot(aes(year, collisions, group = habitat, col = habitat)) +
  geom_line(size = 1) +
  theme(legend.position = "top") +
  labs(title = "Annual trends of reported bird window collisions in Chicago (1978-2016)",
       x = "Year",
       y = "Reported collisions",
       colour = "Habitat",
       caption = "Source:  Winger et al https://doi.org/10.1098/rspb.2019.0364 \nVisiualization by Jose M @Joseph_Mike") +
  scale_color_manual(values = c("#FF7F00", "#984EA3", "#4DAF4A"))

ggsave("TidyTuesday_2019_04_30.png", dpi = "retina", height = 5, width = 8, units = "in")

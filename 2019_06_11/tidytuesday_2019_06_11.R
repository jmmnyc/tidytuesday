library(extrafont)

loadfonts(device = "win")

library(tidyverse)
library(ggthemes)
library(Cairo)
library(cowplot)


meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

# find the year with the most meotorites
meteorites %>% 
  group_by(year) %>%
  tally(sort = T)

# filter for 2003
plot_data <- meteorites %>% filter(year == 2003)

# plot colors
plot_base <- "#272238"

land_mass <- "#8dc6d1"

land_border <- "#388089"

meteor_points <- "#ef601f"

# font and font color
my_font <- "Agency FB" 

font_color <- "#f1e1b0" 

# Initial plot
world_view <- plot_data %>% 
  ggplot() +
  borders("world", col = land_border, fill = land_mass, size = .1) +
  theme_map() +
  theme(text = element_text(family = my_font, color = font_color, face = "bold"),
        plot.caption = element_text(size = 12),
        plot.subtitle = element_text(size = 16),
        plot.title = element_text(margin = margin(0,0,20,0), size = 22)) +
  coord_map(projection = "mollweide", orientation = c(90, 0, 0)) +
  geom_point(aes(x = long, y = lat, size = log(mass)), 
             size = 2, 
             alpha = .15,  
             fill = meteor_points,
             shape = 21) +
  labs(title = "Where did most meteorites fall in 2003?",
       subtitle = "The year with the highest meteorite count",
       caption = "Data from NASA \nVisualization by Jose M @Joseph_Mike")

ggdraw(world_view) +
  theme(
    plot.background = element_rect(fill = plot_base),
    panel.background = element_rect(fill = plot_base, color = plot_base),
    plot.margin = unit(c(.2, .2, .2, .2), "cm")
  ) 

ggsave("TidyTuesday_2019_06_11.png", width = 10, height = 6.5,device = "png", type = "cairo")
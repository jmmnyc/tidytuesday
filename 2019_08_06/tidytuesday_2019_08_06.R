library(extrafont)

loadfonts(device = "win")

library(tidyverse)
library(ggthemes)
library(Cairo)
library(janitor)

#get data
bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")


#set theme
my_font <- "Segoe UI Black"

base_color <- "#f5f3dc"

font_color <- "#331C20" # or #213549

my_theme <- theme(text = element_text(family = my_font, color = font_color),
                  rect = element_rect(fill = base_color, color = NA), 
                  plot.background = element_rect(fill = base_color, color = NA), 
                  panel.background = element_rect(fill = base_color, color = NA), 
                  panel.border = element_blank(),
                  legend.background = element_rect(fill = base_color, color = NA),
                  legend.key = element_rect(fill = base_color),
                  plot.caption = element_text(size = 8, color = "#331C20"),
                  axis.title.x = element_text(margin = margin(20,0,0,0)),
                  axis.title.y = element_text(margin = margin(0,20,0,0)),
                  plot.title = element_text(margin = margin(0,0,20,0), size = 14),
                  plot.subtitle = element_text(size = 10))

theme_set(theme_light() + my_theme)


#change seasons/episode format
plot_data <- bob_ross %>% 
 clean_names(case = "upper_camel") %>% 
  separate(Episode, into = c("Season", "Episode"), sep = "E") %>% 
  mutate(Season = str_extract(Season, "[:digit:]+")) %>% 
  mutate_at(vars(Season, Episode), as.integer)

# import image of Naruto eating ramen
img <- png::readPNG("auroraborealis.png")

rast <- grid::rasterGrob(img, interpolate = T)


plot_data %>% 
  ggplot(aes(Season, AuroraBorealis)) +
  geom_line() +
  annotation_custom(rast, ymin = 0.2, ymax = 0.8, xmin = 10) +
  scale_y_continuous(breaks = c(0,1)) +
  labs(title = "Only 2 Episodes feature Aurora Borealis aka Northern Lights",
       subtitle = "The paintings had many common elements including cabin, conifer, lake, snow and trees",
       x = "Aurora Borealis",
       y = "Season",
       caption = "Source: 538 \nVisualization by Jose M @Joseph_Mike")

ggsave("TidyTuesday_2019_08_06.png", width = 10, height = 6.5,device = "png", type = "cairo")

library(extrafont)

loadfonts(device = "win")

library(tidyverse)
library(ggthemes)
library(ggridges)
library(Cairo)
library(ggsci)

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

# Set custom ggplot theme
my_font <- "Tempus Sans ITC"

base_color <- "#ffffff"

font_color <- "#b85f29" # or #213549

my_theme <- theme(text = element_text(family = my_font, color = font_color, face = "bold"),
                  rect = element_rect(fill = base_color, color = NA), 
                  plot.background = element_rect(fill = base_color, color = NA), 
                  panel.background = element_rect(fill = base_color, color = NA), 
                  panel.border = element_blank(),
                  legend.background = element_rect(fill = base_color, color = NA),
                  legend.key = element_rect(fill = base_color),
                  plot.caption = element_text(size = 8, color = "#293042"),
                  plot.subtitle = element_text(size = 10),
                  axis.title.x = element_text(margin = margin(20,0,0,0), size = 14),
                  axis.title.y = element_text(margin = margin(0,20,0,0), size = 14),
                  axis.text.y = element_text(size = 12),
                  plot.title = element_text(margin = margin(0,0,20,0), size = 18))

theme_set(theme_light() + my_theme)

# find countries with over 100 ramen dishes rated
top_countries <- ramen_ratings %>% 
  group_by(country) %>% 
  tally(sort = TRUE) %>% 
  filter(n >= 100, 
         !is.na(country)) %>% 
  ungroup()

# filter data for the top 11 countires
plot_data <- inner_join(ramen_ratings, top_countries, by = "country") %>% select(-n)

# import image of Naruto eating ramen
img <- png::readPNG("ramen.png")

rast <- grid::rasterGrob(img, interpolate = T)

#create plot
plot_data %>% 
  ggplot(aes(stars, reorder(country, stars, median))) +
  annotation_custom(rast, ymin = 1, ymax = 6, xmin = 5.5) +
  geom_density_ridges(scale = 2,
                      aes(fill = country),
                      color = "#e58f1e",
                      size = 1,
                      alpha = 0.7) + 
  theme(legend.position = "none") +
  scale_fill_igv() +
  scale_x_continuous(breaks = seq(0,6,1), limits = c(1,6)) +
  coord_cartesian(clip = "off") +
  theme(panel.grid.minor.x = element_blank()) +
  labs(title = "Ramen rating distribution by country",
       subtitle = "Countries with over 100 ramen dishes reviewed",
       x = "Rating",
       y = "Country",
       caption = "Data from The Ramen Rater \nVisualization by Jose M @Joseph_Mike") 

ggsave("TidyTuesday_2019_06_04.png", width = 10, height = 6.5,device = "png", type = "cairo")

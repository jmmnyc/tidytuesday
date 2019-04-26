library(tidyverse)
library(ggthemes)
library(ggridges)
library(extrafont)


tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

tiny_tidy_anime <- tidy_anime %>% select(-synopsis, -background)

plot_data <-  tiny_tidy_anime %>% 
  filter( !is.na(score),
          !is.na(source)) 

#custom theme
my_font <- "Rockwell"
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

theme_set(theme_minimal() + my_theme)

  
plot_data %>% 
  filter(type == "TV") %>% 
  ggplot(aes(score, reorder(source, score, median))) +
  geom_density_ridges(quantile_lines = TRUE,
                      quantiles = 2,
                      size = .75,
                      color = "#b4484e" ,
                      fill = "#d3b98c") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0,10,1)) +
  labs(title = "Mangas & Novels produce the highest rated TV Animes",
       subtitle = "Digital Mangas are an outlier with the lowest ratings",
       x = "Fan Score",
       y = "Source of Anime Series",
       caption = "Data from Tam Nguyen & MyAnimeList.net via Kaggle \nPlot Jose M @Joseph_Mike") 




  
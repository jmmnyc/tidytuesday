library(extrafont)

loadfonts(device = "win")

library(tidyverse)
library(ggthemes)
library(Cairo)
library(ggsci)

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

# Set custom ggplot theme
my_font <- "Segoe UI Black"

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
                  axis.text.x = element_text(size = 9),
                  axis.text.y = element_text(size = 9),
                  plot.title = element_text(margin = margin(0,0,20,0), size = 18))

theme_set(theme_light() + my_theme)


# Top 10 Video Game publishers by number of titles
top_publishers <- video_games %>% 
  filter(!is.na(publisher),!is.na(price), !is.na(metascore),!is.na(average_playtime)) %>% 
  group_by(publisher) %>% 
  tally(sort = TRUE) %>% 
  filter(n >= 30)

# Filter for top 10 publishers
plot_data <- inner_join(video_games, top_publishers, by = "publisher") 


plot_data %>% 
  ggplot(aes(metascore, price)) +
  geom_point(aes(color = publisher, alpha = .8), size = 2) +
  scale_x_continuous(breaks = seq(50,100,10), limits = c(45,105)) +
  scale_color_simpsons() +
  theme(legend.position = "none") +
  facet_wrap(~publisher, nrow = 2) +
  theme(panel.grid.minor.x = element_blank(),
        strip.text.x = element_text(size = 8, color = "#f7f5f5"),
        strip.background.x = element_rect(fill = "#000000")) +
  labs (title = "Metascores and Average Playtimes across top Video Game Publishers",
      subtitle = "Top game publishers by number of titles",
      x = "Metascore",
      y = "Price",
      caption = "Data from Liza Wood via Steam Spy\nVisualization by Jose M @Joseph_Mike")

ggsave("TidyTuesday_2019_07_30.png", width = 8, height = 5,device = "png", type = "cairo")
  
  

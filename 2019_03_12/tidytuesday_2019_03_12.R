library(tidyverse)
library(ggthemes)

board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")


#set parameters
number_of_players <- 4
average_play_time <- 15
top_n_games <- 15

#filter the data
test_group <- board_games %>% 
  filter(min_players >= number_of_players & number_of_players <= max_players) %>% 
  filter(playing_time <= average_play_time) %>% 
  arrange(-average_rating) %>% 
  filter(users_rated >= 100) %>% #filter to games with over 100 user ratings
  head(n = top_n_games)

#create a custom theme
my_font <- "Avenir"
base_color <- "#faf7ec"
font_color <- "#399694"  # or "#213549" "#42ac96" 
my_theme <- theme(text = element_text(family = my_font, face = "bold" ,color = font_color),
                  rect = element_rect(fill = base_color, color = NA), 
                  plot.background = element_rect(fill = base_color, color = NA), 
                  panel.background = element_rect(fill = base_color, color = NA), 
                  panel.border = element_blank(),
                  legend.background = element_rect(fill = base_color, color = NA),
                  legend.key = element_rect(fill = base_color),
                  plot.caption = element_text(size = 8))

theme_set(theme_light() + my_theme)

#plot
ggplot(test_group, aes(x = reorder(name, average_rating), y = average_rating)) +
  geom_col(fill = "#c79966") +
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "Highest rated games \nfor 4-players and avg. play time <= 15 minutes",
       x = "",
       y = "Avg. Rating",
       caption = "Plot by: @Joseph_Mike\n Data:  Board Game Geek") +
  scale_y_continuous(breaks = c(0,2,4,6,8,10), limits = c(0,10))
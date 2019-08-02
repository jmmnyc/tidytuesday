library(extrafont)

loadfonts(device = "win")

library(tidyverse)
library(ggthemes)
library(Cairo)
library(ggsci)
library(lubridate)

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

# Set custom ggplot theme
my_font <- "Baskerville Old Face"

base_color <- "#1E1F22"

font_color <- "#CfC5AA" # or #213549

my_theme <- theme(text = element_text(family = my_font, color = font_color),
                  rect = element_rect(fill = base_color, color = NA), 
                  plot.background = element_rect(fill = base_color, color = NA), 
                  panel.background = element_rect(fill = base_color, color = NA), 
                  panel.border = element_blank(),
                  legend.background = element_rect(fill = base_color, color = NA),
                  legend.key = element_rect(fill = base_color),
                  plot.caption = element_text(size = 10, color = "#6173A5"),
                  plot.subtitle = element_text(size = 10),
                  axis.title.x = element_text(margin = margin(20,0,0,0), size = 14),
                  axis.title.y = element_text(margin = margin(0,20,0,0), size = 14),
                  axis.text.x = element_text(size = 12, color = '#99886F'),
                  axis.text.y = element_text(size = 12, color = '#99886F'),
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

data_by_year <- video_games %>% 
  filter(!is.na(publisher),!is.na(price), !is.na(metascore),!is.na(average_playtime))

metascore_avg <- data_by_year %>% 
  summarise(avg = mean(metascore, na.rm = TRUE)) %>% 
  pull(avg)

arrows <- tibble(
  x_start = c(2004,2007),
  x_end = c(2004.8,2008.5),
  y_start = c(43.5,36.5),
  y_end = c(78,72)
)


data_by_year %>% 
  mutate(year = year(mdy(release_date))) %>%
  group_by(year) %>% 
  mutate(yearly_avg = mean(metascore)) %>% 
  ungroup() %>% 
  ggplot(aes(year, metascore)) +
  geom_hline(yintercept = metascore_avg, color = "#f3f3f3", size = 2) +
  geom_jitter(color = '#84A6E1', alpha = .5, size = 1) +
  geom_point(aes(year, yearly_avg), shape = 21, fill = '#84A6E1', size = 6) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_x_continuous(breaks = seq(2004,2018,1), limits = c(2003,2019)) +
  theme(legend.position = "none") +
  annotate("text", x = 2004, y = 42.5, size = 4, color = "#f3f3f3",
           label = paste0("Yearly average")) + 
  annotate("text", x = 2008, y = 35, size = 4, color = "#f3f3f3",
           label = paste0("Overall metascore average")) + 
  geom_curve(data = arrows, aes(x = x_start, y = y_start, xend= x_end, yend = y_end),
             arrow = arrow(length = unit(0.08, "inch")), size = 0.8,
             color = "#CFC5AA", curvature = -0.3) +
  labs (title = "Video Game Metascore trends from 2004-2018",
        subtitle = "Excludes free to play games",
        x = "Year",
        y = "Metascore",
        caption = "Data from Liza Wood via Steam Spy\nVisualization by Jose M @Joseph_Mike")


ggsave("TidyTuesday_2019_07_30.png", width = 8, height = 5,device = "png", type = "cairo")
  
  

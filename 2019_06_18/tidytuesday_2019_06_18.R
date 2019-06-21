library(extrafont)

loadfonts(device = "win")

library(tidyverse)
library(ggthemes)
library(Cairo)
library(ggsci)

bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

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
                  axis.text.y = element_text(size = 12),
                  plot.title = element_text(margin = margin(0,0,20,0), size = 18))

theme_set(theme_light() + my_theme)

# Collect top 10 birds by total count
top_birds <- bird_counts %>%
  filter(year >= 2007) %>% 
  select(year, species, how_many_counted) %>% 
  group_by(species) %>% 
  summarize(count = sum(how_many_counted, na.rm = TRUE)) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  filter(count > quantile(count, .95))
  
# filter for top 10 birds
plot_data <- inner_join(filter(bird_counts, year >= 2007), top_birds, by = "species") %>% 
  select(-count) %>% 
  group_by(year) %>% 
  arrange(year, desc(how_many_counted)) %>% 
  mutate(rank = row_number())


plot_data %>% 
  ggplot(aes(year, rank, group = species)) +
  geom_line(aes(color = species, alpha = .9), size = 2) +
  geom_point(aes(color = species, alpha = .9), fill = "white", shape = 21, size = 3, stroke = 2) +
  scale_y_reverse(breaks = 1:10) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_x_continuous(breaks = seq(2007,2017,1), limits = c(2007,2020)) +
  scale_color_simpsons() +
  theme(legend.position = "none") +
  geom_text(data = filter(plot_data, year == 2017), 
            aes(x = 2017.2, label = species, color = species), 
            size = 4, hjust = "left", fontface = "bold") +
  labs (title = "Top 10 in the last 10",
        subtitle = "Species of birds counted during Christmas in Canada in the last decade",
        x = "Year",
        y = "Rank",
        caption = "Data from Bird Studies Canada @BirdsCanada\nVisualization by Jose M @Joseph_Mike")

ggsave("TidyTuesday_2019_06_18.png", width = 10, height = 6.5,device = "png", type = "cairo")

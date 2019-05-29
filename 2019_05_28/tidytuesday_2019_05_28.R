library(extrafont)

loadfonts(device = "win")

library(tidyverse)
library(ggthemes)
library(Cairo)
library(ggsci)


set.seed(123)

wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

# Set custom ggplot theme
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
                  plot.title = element_text(margin = margin(0,0,20,0), size = 10))

theme_set(theme_light() + my_theme)


# Remove duplicates
wr2 <- wine_ratings %>% 
  select(-X1) %>% 
  distinct()

# filter out NAs for country, price, and points
# select only necessary columns and view top countries
wr2 %>% 
  filter(!is.na(country),
         !is.na(price),
         !is.na(points)) %>% 
  select(country, price, points) %>% 
  group_by(country) %>% 
  tally(sort = TRUE)


# apply filters from above with additional filter on top 5 countries
# create a column for mean price for each country
# reorder countries
plot_data <- wr2 %>% 
  filter(!is.na(country),
         !is.na(price),
         !is.na(points),
         country %in% c("US","France","Italy","Spain","Portugal")) %>% 
  mutate(country = ifelse(country == "US", "United States", country)) %>%
  group_by(country) %>% 
  mutate(cntr_mean_price = mean(price),
         cntr_mean_points = mean(points)) %>% 
  ungroup() %>% 
  mutate(country = fct_reorder(country, cntr_mean_points))

world_avg <- wr2 %>% 
  summarise(avg = mean(points, na.rm = TRUE)) %>% 
  pull(avg)

arrows <- tibble(
  x_start = c(5.0,2.4,1.5,1.5),
  x_end = c(4.8,2,0.9,1.1),
  y_start = c(93,84,83,83),
  y_end = c(88.44,88,85,86)
)

  
plot_data %>%   
  ggplot(aes(country, points, color = country)) +
  geom_segment(aes(x = country, xend = country,
                   y = world_avg, yend = cntr_mean_points),
               size = 0.5) +
  geom_point(aes(country, cntr_mean_points), size = 4) +
  geom_jitter(size = 0.5, alpha = 0.05) +
  geom_hline(yintercept = world_avg, color = "#2D2D2D", size = 0.5) +
  coord_flip() +
  scale_y_continuous(limits = c(80,100), expand = c(0.005,0.005)) +
  theme(panel.grid.major.y = element_blank()) +
  scale_color_futurama() +
  theme(legend.position = "none") +
  annotate("text", x = 5.3, y = 93, size = 4, color = "#2D2D2D",
           label = glue::glue("Average points rating of\n{round(world_avg,1)} across all countries")) +
  annotate("text", x = 2.3, y = 84, size = 4, color = "#2D2D2D",
           label = paste0("Country average")) + 
  annotate("text", x = 1.6, y = 82, size = 4, color = "#2D2D2D",
           label = paste0("Wines\nper country")) + 
  geom_curve(data = arrows, aes(x = x_start, y = y_start, xend= x_end, yend = y_end),
             arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
             color = "#2D2D2D", curvature = -0.3) +
  labs(title = "Distribution of ratings for top 5 sampled countries",
       x = "",
       y = "Points rating (only 80 and above were scored)",
       caption = "Source: Kaggle \nVisualization by Jose M @Joseph_Mike")
  
ggsave("TidyTuesday_2019_05_28.png", width = 10, height = 6.5,device = "png", type = "cairo")


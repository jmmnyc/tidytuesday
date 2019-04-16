library(tidyverse)
library(ggthemes)
library(RColorBrewer)

player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")

grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")

grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

#set custom theme
my_font <- "Verdana"
base_color <- "#faf7ec"
font_color <- "#399694"
my_theme <- theme(text = element_text(family = my_font, face = "bold" ,color = font_color),
                  rect = element_rect(fill = base_color, color = NA), 
                  plot.background = element_rect(fill = base_color, color = NA), 
                  panel.background = element_rect(fill = base_color, color = NA), 
                  panel.border = element_blank(),
                  legend.background = element_rect(fill = base_color, color = NA),
                  legend.key = element_rect(fill = base_color),
                  plot.caption = element_text(size = 8),
                  axis.title.x = element_text(margin = margin(15,0,0,0)),
                  axis.title.y = element_text(margin = margin(0,15,0,0)),
                  axis.title = element_text(margin = margin (0,0,15,0))
)

theme_set(theme_light() + my_theme)



age_slams_comb <- left_join(grand_slams, player_dob, by = c("name")) %>% 
  mutate(age = tournament_date - date_of_birth) %>% # needs to be datetime
  group_by(name, age, gender) %>% 
  summarize(counts = n()) %>% 
  group_by(name) %>% 
  mutate(total_wins = cumsum(counts)) %>% 
  arrange(desc(total_wins)) %>% 
  ungroup() %>% 
  mutate(age = as.numeric(age/365)) 

over_10_wins <- age_slams_comb %>% 
  group_by(name, gender) %>% 
  select(name, total_wins) %>% 
  summarise(max_total_wins = max(total_wins)) %>%
  filter(max_total_wins >= 10) %>% 
  ungroup() %>% 
  group_by(gender) %>% 
  mutate(rank = dense_rank(max_total_wins)) %>% 
  ungroup()

brewer.pal((n=5, name = "Set1"))

color_map <- data.frame(rank = c(1:5), 
                        color = c("#E41A1c", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"))


age_slams_rank <- left_join(age_slams_comb, over_10_wins, by = c("name")) 

plot_data <- age_slams_rank %>% 
  mutate(
    line_col = case_when(
      rank == "1" ~ "1", 
      rank == "2" ~ "2", 
      rank == "3" ~ "3", 
      rank == "4" ~ "4", 
      rank == "5" ~ "5",
      T ~ "other")
    ) 
                      


  ggplot(data = plot_data, aes(age, total_wins, group = name, col = line_col)) +
  geom_step(alpha = 0.6) +
  geom_point(data = plot_data %>% 
               group_by(name) %>% 
               filter(total_wins == max(total_wins)), 
             aes(col = line_col), size = 1.5, alpha = 0.6) +
  facet_wrap(~gender.x, nrow = 2) +
  geom_text(data = plot_data %>% 
              group_by(name) %>% 
              filter(age == max(age)) %>% 
              ungroup(),
            aes(x = age, y = total_wins,label=ifelse(total_wins > 10,name,'')),hjust= -0.1,vjust= -0.3, size = 3) +
  labs(title = "Grand Slam victories by age",
       x = "Age",
       y = "Grand Slams won",
       caption = "\nData sourced from Wikipedia \nPlot by: Jose M. @Joseph_Mike") +
  scale_x_continuous(limits = c(15,40)) +
  scale_color_manual(values = c("#FF7F00", "#984EA3", "#4DAF4A", "#377EB8", "#E41A1c", "#7f7f7f")) +
  theme(strip.text.x = element_text(size = 14, color = "black")) +
  theme(legend.position = "none") 

  
  #transition_reveal(age_years)
  


library(extrafont)

loadfonts(device = "win")

library(tidyverse)
library(ggthemes)
library(Cairo)

student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

my_font <- "Lucida Sans"
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
                  plot.title = element_text(margin = margin(0,0,20,0), size = 14))

theme_set(theme_light() + my_theme)



plot_data <- student_ratio %>% 
  filter(country %in% c("Dominican Republic", "Cuba", "Puerto Rico", "Jamaica", "Guyana", 
                        "Belize", "Barbados", "Grenada", "Dominica", "Saint Lucia",
                        "Saint Vincent and the Grenadines","Bermuda", "Bahamas", "Antigua and Barbuda",
                        "Saint Kitts and Nevis", "Sint Maarten (Dutch part)", "Turks and Caicos Islands",
                        "British Virgin Islands", "Latin America and the Caribbean", "United States of America"),
         indicator %in% c("Primary Education", "Secondary Education", "Tertiary Education"),
         year == "2015") %>% 
  select(country, year, indicator, student_ratio) 


plot_data_2 <- student_ratio %>% 
  filter(str_detect(country, ".countries"),
         indicator %in% c("Primary Education", "Secondary Education", "Tertiary Education"),
         year == "2017") %>% 
  select(country, year, indicator, student_ratio)




plot_data_2 %>% 
  ggplot(aes(student_ratio, reorder(country, -student_ratio), fill = indicator)) +
  geom_point(shape = 21, size = 4, alpha = .6) +
  labs(title = "Less teachers per students in lower income countries in 2017",
       subtitle = "the impact is bigger in primary schools (elementary)",
       x = "Student:Teacher ratio (# of students per teacher)",
       y = "",
       caption = "Source: UNESCO Institute of Statistics \nVisualization by Jose M @Joseph_Mike",
       fill = "") +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("#984EA3", "#FF7F00")) #, "#4DAF4A"))"#984EA3"

ggsave("TidyTuesday_2019_05_07.png", device = "png", type = "cairo")
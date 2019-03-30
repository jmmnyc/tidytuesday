library(tidyverse)
library(lubridate)
library(ggthemes)

#read data
seattle_pets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")

#custom theme
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

#structure of data
str(seattle_pets)

#have a look at the data
head(seattle_pets)

#transform data that will be used for the plot
seattle_pets$date <- floor_date(mdy(seattle_pets$license_issue_date), "month")

plot_data <- seattle_pets %>% 
  filter(species %in% c("Cat", "Dog"), !is.na(animals_name), !is.na(zip_code), date >= '2015-01-01') %>% 
  group_by(species, date) %>% 
  summarise(pet_count = n()) 

ggplot(plot_data, aes(x = date, y = pet_count, col = species)) +
  geom_line() +
  labs(title = "Monthly pet registration trends in Seattle",
       x = "",
       y = "Number of pets registered",
       caption = "Plot: @Joseph_Mike \nData: Seattle's Open Data Portal") +
  theme(legend.position = "top", legend.title = element_blank())
  

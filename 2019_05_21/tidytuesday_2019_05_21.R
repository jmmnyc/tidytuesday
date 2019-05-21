library(extrafont)

loadfonts(device = "win")

library(tidyverse)
library(ggthemes)
library(Cairo)
library(janitor)
library(gridExtra)

coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")

# Set custom ggplot theme
my_font <- "Segoe UI Black"

base_color <- "#f5f3dc"

font_color <- "#42ac96" # or #213549

my_theme <- theme(text = element_text(family = my_font, color = font_color),
                  rect = element_rect(fill = base_color, color = NA), 
                  plot.background = element_rect(fill = base_color, color = NA), 
                  panel.background = element_rect(fill = base_color, color = NA), 
                  panel.border = element_blank(),
                  legend.background = element_rect(fill = base_color, color = NA),
                  legend.key = element_rect(fill = base_color),
                  plot.caption = element_text(size = 7, color = "#223a4f"),
                  axis.title.x = element_text(margin = margin(20,0,0,0)),
                  axis.title.y = element_text(margin = margin(0,20,0,0)),
                  plot.title = element_text(margin = margin(0,0,20,0), size = 10))

theme_set(theme_light() + my_theme)



# Clean names
coast_vs_waste <- clean_names(coast_vs_waste)

# Let's find out the years included
# Seems we have over 200 countries from around mid 1950's until 2013

coast_vs_waste %>% 
  group_by(year) %>% 
  tally() %>% 
  ggplot(aes(x = year, y = n)) +
  geom_col()

coast_vs_waste %>% 
  group_by(year) %>% 
  tally() %>% 
  tail()

# Let's take a look at the data on mismanged waste
# Looks like we only have data for 2010

coast_vs_waste %>% 
  filter(!is.na(mismanaged_plastic_waste_tonnes)) %>% 
  group_by(entity, year) %>% 
  summarise(mismanaged_plastic_waste_tonnes) %>% 
  View()

# Let's filter to focus 2010 and create a measure for % of population that is coastal

plot_data <- coast_vs_waste %>% 
  filter(year == 2010) %>% 
  mutate(coastal_pop_pct = coastal_population/total_population_gapminder) 


# I would expect to see population size and waste are positively correlated

waste_pop <- plot_data %>% 
  #filter(entity == "Dominican Republic") %>% 
  ggplot(aes(x = log(total_population_gapminder), y = log(mismanaged_plastic_waste_tonnes))) +
  geom_point(aes(col = entity), size = 2, alpha = .7) +
  geom_smooth(method = "lm") +
  theme(legend.position = "none") +
  labs(title = "Countries with larger populations produce more plastic waste (2010)",
       x = "Total population (log scale)",
       y = "Mismanaged plastic waste in tonnes (log scale)",
       caption = "") 


# Maybe countires with larger coastal populations behave differently

waste_coast_pop <- plot_data %>% 
  #filter(entity == "Dominican Republic") %>% 
  ggplot(aes(x = coastal_pop_pct, y = log(mismanaged_plastic_waste_tonnes))) +
  geom_point(aes(col = entity), size = 2, alpha = .7) +
  geom_vline(xintercept = 1) +
  geom_text(aes(x = 1, y = 15, 
                label = "Some data points might have errors \n    with coastal pop greater total",
                hjust = -0.1),
                size = 3) +
  geom_smooth(method = "lm") +
  theme(legend.position = "none") +
  labs(title = "Lower waste produced in countries with higher % of coastal population",
       x = "Coastal population as % of total",
       y = "",
       caption = "Source: Our World in Data \nVisualization by Jose M @Joseph_Mike") 


combined_plots <- arrangeGrob(waste_pop, waste_coast_pop, ncol = 2)


ggsave("TidyTuesday_2019_05_21.png", combined_plots, width = 12, height = 4, device = "png", type = "cairo")
  
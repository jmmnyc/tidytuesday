#load packages
library(tidyverse)
library(ggthemes)
library(scales)

jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")

jobs_gender$minor_category <- as.factor(jobs_gender$minor_category)

plot_data <- jobs_gender %>% 
  filter(year == 2016) %>%
  group_by(minor_category) %>% 
  summarise(pct_female_workers = sum(workers_female, na.rm = TRUE)/sum(total_workers, na.rm = TRUE),
            pct_female_wages = sum(total_earnings_female, na.rm = TRUE)/sum(total_earnings_male, na.rm = TRUE)) %>% 
  ungroup() 
  
plot_data %>%   
  ggplot(aes(x = reorder(minor_category, pct_female_wages), y = pct_female_wages)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = "dashed", col = "red") +
  theme_economist_white() +
  labs(title = "Female wages as a % of Male wages by Industry",
       subtitle = "100% = pay parity",
       x = "",
       y = "Percent") +
  coord_flip() +
  theme(axis.text.y = element_blank()) +
  geom_text(aes(x = reorder(minor_category, pct_female_wages), y = .35, label = minor_category), size = 4, col = "white") +
  scale_y_continuous(labels = percent_format(accuracy = 1))

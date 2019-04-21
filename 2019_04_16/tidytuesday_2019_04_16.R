library(tidyverse)
library(scales)
library(ggthemes)

women_research <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")





women_research %>% 
  ggplot(aes(x = reorder(country, -percent_women), y = percent_women, group = field, fill = field)) +
  geom_point(size = 3, shape = 21, alpha = .6) +
  theme_economist() +
  geom_hline(yintercept = .5, linetype = "dashed", col = "red") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,0.65)) +
  labs(title = "Still a man's world...",
       subtitle = "Women among researchers with papers published 2011-2015",
       y = "% of Research papers published by Women",
       x = "Country",
       caption = "\nSource:  'Gender in the Global Research Landscape' by Elsevier; The Economist \nPlot by Jose M. @Joseph_Mike") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 8)) +
  scale_fill_discrete(labels = c("computer science, math", "engineering", "health sciences", "physical sciences", "inventors")) +
  coord_flip()
  


library(tidyverse)
library(ggthemes)

phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")

data <- phd_field %>% 
  filter(major_field == "Mathematics and statistics") %>% 
  group_by(year, field) %>% 
  summarise(total_phds = sum(n_phds, na.rm = TRUE)) %>% 
  ungroup()

ggplot(data, aes(year, total_phds, color = field)) +
  geom_line() +
  theme_economist_white() +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(2008,2021), breaks = 2008:2017) +
  geom_text(data = data %>% filter(year == "2017"), aes(label = field), hjust = -.05, vjust = .5, size = 2) +
  labs(title = "Has the ML/AI hype had an impact on Math & Stats PhDs?",
       subtitle = "I was expecting a stronger lift, but it might be lagged as PhDs take several years to complete",
       x = "Year",
       y = "Number of PhDs awarded",
       caption = "\n Source: NSF https://ncses.nsf.gov/pubs/nsf19301/data \nBy: Jose M @Joseph_Mike")

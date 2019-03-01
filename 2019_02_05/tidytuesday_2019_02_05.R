library(tidyverse)

state_hpi <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/state_hpi.csv")
mortgage_rates <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/mortgage.csv")
recession_dates <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/recessions.csv")


difference_vs_national <- state_hpi %>%  group_by(year,state) %>% 
  summarise(state_annual_avg = mean(price_index), national_annual_avg = mean(us_avg)) %>% 
  mutate(difference_vs_national = state_annual_avg - national_annual_avg) %>% 
  select(year, state, difference_vs_national) 

difference_vs_national$vs_National <- ifelse(difference_vs_national$difference_vs_national>0,"above","below")

difference_vs_national %>% 
  filter(state == "NJ") %>% 
  ggplot(aes(x = year, y = difference_vs_national, fill = vs_National)) + 
  geom_col() + scale_fill_brewer(palette = "Set2") + 
  theme_bw() + geom_hline(yintercept = 0, color = "#333333") + 
  labs(title = "Annual House Price Index trend vs. National Average", 
       x = "Year", 
       y = "Difference vs. National avg",
       caption = "\n Source: Freddie Mac House Price Index\n by: Jose M @sanzbx") + 
  theme(legend.position = "top")
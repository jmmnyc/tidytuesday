library(tidyverse)
library(scales)
library(gridExtra)

spend_df<- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")

spend_percent <- spend_df %>% 
  filter(rd_budget > 0) %>%
  group_by(department, year) %>% 
  summarise( pct_of_total = rd_budget/total_outlays)

plot_1 <- ggplot(spend_df, aes(x = year, y = rd_budget/1000000000)) +
  geom_line() +
  geom_smooth(method = lm, se = FALSE) +
  theme_gdocs() +
  labs(title = "Annual R&D spend \nby deparment",
       subtitle = "Upward trend in spending in most departments yet...",
       x = "Year",y = "Budget Spend (Billions)") +
  theme(strip.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold")) +
  scale_y_continuous(labels = dollar_format()) +
  facet_wrap(~department, ncol=2,  scales = "free_y")

plot_2 <- ggplot(spend_percent, aes(x = year, y = pct_of_total)) +
  geom_line() +
  geom_smooth(method = lm, se = FALSE) +
  theme_gdocs() +
  labs(title = "Annual R&D spend % of total \nby deparment",
       subtitle = "...all departments show a downward trend as a percent of total spend",
       x = "Year",y = "Percent of Total Spend") +
  theme(strip.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold")) +
  scale_y_continuous(labels = percent_format(accuracy = .1)) +
  facet_wrap(~department, ncol=2,  scales = "free_y")

grid.arrange(plot_1,plot_2, ncol = 2)



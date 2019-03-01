#load packages
library(tidyverse)
library(ggthemes)
library(scales)

#get data
full_trains <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")

#create combined date field in order to chart monthly trends across years
full_trains$date <- as.Date(with(full_trains, paste(year, month, "1", sep = "-")), format = "%Y-%m-%d")

#transform the data to get % of trips that are late and count distinct stations
monthly_trips <- full_trains %>%
  group_by(date) %>% 
  summarise(monthly_pct_late = sum(num_late_at_departure) / sum(total_num_trips),
            stations = n_distinct(departure_station)) %>% 
  ungroup()

#plot
plot <- ggplot(monthly_trips, aes(x = date, y = monthly_pct_late)) +
  geom_line(col = "white", size = 1) +
  theme_dark() +
  labs(title = "How has the % of delayed departures changed in 2018?",
       x = "",
       y = "% of trains delayed on departure") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_date(limits = as.Date(c('2015-01-01', '2019-02-01')))

plot

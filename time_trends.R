library("dplyr")
library("ggplot2")
library(tidyverse)

# load data lmao
data <- read.csv("us-prison-jail-rates-1990.csv")

# Plot lmao
chart_data <- select(data, year, aapi_prison_pop_rate, black_prison_pop_rate, latinx_prison_pop_rate, native_prison_pop_rate, white_prison_pop_rate)

new_name <- c("Year", "Asian/Pacific Islander American", "African American", "Hispanic", "Indigenous", "White")

colnames(chart_data) <- new_name

data_long <- gather(chart_data, key = "Ethnicity", value = "IncarcerationRate", -Year) %>% group_by(Year, Ethnicity) %>% summarise(AverageIncarcerationRate = mean(IncarcerationRate, na.rm = TRUE))

ggplot(data = data_long, aes(x = Year, y = AverageIncarcerationRate, group = Ethnicity, color = Ethnicity)) +
  geom_line() + 
  theme_minimal() + 
  labs(title = "Average Incarceration by Ethnicity", x = "Year", y = "Avg. Incarceration Per 100,000 People") +
  scale_color_brewer(palette = "Set1") # cool colors


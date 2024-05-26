library("dplyr")
library("ggplot2")
library(tidyverse)
library(patchwork)

# load data lmao
data <- read.csv("us-prison-jail-rates-1990.csv")

# Explore the stuff
prison_data <- select(data, year, aapi_prison_pop_rate, black_prison_pop_rate, latinx_prison_pop_rate, native_prison_pop_rate, white_prison_pop_rate)

jail_data <- select(data, year, aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate, native_jail_pop_rate, white_jail_pop_rate)

new_name <- c("Year", "Asian/PI", "Black", "Hispanic", "Indigenous", "White")

colnames(prison_data) <- new_name
colnames(jail_data) <- new_name

jail_sum <- gather(jail_data, key = "Ethnicity", value = "IncarcerationRate", -Year) %>% group_by(Year, Ethnicity) %>% summarise(AverageIncarcerationRate = mean(IncarcerationRate, na.rm = TRUE)) %>% filter(Year == 2016)

prison_sum <- gather(prison_data, key = "Ethnicity", value = "IncarcerationRate", -Year) %>% group_by(Year, Ethnicity) %>% summarise(AverageIncarcerationRate = mean(IncarcerationRate, na.rm = TRUE)) %>% filter(Year == 2016)

jplot <- ggplot(jail_sum, aes(x = Ethnicity, y = AverageIncarcerationRate)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title = "Jail population by ethnicity, 2016", x = "Ethnicity", y = "Avg. Incarceration Rate per 100,000")

pplot <- ggplot(prison_sum, aes(x = Ethnicity, y = AverageIncarcerationRate)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title = "Prison population by ethnicity, 2016", x = "Ethnicity", y = "Avg. Incarceration Rate per 100,000")

jplot + pplot
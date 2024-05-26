library("dplyr")
library("ggplot2")
library(maps)
library(tidyverse)

data <- read.csv("us-prison-jail-rates-1990.csv")

# Found from the internet
state_abbreviations <- data.frame(
  state_abb = state.abb,
  state = tolower(state.name)
) %>% rbind(c("DC", "district of columbia"))
# Default dictionary doesn't have DC.

map_data <- filter(data, year == 2016) %>% select(state, total_prison_pop_rate) %>% group_by(state) %>% summarise(AverageIncarcerationRate = mean(total_prison_pop_rate, na.rm = TRUE)) %>% left_join(state_abbreviations, by = c("state" = "state_abb"))

# remove the abbreviations 

map_data <- select(map_data, -state)

# Most of this code is from the textbook
state_shape <- map_data("state") %>% rename(state = region) %>% left_join(map_data, by = c("state" = "state.y"))

ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = AverageIncarcerationRate),
    color = "white",
    size = .1        
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Rate of prison incarceration") +
  labs(title = "Geographic prison incarceration rates, 2016", x = "", y= "") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )




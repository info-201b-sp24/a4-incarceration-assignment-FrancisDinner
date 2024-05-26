library("dplyr")
library("knitr")

# load data lmao
data <- read.csv("us-prison-jail-rates-1990.csv")

# Calculate what years are covered by the data
range_dates <- select(data, year) %>% range()
range_dates <- paste(range_dates[1], "-", range_dates[2])

# Count how many counties are in the dataset.
number_counties <- nrow(filter(data, year == 2018) %>% select(county_name))

# What is the average rate of incarceration in prisons?

## test <- select(data, total_prison_pop_rate, year)
# 2016 is the latest data for prisons!!!

avg_rate <- filter(data, year == 2016) %>% select(total_prison_pop_rate) %>% summarize(avg_value = mean(total_prison_pop_rate, na.rm = TRUE)) %>% pull(avg_value)

# What is the highest rate of incarceration for prisons?
high_val <- filter(data, year == 2016) %>% select(total_prison_pop_rate) %>% top_n(1) %>% pull(total_prison_pop_rate)

# What state has the highest reported average rate of prison incarceration in 2016??
prison_state <- filter(data, year == 2016) %>% select(total_prison_pop_rate, state) %>% na.omit() %>% group_by(state) %>% summarise(average = mean(total_prison_pop_rate)) %>% top_n(1, average) %>% pull(state)

# oh dont forget that this is rates per 100,000 people

# ok create the nice table

table_names <- c("Year Range", "Number of Counties", "Average Prison Incarceration Rate", "Highest Rate of Prison Incarcerations", "State w/ Highest Incarceration")

table_vals <- c(range_dates, number_counties, avg_rate, high_val, prison_state)

df <- data.frame(Detail = table_names, Values = table_vals)

kable(df)

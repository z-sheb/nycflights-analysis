# installing packages 
install.packages('nycflights13')

# loading required libraries
library(tidyverse)
library(ggplot2)
library(nycflights13) 
library(dplyr)

# selecting the wind speed at JFK airport from weather file 
jfk_wind_speed = weather %>%
  filter(origin == "JFK") %>%
  select(month, wind_speed)

View(weather)

# figure 1: monthly summary of wind speed at JFK airport
fig1 = ggplot(jfk_wind_speed, aes(x = factor(month), y = wind_speed)) + 
  geom_boxplot(fill = "skyblue", outlier.color = "red") +
  labs(x = "Month", y = "Windspeed (mph)",
       title = "Windspeed By Month", 
       subtitle = "Collected from JFK Airport in 2013") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# showing figure 1
fig1

# calculating mean, median, max, min, standard of deviation, and IQR
by_month = group_by(jfk_wind_speed, month)
wind_speed_month = summarize(by_month,
                             Range = range(wind_speed),
                             Mean = mean(wind_speed),
                             Median = median(wind_speed),
                             MAX = max(wind_speed),
                             SD = sd(wind_speed),
                             IQR = IQR(wind_speed, na.rm=TRUE)
                             )

# viewing calculated values for analysis
View(wind_speed_month)

# filtering out flights that were not delayed
# selecting flights in January originating from JFK airport
delayed_jfk_flights = flights %>%
  filter(dep_delay > 0, origin == "JFK", month == "1") %>%
  select(month, origin, dep_delay, day, time_hour)

# grouping delayed flights by day
by_day = group_by(delayed_jfk_flights, day)

# calculating average delay by per day
# ignoring NA values
jfk_sum_delay_count = summarize(by_day, total_delay = mean(dep_delay, na.rm=TRUE),
                            count = n())

# selecting weather at JFK airport in January
jfk_weather = weather %>%
  filter(origin == "JFK", month == "1") %>%
  select(month, origin, wind_speed, day)

# merging sum_delay_count and jfk_weather data sets by day
flights_weather = merge(jfk_sum_delay_count, jfk_weather, by = c("day"))

# grouping by wind speed
delay_wind_speed = group_by(flights_weather, wind_speed)

#calculating average departure delay and wind speed
avg_delay_ws = summarize(delay_wind_speed, 
                         avg_dep_delay = mean(total_delay, na.rm=TRUE))

# figure 2: plotting wind speed vs average departure delay
fig2 = ggplot(avg_delay_ws, aes(x = wind_speed, y = avg_dep_delay)) + 
  geom_point() + geom_smooth(method = "lm", color = "skyblue") +
  labs(x = "Wind Speed (mph)", 
       y="Average Departure Delay Time (minutes)",
       title = "Average Departure Delay vs. Wind Speed in January", 
       subtitle = "Collected from JFK Airport in 2013")

# showing figure 2
fig2
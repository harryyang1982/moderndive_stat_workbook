library(nycflights13)
library(tidyverse)

# 5NG#1: Scatterplots

alaska_flights <- flights %>% 
  filter(carrier == "AS")

ggplot(alaska_flights, aes(x = dep_delay, y = arr_delay)) +
  geom_point()

# Method 1: Changing the Transparency

ggplot(alaska_flights, aes(x = dep_delay, y = arr_delay)) +
  geom_point(alpha = 0.2)

# Method 2: Jittering the Points

ggplot(alaska_flights, aes(x = dep_delay, y = arr_delay)) +
  geom_jitter(width = 30, height = 30)

# 5NG#2 : Linegraphs

data(weather)
weather

early_january_weather <- weather %>% 
  filter(origin == "EWR" & month == 1 & day <= 15)

ggplot(data=early_january_weather, aes(x = time_hour, y = temp)) +
  geom_line()

ggplot(early_january_weather, aes(x = time_hour, y = humid)) +
  geom_line()

# 5NG#3 : Histogram

ggplot(weather, aes(x = temp)) +
  geom_histogram()

ggplot(weather, aes(x = temp)) +
  geom_histogram(color = "white")

ggplot(weather, aes(x = temp)) +
  geom_histogram(color = "white", fill = "steelblue")

ggplot(weather, aes(x = temp)) +
  geom_histogram(color = "white", bins = 40)

ggplot(weather, aes(x = temp)) +
  geom_histogram(color = "white", binwidth = 10)

# Facets
ggplot(weather, aes(x = temp)) +
  geom_histogram(binwidth = 5, color = "white") +
  facet_wrap(~ month)

ggplot(weather, aes(x = temp)) +
  geom_histogram(binwidth = 5, color = "white") +
  facet_wrap(~ month, nrow = 4)

# 5NG#4: Boxplots

ggplot(weather, aes(x = month, y = temp)) +
  geom_boxplot()

ggplot(weather, aes(x = factor(month), y = temp)) +
  geom_boxplot()

# 5NG#5: Barplots

fruits <- tibble(
  fruit = c("apple", "apple", "orange", "apple", "orange")
)
fruits_counted <- tibble(
  fruit = c("apple", "orange"),
  number = c(3, 2)
)
fruits
fruits_counted

ggplot(fruits, aes(x = fruit)) +
  geom_bar()

ggplot(fruits_counted, aes(x = fruit, y = number)) +
  geom_col()

ggplot(flights, aes(x = carrier)) +
  geom_bar()

ggplot(flights, aes(x = fct_infreq(carrier))) +
  geom_bar()

ggplot(flights, aes(x = fct_rev(fct_infreq(carrier)))) +
  geom_bar()

ggplot(flights, aes(x = carrier)) +
  geom_bar()

ggplot(flights, aes(x = carrier, fill = origin)) +
  geom_bar()

ggplot(flights, aes(x = carrier, color = origin)) +
  geom_bar()

ggplot(flights, aes(x = carrier), fill = origin) +
  geom_bar()

ggplot(flights, aes(x = carrier, fill = origin)) +
  geom_bar(position = "dodge")

ggplot(flights, aes(x = carrier, fill = origin)) +
  geom_bar(position = position_dodge(preserve = "single"))

ggplot(flights, aes(x = carrier)) +
  geom_bar() +
  facet_wrap(~origin, ncol=1)


library(tidyverse)
library(nycflights13)

# pipe

alaska_flights <- flights %>% 
  filter(carrier == "AS")

# filter rows

portland_flights <- flights %>% 
  filter(dest == "PDX")
View(portland_flights)

btv_sea_flights_fall <- flights %>% 
  filter(origin == "JFK" & (dest == "BTY" | dest == "SEA") & month >= 10)
View(btv_sea_flights_fall)

not_BTA_SEA <- flights %>% 
  filter(!(dest == "BTV" | dest == "SEA"))
View(not_BTA_SEA)

many_airports <- flights %>% 
  filter(dest %in% c("SEA", "SFO", "PDX", "BTV", "BDL"))
View(many_airports)

# summarize

summary_temp <- weather %>% 
  summarize(mean = mean(temp), std_dev = sd(temp))
summary_temp

summary_temp <- weather %>% 
  summarize(mean = mean(temp, na.rm = TRUE), std_dev = sd(temp, na.rm = TRUE))
summary_temp

# groupby
summary_monthly_temp <- weather %>% 
  group_by(month) %>% 
  summarize(mean = mean(temp, na.rm = TRUE),
            std_dev = sd(temp, na.rm = TRUE))
summary_monthly_temp

diamonds

data(diamonds)

diamonds %>% 
  group_by(cut) %>% 
  summarize(avg_price = mean(price))

diamonds %>% 
  group_by(cut) %>% 
  ungroup()

by_origin <- flights %>% 
  group_by(origin) %>% 
  summarize(count = n())

by_origin_monthly <- flights %>% 
  group_by(origin, month) %>% 
  summarize(count = n())

by_origin_monthly

by_origin_monthly_incorrect <- flights %>% 
  group_by(origin) %>% 
  group_by(month) %>% 
  summarize(count = n())

by_origin_monthly_incorrect

# mutate
weather <- weather %>% 
  mutate(temp_in_C = (temp - 32) / 1.8)
weather

summary_monthly_temp <- weather %>% 
  group_by(month) %>% 
  summarize(mean_temp_in_F = mean(temp, na.rm = TRUE),
            mean_temp_in_C = mean(temp_in_C, na.rm = TRUE))

summary_monthly_temp

flights <- flights %>% 
  mutate(gain = dep_delay - arr_delay)
flights

gain_summary <- flights %>% 
  summarize(
    min = min(gain, na.rm = TRUE),
    q1 = quantile(gain, 0.25, na.rm = TRUE),
    median = median(gain, na.rm = TRUE),
    q3 = quantile(gain, 0.75, na.rm = TRUE),
    max = max(gain, na.rm = TRUE),
    sd = sd(gain, na.rm = TRUE),
    missing = sum(is.na(gain))
  )
gain_summary

ggplot(flights, aes(x = gain)) +
  geom_histogram(color = "white", bins = 20)

# arrange

freq_dest <- flights %>% 
  group_by(dest) %>% 
  summarize(num_flights = n())

freq_dest %>% 
  arrange(num_flights)

freq_dest %>% 
  arrange(-num_flights)

freq_dest %>% 
  arrange(desc(num_flights))

# join
View(airlines)

flights_joined <- flights %>% 
  inner_join(airlines, by = "carrier")

flights_joined %>% 
  select(17:21)

flights_with_airport_names <- flights %>% 
  inner_join(airports, by = c("dest" = "faa"))
View(flights_with_airport_names)

named_dests <- flights %>% 
  group_by(dest) %>% 
  summarize(num_flights = n()) %>% 
  arrange(desc(num_flights)) %>% 
  inner_join(airports, by = c("dest" = "faa")) %>% 
  rename(airport_name = name)
named_dests

flights_weather_joined <- flights %>% 
  inner_join(weather, by = c("year", "month", "day", "hour", "origin"))
View(flights_weather_joined)

joined_flights <- flights %>% 
  inner_join(airlines, by = "carrier")
View(joined_flights)

# top_n

named_dests %>% 
  top_n(n = 10, wt = num_flights)

# exercise
data(package = 'nycflights13')
planes %>% 
  inner_join(airlines, by = 'carrier')
airlines
planes

flights %>% 
  inner_join(planes, by = c('year', 'tailnum'))

data(planes)
planes

flights %>% 
  group_by(year, tailnum) %>% 
  summarize(flight = sum(flight)) %>% 
  ungroup() %>% 
  inner_join(planes, by = c('year', 'tailnum')) %>% 
  mutate(asm = seats * flight) %>% 
  group_by(tailnum) %>% 
  summarize(asm = mean(asm))



flights_joined %>% 
  inner_join(planes %>% 
               select(-year), by = 'tailnum') %>% 
  mutate(asm = seats * flight) %>% 
  group_by(carrier, name) %>% 
  summarize(asm = sum(asm)) %>% 
  arrange(-asm)

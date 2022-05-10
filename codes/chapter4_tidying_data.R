library(tidyverse)
library(nycflights13)
library(fivethirtyeight)


dem_score <- read_csv("https://moderndive.com/data/dem_score.csv")
dem_score

data(drinks)

drinks_smaller <- drinks %>% 
  filter(country %in% c("USA", "China", "Italy", "Saudi Arabia")) %>% 
  select(-total_litres_of_pure_alcohol,
         beer = beer_servings,
         spirit = spirit_servings,
         wine = wine_servings)
drinks_smaller

ggplot(drinks_smaller %>% 
         pivot_longer(beer:wine, names_to = "type", values_to = "servings")) +
  geom_col(aes(x = country,
               y = servings,
               fill = type),
           position = "dodge")

data(airline_safety)

airline_safety_smaller <- airline_safety %>% 
  select(airline, starts_with("fatalities"))
airline_safety_smaller %>% 
  pivot_longer(fatalities_85_99:fatalities_00_14, names_to = "fatalities_years", values_to = "value") %>% 
  ggplot(aes(x = airline,
             y = value,
             fill = fatalities_years)) +
  geom_col(position = "dodge")

# 4.3 Case Study: Democracy in Guatemala

guat_dem <- dem_score %>% 
  filter(country == "Guatemala")
guat_dem

## names_ptype to names_transform
guat_dem_tidy <- guat_dem %>% 
  pivot_longer(names_to="year", values_to="score", `1952`:`1992`,
               names_transform = list(year = as.integer))

guat_dem_tidy <- guat_dem %>% 
  pivot_longer(names_to="year", values_to="score",
               cols=-country,
               names_transform = list(year = as.integer))

ggplot(guat_dem_tidy, aes(x = year, y = score)) +
  geom_line() +
  labs(x = 'Year', y = 'Democracy Score')

dem_score_tidy <- dem_score %>% 
  pivot_longer(names_to='year',
               values_to='score',
               cols=-country,
               names_transform = list(year = as.integer))

le <- read_csv("https://moderndive.com/data/le_mess.csv")

le %>% 
  pivot_longer(names_to = 'year',
               values_to = 'life_expectancy',
               cols = -country) -> le_tidy


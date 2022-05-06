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

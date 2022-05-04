if (!require('nycflights13')) install.packages("nycflights13")

library(tidyverse)
library(nycflights13)
library(knitr)

data(flights)
flights

glimpse(flights)

kable(airlines)

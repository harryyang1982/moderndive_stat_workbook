library(tidyverse)
library(moderndive)
library(infer)

# 8.1 Pennies activity
# 8.1.1 What is the average year on US pennies in 2019?

data("pennies_sample")
pennies_sample

ggplot(pennies_sample, aes(x = year)) +
  geom_histogram(binwidth = 10, color = "white")

pennies_sample %>% 
  summarize(mean_year = mean(year))

mean(pennies_sample$year)

pennies_resample <- tibble(
  year = c(1976, 1962, 1976, 1983, 2017, 2015, 2015, 1962, 2016, 1976,
           2006, 1997, 1988, 2015, 2015, 1988, 2016, 1978, 1979, 1997,
           1974, 2013, 1978, 2015, 2008, 1982, 1986, 1979, 1981, 2004,
           2000, 1995, 1999, 2006, 1979, 2015, 1979, 1998, 1981, 2015,
           2000, 1999, 1988, 2017, 1992, 1997, 1990, 1988, 2006, 2000)
)

p1 <- ggplot(pennies_resample, aes(x = year)) +
  geom_histogram(binwidth = 10, color = "white") +
  labs(title = "Resample of 50 pennies")
p2 <- ggplot(pennies_sample, aes(x = year)) +
  geom_histogram(binwidth = 10, color = "white") +
  labs(title = "Original sample of 50 pennies")

library(gridExtra)
grid.arrange(p1, p2, ncol = 1)
pennies_resample %>% 
  summarize(mean_year = mean(year))
mean(pennies_resample$year)

# 8.1.3 Resampling 35 times

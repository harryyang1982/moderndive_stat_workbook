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

ggplot(pennies_resample, aes(x = year)) +
  geom_histogram(binwidth = 10, color = "white") +
  labs(title = "Resample of 50 pennies")
ggplot(pennies_sample, aes(x = year)) +
  geom_histogram(binwidth = 10, color = "white") +
  labs(title = "Original sample of 50 pennies")

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

data("pennies_resamples")
pennies_resamples

resampled_means <- pennies_resamples %>% 
  group_by(name) %>% 
  summarize(mean_year = mean(year))

resampled_means

ggplot(resampled_means, aes(x = mean_year)) +
  geom_histogram(binwidth = 1, color = 'white', boundary = 1990) +
  labs(x = "Sampled mean year")

# 8.1.4 What did we just do?

# 8.2 Computer simulation of resampling
## 8.2.1 Virtually resampling once

virtual_shovel <- bowl %>% 
  rep_sample_n(size = 50)

virtual_resample <- pennies_sample %>% 
  rep_sample_n(size = 50, replace = TRUE)

virtual_resample

virtual_resample %>% 
  summarize(resample_mean = mean(year))

## 8.2.2 Virtually resampling 35 times
virtual_resamples <- pennies_sample %>% 
  rep_sample_n(size = 50, replace = TRUE, reps = 35)
virtual_resamples

virtual_resampled_means <- virtual_resamples %>% 
  group_by(replicate) %>% 
  summarize(mean_year = mean(year))

virtual_resampled_means

mean(virtual_resampled_means$mean_year)

ggplot(virtual_resampled_means, aes(x = mean_year)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 1990) +
  labs(x = "Resample mean year")

## 8.2.3 Virtually resampling 1000 times

# Repeat resampling 1000 times
virtual_resamples <- pennies_sample %>% 
  rep_sample_n(size = 50, replace = TRUE, reps = 1000)

# Compute 1000 sample means
virtual_resampled_means <- virtual_resamples %>% 
  group_by(replicate) %>% 
  summarize(mean_year = mean(year))

virtual_resampled_means

ggplot(virtual_resampled_means, aes(x = mean_year)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 1990) +
  labs(x = "sample mean")

virtual_resampled_means %>% 
  summarize(mean_of_means = mean(mean_year))

# 8.3 Understanding confidence intervals
virtual_resampled_means %>% 
  summarize(SE = sd(mean_year))

# 8.4 Constructing confidence intervals
## 8.4.1 Original workflow
pennies_sample %>% 
  rep_sample_n(size = 50, replace = TRUE, reps = 1000)

pennies_sample %>% 
  rep_sample_n(size = 50, replace = TRUE, reps = 1000) %>% 
  group_by(replicate) %>% 
  summarize(mean_year = mean(year))

## 8.4.2 infer package workflow
pennies_sample %>% 
  summarize(stat = mean(year))

pennies_sample %>% 
  specify(response = year) %>% 
  calculate(stat="mean")

pennies_sample %>% 
  specify(response = year)

pennies_sample %>% 
  specify(formula = year ~ NULL)

pennies_sample %>% 
  specify(response = year) %>% 
  generate(reps = 1000, type = "bootstrap")

# infer workflow:
pennies_sample %>% 
  specify(response = year) %>% 
  generate(reps = 1000)

# Original workflow
pennies_sample %>% 
  rep_sample_n(size = 50, replace = TRUE,
               reps = 1000)

bootstrap_distribution <- pennies_sample %>% 
  specify(response = year) %>% 
  generate(reps = 1000) %>% 
  calculate(stat = "mean")

bootstrap_distribution %>% 
  .[['stat']] %>% mean()

# infer workflow
pennies_sample %>% 
  specify(response = year) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "mean")

# Original workflow:
pennies_sample %>% 
  rep_sample_n(size = 50, replace = TRUE,
               reps = 1000) %>% 
  group_by(replicate) %>% 
  summarize(stat = mean(year))

# infer workflow:
visualize(bootstrap_distribution)

# Original workflow:
ggplot(bootstrap_distribution, aes(x = stat)) +
  geom_histogram()

## 8.4.3 Percentile method with infer
percentile_ci <- bootstrap_distribution %>% 
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci

visualize(bootstrap_distribution) +
  shade_confidence_interval(endpoints = percentile_ci)

visualize(bootstrap_distribution) +
  shade_ci(endpoints = percentile_ci, color = "hotpink", fill = "khaki")

## 8.4.4 Standard error method with infer
x_bar <- 1995.44

standard_error_ci <- bootstrap_distribution %>% 
  get_confidence_interval(type = "se", point_estimate = x_bar)

visualize(bootstrap_distribution) +
  shade_confidence_interval(endpoints = standard_error_ci)

# 8.5 Interpreting confidence intervals
bowl %>% 
  summarize(p_red = mean(color == "red"))

bowl
bowl_sample_1 %>% 
  summarize(p_red = mean(color == "red"))

sample_1_bootstrap <- bowl_sample_1 %>% 
  specify(response = color, success = "red") %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate("prop")

sample_1_bootstrap

percentile_ci_1 <- sample_1_bootstrap %>% 
  get_ci(level = 0.95, type = "percentile")

percentile_ci_1
sample_1_bootstrap %>% 
  visualize(bins = 15) +
  shade_confidence_interval(endpoints = percentile_ci_1) +
  geom_vline(xintercept = 0.375, linetype = "dashed")

bowl_sample_2 <- bowl %>% rep_sample_n(size = 50)
bowl_sample_2

sample_2_bootstrap <- bowl_sample_2 %>% 
  specify(response = color,
          success = "red") %>% 
  generate(reps = 1000,
           type = "bootstrap") %>% 
  calculate(stat = "prop")

sample_2_bootstrap

percentile_ci_2 <- sample_2_bootstrap %>% 
  get_ci(level = 0.95, type = "percentile")
percentile_ci_2

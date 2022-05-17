library(tidyverse)
library(moderndive)
library(infer)
library(nycflights13)
library(ggplot2movies)

# 9.1 Promotions activity
## 9.1.1 Does gender affect promotions at a bank?

data(promotions)

promotions %>% 
  sample_n(size = 6) %>% 
  arrange(id)

p1 <- ggplot(promotions, aes(x = gender, fill = decision)) +
  geom_bar() +
  labs(x = "Gender of name on resume")

promotions %>% 
  group_by(gender, decision) %>% 
  count() # same as tally ()


promotions %>% 
  group_by(gender, decision) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(gender) %>% 
  mutate(pct = n / sum(n))

## 9.1.2 Shuffling once

p2 <- ggplot(promotions_shuffled, aes(x = gender, fill = decision)) +
  geom_bar() +
  labs(x = "Gender of resume name")

library(gridExtra)

grid.arrange(p1, p2, ncol = 2)
promotions_shuffled %>% 
  group_by(gender, decision) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(gender) %>% 
  mutate(pct = n /sum(n))

## 9.1.3 Shuffling 16 times
## 9.1.4 What did we just do?

## bootstrap (with replacement), resampling (without replacement)

# 9.2 Understanding hypothesis tests
# 9.3 Conducting hypothesis tests

## 9.3.1 infer package workflow
promotions %>% 
  specify(formula = decision ~ gender, success = "promoted")

promotions %>% 
  specify(formula = decision ~ gender, success = "promoted") %>% 
  hypothesize(null = "independence")

promotions_generate <- promotions %>% 
  specify(formula = decision ~ gender, success = "promoted") %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute")

nrow(promotions_generate)

null_distribution <- promotions %>% 
  specify(formula = decision ~ gender, success = "promoted") %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in props", order = c("male", "female"))

null_distribution

obs_diff_prop <- promotions %>% 
  specify(decision ~ gender, success = "promoted") %>% 
  calculate(stat = "diff in props", order = c("male", "female"))

obs_diff_prop

visualize(null_distribution, bins = 10)
visualize(null_distribution, bins = 10) +
  shade_p_value(obs_stat = obs_diff_prop, direction = "right")

null_distribution %>% 
  get_p_value(obs_stat = obs_diff_prop, direction = "right")

## 9.3.2 Comparision with confidence intervals
# previous code
# null_distribution <- promotions %>% 
#   specify(formula = decision ~ gender, success = "promoted") %>% 
#   hypothesize(null = "independence") %>% 
#   generate(reps = 1000, type = "permute") %>% 
#   calculate(stat = "diff in props", order = c("male", "female"))

bootstrap_distribution <- promotions %>%
  specify(formula = decision ~ gender, success = "promoted") %>%
  # hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in props", order = c("male", "female"))

bootstrap_distribution
percentile_ci <- bootstrap_distribution %>% 
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci

visualize(bootstrap_distribution) +
  shade_confidence_interval(endpoints = percentile_ci)

se_ci <- bootstrap_distribution %>% 
  get_confidence_interval(level = 0.95, type = "se",
                          point_estimate = obs_diff_prop)
se_ci

visualize(bootstrap_distribution) +
  shade_confidence_interval(endpoints = se_ci)

# 9.4 Interpreting hypothesis tests
## 9.4.2 Types of errors

## 9.4.3 How do we choose alpha?

# 9.5 Case Study: Are action or romance movies rated higher?

## 9.5.1 IMDb ratings data
movies

movies_sample
ggplot(movies_sample, aes(x = genre, y = rating)) +
  geom_boxplot() +
  labs(y = "IMDb rating")

movies_sample %>% 
  group_by(genre) %>% 
  summarize(n = n(), mean_rating = mean(rating), std_dev = sd(rating))

## 9.5.2 Sampling scenario

# 9.5.3 Conducting the hypothesis test

movies_sample %>% 
  specify(formula = rating ~ genre)

movies_sample %>% 
  specify(formula = rating ~ genre) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  View()

null_distribution_movies <- movies_sample %>% 
  specify(formula = rating ~ genre) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("Action", "Romance"))

obs_diff_means <- movies_sample %>% 
  specify(formula = rating ~ genre) %>% 
  calculate(stat = "diff in means", order = c("Action", "Romance"))
obs_diff_means

visualize(null_distribution_movies, bins = 10) +
  shade_p_value(obs_stat = obs_diff_means, direction = "both")

null_distribution_movies %>% 
  get_p_value(obs_stat = obs_diff_means, direction = "both")

# 9.6 Conclusion
movies_sample %>% 
  group_by(genre) %>% 
  summarize(n = n(), mean_rating = mean(rating), std_dev = sd(rating))

# Construct null distribution of xbar_a - xbar_m:
null_distribution_movies <- movies_sample %>% 
  specify(formula = rating ~ genre) %>% 
  hypothesize(null = 'independence') %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("Action", "Romance"))
p1 <- visualize(null_distribution_movies, bins = 10)

null_distribution_movies_t <- movies_sample %>% 
  specify(formula = rating ~ genre) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  # Notice we switched stat from "diff in means" to "t"
  calculate(stat = "t", order = c("Action", "Romance"))

p2 <- visualize(null_distribution_movies_t, bins = 10)

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

visualize(null_distribution_movies_t, bins = 10, method = "both")

obs_two_sample_t <- movies_sample %>% 
  specify(formula = rating ~ genre) %>% 
  calculate(stat = "t", order = c("Action", "Romance"))

obs_two_sample_t
visualize(null_distribution_movies_t, method = "both") +
  shade_p_value(obs_stat = obs_two_sample_t, direction = "both")

null_distribution_movies_t %>% 
  get_p_value(obs_stat = obs_two_sample_t, direction = "both")

data("movies")
data("movies_sample")

## 9.6.2 When inference is not needed

flights_sample <- flights %>% 
  filter(carrier %in% c("HA", "AS"))

ggplot(flights_sample, aes(x = carrier, y = air_time)) +
  geom_boxplot() +
  labs(x = "Carrier", y = "Air Time")

flights_sample %>% 
  group_by(carrier, dest) %>% 
  summarize(n = n(), mean_time = mean(air_time, na.rm = TRUE))

## 9.6.3 Problems with p-values

## 9.6.5 What's to come
score_model <- lm(score ~ bty_avg, data = evals)
get_regression_table(score_model)

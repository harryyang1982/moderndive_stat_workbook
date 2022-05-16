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


library(tidyverse)
library(moderndive)
library(infer)

# 10.1 Regression refresher
## 10.1.1 Teaching evaluations analysis

evals_ch5 <- evals %>% 
  select(ID, score, bty_avg, age)
glimpse(evals_ch5)

ggplot(evals_ch5,
       aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "Beauty Score",
       y = "Teaching Score",
       title = "Relationship between teaching and beauty scores") +
  geom_smooth(method = "lm", se = FALSE)

# Fit regression model:
score_model <- lm(score ~ bty_avg, data = evals_ch5)
# Get regression table:
get_regression_table(score_model)

## 10.1.2 Sampling scenario

# 10.2 Interpreting regression tables
## 10.2.1 Standard error

## 10.2.2 Test statistic

## 10.2.3 p-value

# 10.3 Conditions for inference for regression

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
## 10.3.1 Residuals Refresher

# Fit regression model:
score_model <- lm(score ~ bty_avg, data = evals_ch5)
# Get regression points:
regression_points <- get_regression_points(score_model)
regression_points

## 10.3.2 Linearity of relationship
## 10.3.3 Independence of residuals

evals %>% 
  select(ID, prof_ID, score, bty_avg)

## 10.3.4 Normality of residuals

ggplot(regression_points, aes(x = residual)) +
  geom_histogram(binwidth = 0.25, color = "white") +
  labs(x = "Residual")

## 10.3.5
ggplot(regression_points, aes(x = bty_avg, y = residual)) +
  geom_point() +
  labs(x = "Beauty Score", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1)

# 10.4 Simulation-based inference for regression
## 10.4.1 Confidence interval for slope

bootstrap_distn_slope <- evals_ch5 %>% 
  specify(formula = score ~ bty_avg) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "slope")
bootstrap_distn_slope

visualize(bootstrap_distn_slope)

### Percentile-method
percentile_ci <- bootstrap_distn_slope %>% 
  get_confidence_interval(type = "percentile", level = 0.95)
percentile_ci

### Standard error method
observed_slope <- evals %>% 
  specify(score ~ bty_avg) %>% 
  calculate(stat = "slope")
observed_slope

se_ci <- bootstrap_distn_slope %>% 
  get_ci(level = 0.95, type = "se", point_estimate = observed_slope)
se_ci

### Comparing all three
visualize(bootstrap_distn_slope) +
  shade_confidence_interval(endpoints = percentile_ci, fill = NULL,
                            linetype = "solid", color = "grey90") +
  shade_confidence_interval(endpoints = se_ci, fill = NULL,
                            linetype = "dashed", color = "grey60") +
  shade_confidence_interval(endpoints = c(0.035, 0.099), fill = NULL,
                            linetype = "dotted", color = "black")

## 10.4.2 Hypothesis test for slope
null_distn_slope <- evals %>% 
  specify(score ~ bty_avg) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "slope")

visualize(null_distn_slope)
null_distn_slope %>% 
  get_p_value(obs_stat = observed_slope, direction = "both")

# 10.5 Conclusion
## 10.5.1 Theory-based inference for regression

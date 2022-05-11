library(tidyverse)
library(moderndive)
library(skimr)
library(ISLR)

# 6.1 One numerical and one categorical explanatory variable

# 6.1.1 Exploratory data analysis

evals_ch6 <- evals %>% 
  select(ID, score, age, gender)

evals_ch6

glimpse(evals_ch6)
skim(evals_ch6)

evals_ch6 %>% sample_n(size = 5)

evals_ch6 %>% select(score, age, gender) %>% skim()

evals_ch6 %>% 
  get_correlation(formula = score ~ age)

p1 <- ggplot(evals_ch6, aes(x = age, y = score, color = gender)) +
  geom_point() +
  labs(x = "Age", y = "Teaching Score", color = "Gender") +
  geom_smooth(method = "lm", se = FALSE)

# Fit regression model:
score_model_interaction <- lm(score ~ age * gender, data = evals_ch6)

# Get regression table:
get_regression_table(score_model_interaction)

# 6.1.3 Parallel slope model
p2 <- ggplot(evals_ch6, aes(x = age, y = score, color = gender)) +
  geom_point() +
  labs(x = "Age", 
       y = "Teaching Score",
       color = "Gender") +
  geom_parallel_slopes(se = FALSE)

# Fit regression model:
score_model_parallel_slopes <- lm(score ~ age + gender, data = evals_ch6)
# Get regression table:
get_regression_table(score_model_parallel_slopes)

library(gridExtra)
grid.arrange(p1, p2, nrow=1)

# 6.1.4 Observed/fitted values and residuals

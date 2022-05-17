library(tidyverse)
library(moderndive)
library(skimr)
library(ISLR)
library(grid)
library(gridExtra)

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
predict(score_model_interaction)
get_regression_points(score_model_interaction) %>% 
  filter(age == 59 & gender == "male")

regression_points <- get_regression_points(score_model_interaction)

get_regression_points(score_model_parallel_slopes)

# 6.2 Two numerical explanatory variables

credit_ch6 <- Credit %>% as_tibble() %>% 
  select(ID, debt = Balance, credit_limit = Limit,
         income = Income, credit_rating = Rating, age = Age)
credit_ch6

glimpse(credit_ch6)
credit_ch6 %>% sample_n(size = 5)
credit_ch6 %>% select(debt, credit_limit, income) %>% skim()

credit_ch6 %>% get_correlation(debt ~ credit_limit)
credit_ch6 %>% get_correlation(debt ~ income)

credit_ch6 %>% 
  select(debt, credit_limit, income) %>% 
  cor()

ggplot(credit_ch6, aes(x = credit_limit, y = debt)) +
  geom_point() +
  labs(x = "Credit limit (in $)", y = "Credit card debt (in $)",
       title = "Debt and credit limit") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(credit_ch6, aes(x = income, y = debt)) +
  geom_point() +
  labs(x = "Income (in $1000)", y = "Credit card debt (in $)",
       title = "Debt and income") +
  geom_smooth(method = "lm", se = FALSE)

credit_ch6 %>% 
  select(debt, credit_rating, age) %>% 
  cor()

# 6.2.2 Regression plane
# Fit regression model:
debt_model <- lm(debt ~ credit_limit + income, data = credit_ch6)
# Get regression table:
get_regression_table(debt_model)

debt_model2 <- lm(debt ~ credit_rating + age, data = credit_ch6)
get_regression_table(debt_model2)

# 6.2.3 Observed/fitted values and residuals
get_regression_points(debt_model)

# 6.3 Related Topics
# 6.3.1 Model selection

# Interaction model
g1 <- ggplot(MA_schools, aes(x = perc_disadvan,
                       y = average_sat_math,
                       color = size)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Percent economically disadvantaged",
       y = "Math SAT Score",
       color = "School size",
       title = "Interaction model")

# Parallel slopes model
g2 <- ggplot(MA_schools, aes(x = perc_disadvan,
                       y = average_sat_math,
                       color = size)) +
  geom_point(alpha = 0.25) +
  geom_parallel_slopes(se = FALSE) +
  labs(x = "Percent economically disadvantaged",
       y = "Math SAT Score",
       color = "School size",
       title = "Parallel slopes model")

grid.arrange(g1, g2, ncol = 2)

model_2_interaction <- lm(average_sat_math ~ perc_disadvan * size,
                          data = MA_schools)
get_regression_table(model_2_interaction)

model_2_parallel_slopes <- lm(average_sat_math ~ perc_disadvan + size, 
                              data = MA_schools)
get_regression_table(model_2_parallel_slopes)

# 6.3.2 Correlation coefficient
credit_ch6 %>% select(debt, income) %>% 
  mutate(income = income * 1000) %>% 
  cor()
# as same as not multiple 1000

# 6.3.3 Simple's Paradox
stats <- boxplot(credit_ch6$credit_limit)$stats

ggplot(credit_ch6, aes(x = credit_limit)) +
  geom_histogram(color = "white") +
  geom_vline(xintercept = stats[2]) +
  geom_vline(xintercept = stats[3]) +
  geom_vline(xintercept = stats[4])

ggplot(credit_ch6, aes(x = income,
                       y = debt,
                       color = credit_limit_fac)) +
  geom_point()

c2 <- credit_ch6 %>% 
  mutate(credit_limit_bracket = case_when(credit_limit >= stats[4] ~ "high",
                                          credit_limit >= stats[3] ~ "medium-high",
                                          credit_limit >= stats[2] ~ "medium-low",
                                          TRUE ~ "low"),
         credit_limit_bracket = parse_factor(credit_limit_bracket, levels = c("low", "medium-low", "medium-high", "high"))) %>% 
  ggplot(aes(x = income,
             y = debt,
             color = credit_limit_bracket)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Income (in $1000)")

c1 <- ggplot(credit_ch6, aes(x = income,
             y = debt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Income (in $1000)",
       y = "Credit card debt(in $)")

grid.arrange(c1, c2, top = textGrob("Two scatterplots of credit card debt vs income"),
             ncol = 2)

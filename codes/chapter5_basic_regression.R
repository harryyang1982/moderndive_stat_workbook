library(tidyverse)
library(moderndive)
library(skimr)
library(gapminder)

# 5.1 One Numerical explanatory variable
# 5.1.1 EDA
evals_ch5 <- evals %>% 
  select(ID, score, bty_avg, age)

glimpse(evals_ch5)
evals_ch5 %>% 
  sample_n(5)

evals_ch5 %>% 
  summarize(mean_bty_avg = mean(bty_avg),
            mean_score = mean(score),
            median_bty_avg = median(bty_avg),
            median_score = median(score))

evals_ch5 %>% 
  select(score, bty_avg) %>% 
  skim()

evals_ch5 %>% 
  get_correlation(formula = score ~ bty_avg)

evals_ch5 %>% 
  cor()

evals_ch5 %>% 
  summarize(correlation = cor(score, bty_avg))

ggplot(evals_ch5, aes(x = bty_avg,
                      y = score)) +
  geom_point() +
  labs(x = "Beauty Score",
       y = "Teaching Score",
       title = "Scatterplot of relationship of teaching and beauty scores")

ggplot(evals_ch5, aes(x = bty_avg,
                      y = score)) +
  geom_jitter() +
  labs(x = "Beauty Score",
       y = "Teaching Score",
       title = "Scatterplot of relationship of teaching and beauty scores")

ggplot(evals_ch5, aes(x = bty_avg,
                      y = score)) +
  geom_point() +
  labs(x = "Beauty Score",
       y = "Teaching Score",
       title = "Relationship of relationship of teaching and beauty scores") +
  geom_smooth(method = "lm", se = FALSE)

# Fit regression model
score_model <- lm(score ~ bty_avg, data = evals_ch5)

# Get regression table
get_regression_table(score_model)
# summary(score_model)

score_model2 <- lm(score ~ age, data = evals_ch5)
get_regression_table(score_model2)

ggplot(evals_ch5) +
  geom_point(aes(x = age, y = score))

ggplot(evals_ch5) +
  geom_jitter(aes(x = age, y = score))

ggplot(evals_ch5, aes(x = age, y = score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# 5.1.3 Observed / fitted values and residuals

evals_ch5[evals_ch5$ID == 21, ]
regression_points <- get_regression_points(score_model)

regression_points

regression_points2 <- get_regression_points(score_model2)
regression_points2

# 5.2 One categorical explanatory variable

## 5.2.1 Exploratory data analysis
library(gapminder)
gapminder2007 <- gapminder %>% 
  filter(year == 2007) %>% 
  select(country, lifeExp, continent, gdpPercap)

gapminder2007
glimpse(gapminder2007)
gapminder2007 %>% sample_n(size=5)

gapminder2007 %>% 
  select(lifeExp, continent) %>% 
  skim()

ggplot(gapminder2007, aes(x = lifeExp)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Life expectancy", y = "Number of countries",
       title = "Histogram of distribution of worldwide life expectancies")

ggplot(gapminder2007, aes(x = lifeExp)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Life expectancy",
       y = "Number of countries",
       title = "Histogram of distribution of worldwide life expectancies") +
  facet_wrap(~continent, nrow = 2)

ggplot(gapminder2007, aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy",
       title = "Life expectancy by continent")

# ggplot(gapminder2007, aes(x = continent, y = lifeExp)) +
#   geom_boxplot() +
#   stat_summary(fun = "mean") +
#   labs(x = "Continent", y = "Life expectancy",
#        title = "Life expectancy by continent")

lifeExp_by_continent <- gapminder2007 %>% 
  group_by(continent) %>% 
  summarize(median = median(lifeExp),
            mean = mean(lifeExp))
lifeExp_by_continent

gapminder2007
gdpPercap_by_continent <- gapminder2007 %>% 
  group_by(continent) %>% 
  summarize(median = median(gdpPercap),
            mean = mean(gdpPercap))
gdpPercap_by_continent

lifeExp_model <- lm(lifeExp ~ continent, data = gapminder2007)
get_regression_table(lifeExp_model)

gdpPer_model <- lm(gdpPercap ~ continent, data = gapminder2007)
get_regression_table(gdpPer_model)

regression_points <- get_regression_points(lifeExp_model, ID = "country")
regression_points %>% 
  group_by(continent) %>% 
  slice_min(residual, n = 5) -> worst_5

regression_points %>% 
  group_by(continent) %>% 
  slice_min(lifeExp, n = 5)

regression_points %>% 
  group_by(continent) %>% 
  slice_max(residual, n = 5)

# Fit regression model
score_model <- lm(score ~ bty_avg,
                  data = evals_ch5)
regression_points <- get_regression_points(score_model)
regression_points
# Compute sum of squared residuals
regression_points %>% 
  mutate(squared_residuals = residual^2) %>% 
  summarize(sum_of_squared_residuals = sum(squared_residuals))

get_regression_table(score_model)

# wrapper functions
# tidy (from broom package)
# clean_names (from janitor package)

library(broom)
library(janitor)

score_model %>% 
  tidy(conf.int = TRUE) %>% 
  mutate(across(where(is.numeric), ~round(., 3))) %>% 
  clean_names() %>% 
  rename(lower_ci = conf_low, upper_ci = conf_high)

score_model %>% 
  augment() %>% 
  mutate(across(where(is.numeric), ~round(., 3))) %>% 
  clean_names() %>% 
  select(-c("hat", "sigma", "cooksd", "std_resid"))

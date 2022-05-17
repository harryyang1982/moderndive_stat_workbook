library(tidyverse)
library(moderndive)
library(infer)
library(skimr)
library(fivethirtyeight)
library(nycflights13)

# 11. 1 Review

# 11.2 Case study: Seattle house prices

View(house_prices)
glimpse(house_prices)

house_prices %>% 
  select(price, sqft_living, condition) %>% 
  skim()

p1 <- ggplot(house_prices, aes(x = price)) +
  geom_histogram(color = "white") +
  labs(x = "price (USD)", title = "House price")

p2 <- ggplot(house_prices, aes(x = sqft_living)) +
  geom_histogram(color = "white") +
  labs(x = "living space (square feet)", title = "House size")

p3 <- ggplot(house_prices, aes(x = condition)) +
  geom_bar() +
  labs(x = "condition", title = "House condition")

library(gridExtra)

grid.arrange(p1, p2, p3, ncol = 2)

house_prices <- house_prices %>% 
  mutate(log10_price = log10(price),
         log10_size = log10(sqft_living))

house_prices %>% 
  select(price, log10_price, sqft_living, log10_size)

# Before log10 transformation:
ggplot(house_prices, aes(x = price)) +
  geom_histogram(color = "white") +
  labs(x = "price (USD)", title = "House price: Before transformation")

# After log10 transformation:
ggplot(house_prices, aes(x = log10_price)) +
  geom_histogram(color = "white") +
  labs(x = "log10 price (USD)", title = "House price: After transformation")

# Before log10 transformation:
ggplot(house_prices, aes(x = sqft_living)) +
  geom_histogram(color = "white") +
  labs(x = "living space (square feet)", title = "House size: Before")

ggplot(house_prices, aes(x = log10_size)) +
  geom_histogram(color = "white") +
  labs(x = "log10 living space (square feet)", title = "House size: After")

## 11.2.2 Exploratory data analysis: Part II

## Plot interaction model
g1 <- ggplot(house_prices, aes(x = log10_size, y = log10_price, color = condition)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(y = "log10 price",
       x = "log10 size",
       title = "House prices in Seattle")

# Plot parallel slopes model
g2 <- ggplot(house_prices, aes(x = log10_size, y = log10_price, color = condition)) +
  geom_point(alpha = 0.05) +
  geom_parallel_slopes(se = FALSE) +
  labs(y = "log10 price",
       x = "log10 size",
       title = "House prices in Seattle")

grid.arrange(g1, g2, ncol = 2)

ggplot(house_prices, 
       aes(x = log10_size, y = log10_price, color = condition)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(y = "log10 price",
       x = "log10 size",
       title = "House prices in Seattle") +
  facet_wrap(~condition)

## 11.2.3 Regression modelling
# Fit regression model:
price_interaction <- lm(log10_price ~ log10_size * condition,
                        data = house_prices)

# Get regression table:
get_regression_table(price_interaction)

## 11.2.4 Making predictions
2.45 + 1 * log10(1900)
10^(2.45 + 1 * log10(1900))

# 11.3 Case study: Effective data storytelling

## 11.3.1 Bechdel test for Hollywood gender representation
## 11.3.2 US Births in 1999
data("US_births_1994_2003")
glimpse(US_births_1994_2003)

US_births_1999 <- US_births_1994_2003 %>% 
  filter(year == 1999)

ggplot(US_births_1999, aes(x = date, y = births)) +
  geom_line() +
  labs(x = "Date",
       y = "Number of births",
       title = "US Births in 1999")
US_births_1999 %>% 
  arrange(desc(births))

US_births_1994_2003 %>% 
  arrange(births)

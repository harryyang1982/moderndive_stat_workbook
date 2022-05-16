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

ggplot(promotions, aes(x = gender, fill = decision)) +
  geom_bar() +
  labs(x = "Gender of name on resume")

promotions %>% 
  group_by(gender, decision) %>% 
  count() # as same as tally ()


promotions %>% 
  group_by(gender, decision) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(gender) %>% 
  mutate(pct = n / sum(n))

## 9.1.2 Shuffling once

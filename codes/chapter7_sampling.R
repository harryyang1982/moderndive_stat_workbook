library(tidyverse)
library(moderndive)

# 7.1 Sampling bowl activity
# 7.1.2 Using the shovel once
data(tactile_prop_red)
tactile_prop_red

ggplot(tactile_prop_red, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red",
       title = "Distribution of 33 proportions red")
# 7.1.4 What did we do?

# 7.2 Virtual sampling
# 7.2.1 Using the virtual shovel once

bowl
virtual_shovel <- bowl %>% 
  rep_sample_n(size = 50)
virtual_shovel

virtual_shovel %>% 
  mutate(is_red = (color == "red")) %>% 
  summarize(num_red = sum(is_red))

virtual_shovel %>% 
  mutate(is_red = (color == "red")) %>% 
  summarize(num_red = mean(is_red))

virtual_shovel %>% 
  summarize(num_red = mean(color == "red"))

n_sim <- 33
s_dist <- vector(length=n_sim)
for (i in 1:n_sim) {
  s_dist[i] <- bowl %>% rep_sample_n(size=50) %>% 
    summarize(num_red = mean(color == "red")) %>% .[['num_red']]
}

mean(s_dist)

# 7.2.2 Using the virtual shovel 33 times
virtual_samples <- bowl %>% 
  rep_sample_n(size = 50, reps = 33)
virtual_samples

s_dist <- bowl %>% 
    rep_sample_n(size = 50, reps = 500000) %>% 
    group_by(factor(replicate)) %>% 
    summarize(num_red = mean(color == "red")) %>% .[['num_red']]
mean(s_dist)

virtual_prop_red <- virtual_samples %>% 
  group_by(replicate) %>% 
  summarize(red = sum(color == 'red'),
            prop_red = mean(color == 'red'))

virtual_prop_red

ggplot(virtual_prop_red, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red",
       title = "Distribution of 33 proportions red")

# 7.2.3 Using the virtual shovel 1000 times
virtual_samples <- bowl %>% 
  rep_sample_n(size = 50, reps = 1000)
virtual_samples
virtual_prop_red <- virtual_samples %>% 
  group_by(replicate) %>% 
  summarize(red = sum(color == "red"),
            prop_red = mean(color == "red"))
virtual_prop_red
mean(virtual_prop_red$prop_red)

ggplot(virtual_prop_red, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red",
       title = "Distribution of 1000 proportions red")

# 7.2.4 Using different shovels
